;;; init-vterm.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 b40yd

;; Author: b40yd <b40yd@scanbuf.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;
(require 'init-const)
(require 'init-custom)
;;; Code:

(use-package shell
  :ensure nil
  :hook ((shell-mode . my-shell-mode-hook)
         (comint-output-filter-functions . comint-strip-ctrl-m))
  :init
  (setq system-uses-terminfo nil)

  (with-no-warnings
    (defun my-shell-simple-send (proc command)
      "Various PROC COMMANDs pre-processing before sending to shell."
      (cond
       ;; Checking for clear command and execute it.
       ((string-match "^[ \t]*clear[ \t]*$" command)
        (comint-send-string proc "\n")
        (erase-buffer))
       ;; Checking for man command and execute it.
       ((string-match "^[ \t]*man[ \t]*" command)
        (comint-send-string proc "\n")
        (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
        (setq command (replace-regexp-in-string "[ \t]+$" "" command))
        ;;(message (format "command %s command" command))
        (funcall 'man command))
       ;; Send other commands to the default handler.
       (t (comint-simple-send proc command))))

    (defun my-shell-mode-hook ()
      "Shell mode customization."
      (local-set-key '[up] 'comint-previous-input)
      (local-set-key '[down] 'comint-next-input)
      (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)

      (ansi-color-for-comint-mode-on)
      (setq comint-input-sender 'my-shell-simple-send))))

;; ANSI & XTERM 256 color support
(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :functions (compilation-filter my-advice-compilation-filter)
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled
              (make-local-variable 'font-lock-function)
              (setq font-lock-function #'ignore)))

  ;; For eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc
             (if (eq major-mode 'rg-mode) ; compatible with `rg'
                 string
               (xterm-color-filter string))))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
  (advice-add 'gud-filter :around #'my-advice-compilation-filter))

;; Better term
;; @see https://github.com/akermu/emacs-libvterm#installation
(unless IS-WINDOWS
  (when (and module-file-suffix           ; dynamic module
             (executable-find "cmake")
             (executable-find "libtool")
             (executable-find "make"))
    (use-package vterm
      :init
      :init (setq vterm-always-compile-module t)
      :preface
      ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
      ;;      the package is loaded, this is necessary to prevent it when
      ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
      ;;      when compiled).
      (when noninteractive
        (advice-add #'vterm-module-compile :override #'ignore)
        (provide 'vterm-module))
      :config
      (map! :map vterm-mode-map "C-q" #'vterm-send-next-key)
      ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
      ;; spawn another if want one.
      (setq vterm-kill-buffer-on-exit t)

      ;; 5000 lines of scrollback, instead of 1000
      (setq vterm-max-scrollback 5000))

    (use-package multi-vterm
      :bind ("C-<f9>" . multi-vterm)
      :init (setq multi-vterm-buffer-name "vterm")
      :config
      (with-no-warnings
        ;; Use `pop-to-buffer' instead of `switch-to-buffer'
        (defun my-multi-vterm ()
          "Create new vterm buffer."
          (interactive)
          (let ((vterm-buffer (multi-vterm-get-buffer)))
            (setq multi-vterm-buffer-list
                  (nconc multi-vterm-buffer-list (list vterm-buffer)))
            (set-buffer vterm-buffer)
            (multi-vterm-internal)
            (pop-to-buffer vterm-buffer)))
        (advice-add #'multi-vterm :override #'my-multi-vterm)))))

;; Powershell
(use-package powershell
  :init
  (defun powershell (&optional buffer)
    "Launches a powershell in buffer *powershell* and switches to it."
    (interactive)
    (let ((buffer (or buffer "*powershell*"))
          (program (if (executable-find "pwsh") "pwsh" "powershell")))
      (make-comint-in-buffer "Powershell" buffer program nil "-NoProfile")
      (with-current-buffer buffer
        (setq-local mode-line-format nil)
        (and (bound-and-true-p corfu-mode) (corfu-mode -1)))
      (pop-to-buffer buffer))))

;; Better terminal emulator
(unless IS-WINDOWS
  (use-package ghostel
    :hook (eshell-load . ghostel-eshell-visual-command-mode)))

;; Shell Pop
(use-package popterm
    :functions childframe-workable-p
    :bind (("C-`"   . popterm-toggle)
           ("C-~"   . popterm-toggle-cd)
           ([f9]    . popterm-window-toggle))
    :hook (after-init . popterm-global-mode)
    :init
    (setq popterm-backend (if IS-WINDOWS 'eshell 'ghostel)
          popterm-display-method (if (childframe-workable-p)
                                     'posframe
                                   'window)
          popterm-scope 'project)
    :config
    (with-no-warnings
      (defun popterm--reset-cursor-point (buffer)
        "Reset cursor point."
        (with-current-buffer buffer
          (when (derived-mode-p 'ghostel-mode)
            (ghostel-send-key "down"))))
      (advice-add #'popterm--posframe-show :after #'popterm--reset-cursor-point)))

(provide 'init-vterm)
;;; init-vterm.el ends here
