;;; init-vterm.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  7ym0n.q6e

;; Author: 7ym0n.q6e <bb.qnyd@gmail.com>
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

;;; Code:

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
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :init (setq vterm-always-compile-module t)
    ;; disable some unnecessary minor-modes in term-mode
    :hook (vterm-mode . (lambda ()
                          (yas-minor-mode -1)
                          (setq-local global-hl-line-mode nil)

                          ;; Prevent premature horizontal scrolling
                          (setq-local hscroll-margin 0)))
    :config
    ;; vterm buffers are killed when the associated process is terminated
    (setq vterm-kill-buffer-on-exit t)

    ;; vterm-toggle: toggles between the vterm buffer and whatever buffer you are editing.
    ;; https://github.com/jixiuf/vterm-toggle
    (use-package vterm-toggle
      :after vterm)

    ;; multi-vterm: manage multiple terminal windows easily within emacs
    ;; https://github.com/suonlight/multi-vterm
    (use-package multi-vterm
      :after vterm
      :config
      ;; hydra for using multi-vterm
      (defhydra multi-term-hydra ()
        "multi-term"
        ("o" multi-vterm "new terminal")
        ("t" vterm-toggle-cd "toggle/open")
        ("n" multi-vterm-next "Next")
        ("p" multi-vterm-prev "Prev")
        ("d" multi-vterm-dedicated-toggle "Dedicated terminal")
        ("r" multi-vterm-projectile "vterm projectile")
        ("q" nil "Quit" :color blue)))

    (map! :map vterm-mode-map
          :localleader
          "y" #'vterm-yank
          "m" #'multi-term-hydra/body
          ))
  )

;; Shell Pop
(use-package shell-pop
  :bind ([f9] . shell-pop)
  :init (setq shell-pop-window-size 30
              shell-pop-shell-type
              (cond ((fboundp 'vterm) '("vterm" "*vterm*" #'vterm))
                    (IS-WINDOWS '("eshell" "*eshell*" #'eshell))
                    (t '("terminal" "*terminal*"
                         (lambda () (term shell-pop-term-shell)))))))

(provide 'init-vterm)
;;; init-vterm.el ends here
