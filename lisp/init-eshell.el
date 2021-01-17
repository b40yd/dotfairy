;;; init-eshell.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  7ym0n.q6e

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
;; Emacs command shell
(use-package eshell
  :ensure nil
  :defines eshell-prompt-function
  :functions eshell/alias
  :hook (eshell-mode . (lambda ()
                         (bind-key "C-l" 'eshell/clear eshell-mode-map)
                         ;; Aliases
                         (eshell/alias "f" "find-file $1")
                         (eshell/alias "fo" "find-file-other-window $1")
                         (eshell/alias "d" "dired $1")
                         (eshell/alias "l" "ls -lFh")
                         (eshell/alias "ll" "ls -l")
                         (eshell/alias "la" "ls -lAFh")
                         (eshell/alias "lr" "ls -tRFh")
                         (eshell/alias "lrt" "ls -lFcrt")
                         (eshell/alias "lsa" "ls -lah")
                         (eshell/alias "lt" "ls -ltFh")))
  :config
  (with-no-warnings
    ;; For compatibility
    (unless (fboundp 'flatten-tree)
      (defalias 'flatten-tree #'eshell-flatten-list))

    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))

    (defun eshell/emacs (&rest args)
      "Open a file (ARGS) in Emacs.  Some habits die hard."
      (if (null args)
          ;; If I just ran "emacs", I probably expect to be launching
          ;; Emacs, which is rather silly since I'm already in Emacs.
          ;; So just pretend to do what I ask.
          (bury-buffer)
        ;; We have to expand the file names or else naming a directory in an
        ;; argument causes later arguments to be looked for in that directory,
        ;; not the starting directory
        (mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))
    (defalias 'eshell/e #'eshell/emacs)
    (defalias 'eshell/ec #'eshell/emacs)

    (defun eshell/ebc (&rest args)
      "Compile a file (ARGS) in Emacs. Use `compile' to do background make."
      (if (eshell-interactive-output-p)
          (let ((compilation-process-setup-function
                 (list 'lambda nil
                       (list 'setq 'process-environment
                             (list 'quote (eshell-copy-environment))))))
            (compile (eshell-flatten-and-stringify args))
            (pop-to-buffer compilation-last-buffer))
        (throw 'eshell-replace-command
               (let ((l (eshell-stringify-list (flatten-tree args))))
                 (eshell-parse-command (car l) (cdr l))))))
    (put 'eshell/ebc 'eshell-no-numeric-conversions t)

    (defun eshell-view-file (file)
      "View FILE.  A version of `view-file' which properly rets the eshell prompt."
      (interactive "fView file: ")
      (unless (file-exists-p file) (error "%s does not exist" file))
      (let ((buffer (find-file-noselect file)))
        (if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class)
                'special)
            (progn
              (switch-to-buffer buffer)
              (message "Not using View mode because the major mode is special"))
          (let ((undo-window (list (window-buffer) (window-start)
                                   (+ (window-point)
                                      (length (funcall eshell-prompt-function))))))
            (switch-to-buffer buffer)
            (view-mode-enter (cons (selected-window) (cons nil undo-window))
                             'kill-buffer)))))

    (defun eshell/less (&rest args)
      "Invoke `view-file' on a file (ARGS).
\"less +42 foo\" will go to line 42 in the buffer for foo."
      (while args
        (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
            (let* ((line (string-to-number (match-string 1 (pop args))))
                   (file (pop args)))
              (eshell-view-file file)
              (forward-line line))
          (eshell-view-file (pop args)))))
    (defalias 'eshell/more #'eshell/less))

  ;;  Display extra information for prompt
  (use-package eshell-prompt-extras
    :after esh-opt
    :defines eshell-highlight-prompt
    :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
    :init (setq eshell-highlight-prompt nil
                eshell-prompt-function #'epe-theme-lambda))

  ;; Fish-like history autosuggestions
  (use-package esh-autosuggest
    :defines ivy-display-functions-alist
    :bind (:map eshell-mode-map
                ([remap eshell-pcomplete] . completion-at-point))
    :hook ((eshell-mode . esh-autosuggest-mode)
           (eshell-mode . eshell-setup-ivy-completion))
    :init (defun eshell-setup-ivy-completion ()
            "Setup `ivy' completion in `eshell'."
            (setq-local ivy-display-functions-alist
                        (remq (assoc 'ivy-completion-in-region
                                     ivy-display-functions-alist)
                              ivy-display-functions-alist))))

  ;; `eldoc' support
  (use-package esh-help
    :init (setup-esh-help-eldoc))

  ;; `cd' to frequent directory in `eshell'
  (use-package eshell-z
    :hook (eshell-mode . (lambda () (require 'eshell-z)))))

(use-package vterm
  :if (executable-find "cmake")
  :bind (:map vterm-mode-map
              ("C-s" . counsel-grep-or-swiper)
              ("C-y" . vterm-yank))
  :bind ("C-c t m" . multi-term-hydra/body)
  :config

  ;; disable some unnecessary minor-modes in term-mode
  (add-hook 'vterm-mode-hook (lambda ()
                               (yas-minor-mode -1)
                               (setq-local global-hl-line-mode nil)

                               ;; Prevent premature horizontal scrolling
                               (setq-local hscroll-margin 0)))

  ;; vterm buffers are killed when the associated process is terminated
  (setq vterm-kill-buffer-on-exit t))

;; vterm-toggle: toggles between the vterm buffer and whatever buffer you are editing.
;; https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle
  :if (executable-find "cmake")
  :after vterm)

;; multi-vterm: manage multiple terminal windows easily within emacs
;; https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :after vterm
  :if (executable-find "cmake")
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

(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output t)

;; https://gitlab.com/jgkamat/rmsbolt
;; rmsbolt:A godbolt embedded in Emacs
(use-package rmsbolt
  :defer t)

;; quickrun - Execute editing buffer and show its output quickly.
;; https://github.com/syohex/emacs-quickrun
(use-package quickrun
  :config
  ;; hydra for quickrun
  (bind-key "C-c h q"
            (defhydra hydra-quickrun (:color teal
                                             :hint nil)
              "
_u_: compile + run     _c_: compile file                _s_: execute buffer in eshell
_r_: execute region    _e_: execute + replace region    _a_: execute with args
_q_: quit
"
              ("u" quickrun)
              ("r" quickrun-region)
              ("e" quickrun-replace-region)
              ("c" quickrun-compile-only)
              ("a" quickrun-with-arg)
              ("s" quickrun-shell)
              ("q" nil "Quit" :color blue))))

(provide 'init-eshell)
;;; init-eshell.el ends here
