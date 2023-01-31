;;; init.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020

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

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Speed up startup
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (put 'file-name-handler-alist 'initial-value old-value)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101))

  ;; Prevent flash of unstyled modeline at startup
  (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf (setq mode-line-format nil)))

  ;; Prevent flash of messages at startup
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redraw-frame)))

  (define-advice startup--load-user-iFnit-file (:after (&rest _) undo-inhibit-vars)
    (when init-file-had-error
      (setq-default inhibit-redisplay nil
                    inhibit-message nil)
      (redraw-frame))
    (unless (default-toplevel-value 'mode-line-format)
      (setq-default mode-line-format (get 'mode-line-format 'initial-value)))))

;; (setq user-emacs-directory (expand-file-name "lisp" load-file-name))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defun custom-config-load-path (&rest _)
  "Load lisp path"
  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'custom-config-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)
(custom-config-load-path)

;; Load the heart of dotfairy
(require 'init-startup)

(require 'custom-keybinds)
;;; init.el ends here
