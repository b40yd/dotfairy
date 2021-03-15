;;; init-basic.el ---                                -*- lexical-binding: t; -*-

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


;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
;; !!! windows is not work.
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Don't autosave files or create lock/history/backup files. We don't want
;; copies of potentially sensitive material floating around or polluting our
;; filesystem. We rely on git and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      ;; auto-save-list-file-name (concat doom-cache-dir "autosave")
      auto-save-list-file-prefix (concat dotfairy-cache-dir "autosave/")
      auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
      backup-directory-alist `((".*" . ,(concat dotfairy-cache-dir "backup/"))))

;;
;;; Formatting

;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like go-mode).
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant.
(setq require-final-newline t)

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)

;;
;;; Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-ring-max 1024)
(setq kill-do-not-save-duplicates t)

;; undo limit
(setq undo-outer-limit 5000000)

;; Allow UTF or composed text from the clipboard, even in the terminal or on
;; non-X systems (like Windows or macOS), where only `STRING' is used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;; Display available keybindings in popup
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package recentf
  :ensure nil
  :bind (("C-c C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))


;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
;; (when (display-graphic-p)
;;   (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;         mouse-wheel-progressive-speed nil))
;; (setq scroll-step 1
;;       scroll-margin 0
;;       scroll-conservatively 100000)

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(global-set-key (kbd "C-c C-l") #'reload-init-file)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c f e d") 'open-init-file)

(defcustom display-icon (display-graphic-p)
  "Display icons or not."
  :group 'dotfairy
  :type 'boolean)

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and display-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

;; Highlight the current line
(use-package hl-line
  :ensure t
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))


(provide 'init-basic)
;;; init-basic.el ends here
