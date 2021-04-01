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
(require 'init-const)
(require 'init-custom)

;; Personal information
(setq user-full-name dotfairy-full-name
      user-mail-address dotfairy-mail-address)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8-unix)            ; pretty
(setq locale-coding-system 'utf-8)

(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(when (eq system-type 'windows-nt)
  (set-language-environment 'chinese-gb18030)
  (prefer-coding-system 'utf-8-unix)
  (setq locale-coding-system 'gb18030)
  (setq file-name-coding-system 'gb18030)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

;; The clipboard's on Windows could be in a wider (or thinner) encoding than
;; utf-8 (likely UTF-16), so let Emacs/the OS decide what encoding to use there.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

;;
;;; Clipboard / kill-ring
;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-ring-max 1024)
(setq kill-do-not-save-duplicates t)

;; undo limit
(setq undo-outer-limit 5000000)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Misc
(set-default 'cursor-type 'bar)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq-default fill-column 120)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; Start server
(use-package server
  :ensure nil
  :if dotfairy-server
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file (concat dotfairy-cache-dir "saveplace")
        save-place-limit 100)
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-file (expand-file-name ".savehist" dotfairy-cache-dir)
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(when (>= emacs-major-version 27)
  (use-package so-long
    :ensure nil
    :hook (after-init . global-so-long-mode)
    :config (setq so-long-threshold 400)))

;; Highlight the current line
(use-package hl-line
  :ensure t
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode html-mode css-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; File and buffer
(defun dotfairy/revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

;; Reload configurations
(defun dotfairy/reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file)
  (funcall major-mode))


(defun dotfairy/open-init-file()
  (interactive)
  (find-file user-init-file))
(provide 'init-basic)
;;; init-basic.el ends here
