;;; init-custom.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n.q6e, all rights reserved.

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
(defgroup dotfairy nil
  "Dotfairy Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/7ym0n/dotfairy"))

(defcustom dotfairy-logo (expand-file-name
                          (if (display-graphic-p) "logo.png" "banner.txt")
                          user-emacs-directory)
  "Set Dotfairy logo. nil means official logo."
  :group 'dotfairy
  :type 'string)

(defcustom dotfairy-full-name ""
  "Set user full name."
  :group 'dotfairy
  :type 'string)

(defcustom dotfairy-mail-address ""
  "Set user email address."
  :group 'dotfairy
  :type 'string)

(defcustom dotfairy-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'dotfairy
  :type 'boolean)

(defcustom dotfairy-proxy "127.0.0.1:1087"
  "Set network proxy."
  :group 'dotfairy
  :type 'string)

(defcustom dotfairy-theme-alist
  '((default . doom-one)
    (doom-one . doom-one)
    (doom-monokai-pro     . doom-monokai-pro)
    (doom-dark+    . doom-dark+)
    (doom-one-light   . doom-one-light)
    (doom-solarized-light    . doom-solarized-light)
    (doom-city-lights    . doom-city-lights)
    (doom-tomorrow-day    . doom-tomorrow-day)
    (doom-tomorrow-night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'dotfairy
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom dotfairy-theme 'default
  "The color theme."
  :group 'dotfairy
  :type 'symbol)

(defcustom dotfairy-lsp-format-on-save-ignore-modes '(c-mode c++-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'dotfairy
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom dotfairy-server nil
  "Enable `server-mode' or not."
  :group 'dotfairy
  :type 'boolean)

(defcustom dotfairy-company-prescient t
  "Enable `company-prescient' or not. it's on Windows 10 very slow."
  :group 'dotfairy
  :type 'boolean)

(defcustom dotfairy-quelpa-upgrade nil
  "Enable `quelpa-upgrade-p' or not. it will try upgrade packages."
  :group 'dotfairy
  :type 'boolean)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom dotfairy-package-archives-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    `(,(cons 'melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
      ,(cons 'emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
      ,(cons 'netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
      ,(cons 'ustc
             `(,(cons "gnu"   (concat proto "://mirrors.ustc.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.ustc.edu.cn/elpa/melpa/"))))
      ,(cons 'tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))
  "The package archives group list."
  :group 'dotfairy
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom dotfairy-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'dotfairy
  :set (lambda (symbol value)
         (set symbol value)
         ()
         (setq package-archives
               (or (alist-get value dotfairy-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    dotfairy-package-archives-alist)))

(defcustom display-icon (display-graphic-p)
  "Display icons or not."
  :group 'dotfairy
  :type 'boolean)

(defcustom dotfairy-completion-style 'childframe
  "Completion display style."
  :group 'dotfairy
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom dotfairy-lsp 'lsp-mode
  "Set language server.
`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
tags: Use tags file instead of language server. See https://github.com/universal-ctags/citre.
nil means disabled."
  :group 'dotfairy
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom dotfairy-prettify-symbols-alist
  '(("<-" . ?←)
    ("->" . ?→)
    ("->>" . ?↠)
    ("=>" . ?⇒)
    ("/=" . ?≄)
    ("!=" . ?≠)
    ("==" . ?≡)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("=<<" . (?= (Br . Bl) ?≪))
    (">>=" . (?≫ (Br . Bl) ?=))
    ("<=<" . ?↢)
    (">=>" . ?↣)
    ("&&" . ?∧)
    ("||" . ?∨)
    (">>" . ?≫)
    ("<<" . ?≪)
    ("return" . ?⟼)
    ("defun" . ?ƒ)
    ("define" . ?ƒ)
    ("bool" . ?𝔹)
    ("false" . ?𝔽)
    ("true" . ?𝕋)
    ("for" . ?∀))
  "Alist of symbol prettifications.
Nil to use font supports ligatures."
  :group 'dotfairy
  :type '(alist :key-type string :value-type (choice character sexp)))

(provide 'init-custom)
;;; init-custom.el ends here
