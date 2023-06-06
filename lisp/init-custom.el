;;; init-custom.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2020-2024 b40yd

;; Author: b40yd <bb.qnyd@gmail.com>
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
  '((default . solarized-zenburn)
    (doom-solarized-dark . doom-solarized-dark)
    (doom-one . doom-one)
    (doom-monokai-pro     . doom-monokai-pro)
    (doom-dark . doom-vibrant)
    (doom-cold . doom-palenight)
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

(defcustom dotfairy-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode)
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
  (let ((proto (if (gnutls-available-p) "https" "http")))
    `((melpa    . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                   ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
                   ("melpa"  . ,(format "%s://melpa.org/packages/" proto))))
      (bfsu     . (("gnu"    . ,(format "%s://mirrors.bfsu.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.bfsu.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.bfsu.edu.cn/elpa/melpa/" proto))))
      (netease  . (("gnu"    . ,(format "%s://mirrors.163.com/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.163.com/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.163.com/elpa/melpa/" proto))))
      (sjtu     . (("gnu"    . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/" proto))))
      (tuna     . (("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto))))
      (ustc     . (("gnu"    . ,(format "%s://mirrors.ustc.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.ustc.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.ustc.edu.cn/elpa/melpa/" proto))))))
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

(defcustom dotfairy-lsp-format-on-save
  nil
  "Auto format buffers on save."
  :group 'dotfairy
  :type 'boolean)

(defcustom dotfairy-complete 'ivy
  "Set complete.
`ivy': See https://github.com/abo-abo/swiper.
`vertico': See https://github.com/minad/vertico.
nil means disabled."
  :group 'dotfairy
  :type '(choice (const :tag "ivy" ivy)
                 (const :tag "vertico" vertico)
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
    ("bool" . ?𝔹)
    ("false" . ?𝔽)
    ("true" . ?𝕋)
    ("for" . ?∀))
  "Alist of symbol prettifications.
Nil to use font supports ligatures."
  :group 'dotfairy
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom dotfairy-tree-sitter
  (and (fboundp 'treesit-available-p) (treesit-available-p))
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'dotfairy
  :type 'boolean)


(defcustom dotfairy-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
  :group 'dotfairy
  :type 'boolean)

(defcustom dotfairy-org-repository ""
  "Set network proxy."
  :group 'dotfairy
  :type 'string)

(provide 'init-custom)
;;; init-custom.el ends here
