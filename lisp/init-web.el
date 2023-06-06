;;; init-web.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 b40yd

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
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)
(require 'init-keybinds)

;;; Code:
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-css-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

(use-package js-mode
  :ensure nil
  :defines js-indent-level
  :config
  (setq js-indent-level 2))

;; JavaScript
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)
         (js2-mode . (lambda ()
                       (dotfairy-set-prettify '(
                                                ("function" . ?ƒ)
                                                ("function()" . ?λ)
                                                ("() =>" . ?λ)
                                                ("()=>" . ?λ)
                                                ("null" . ?∅)
                                                ("===" . ?≡)
                                                ("!==" . ?≢)
                                                ("**2" . ?²)
                                                ("**3" . ?³)
                                                ("**4" . ?⁴)
                                                ("**5" . ?⁵)
                                                ("**6" . ?⁶)
                                                ("**7" . ?⁷)
                                                ("**8" . ?⁸)
                                                ("**9" . ?⁹)
                                                ("**-1" . (?⁻ (Br . Bl) ?¹))  ; ⁻¹
                                                ("**-2" . (?⁻ (Br . Bl) ?²))  ; ⁻²
                                                ("**-3" . (?⁻ (Br . Bl) ?³))  ; ⁻³
                                                ("**-4" . (?⁻ (Br . Bl) ?⁴))  ; ⁻⁴
                                                ("**-5" . (?⁻ (Br . Bl) ?⁵))  ; ⁻⁵
                                                ("**-6" . (?⁻ (Br . Bl) ?⁶))  ; ⁻⁶
                                                ("**-7" . (?⁻ (Br . Bl) ?⁷))  ; ⁻⁷
                                                ("**-8" . (?⁻ (Br . Bl) ?⁸))  ; ⁻⁸
                                                ("**-9" . (?⁻ (Br . Bl) ?⁹))  ; ⁻⁹
                                                ("all" . ?∀)  ; custom
                                                ("any" . ?∃)  ; custom
                                                ("undefined" . ?∅)
                                                ("String" . ?𝕊)
                                                ("Infinity" . ?∞))))))
  :config
  ;; Use default keybindings for lsp
  (unbind-key "M-." js2-mode-map))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(when (executable-find "prettier")
  (use-package prettier
    :diminish
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none)))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish
  :hook (((js-mode js2-mode). skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

;; Run Mocha or Jasmine tests
(use-package mocha
  :config (use-package mocha-snippets))

;; Major mode for CoffeeScript code
(use-package coffee-mode
  :config (setq coffee-tab-width 2))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package haml-mode)
(use-package php-mode)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . (lambda ()
                          (when dotfairy-lsp-format-on-save
                            tide-format-before-save)))))

(use-package rjsx-mode
  :mode "\\jsx\\|.[mc]?js\\'"
  :interpreter "node"
  :hook (rjsx-mode . rainbow-delimiters-mode))

(use-package emmet-mode
  :hook ((web-mode rjsx-mode) . emmet-mode)
  :config)

(use-package auto-rename-tag
  :hook ((xml-mode web-mode) . auto-rename-tag-mode))

(after! emmet-mode
  (map! :leader
        :map emmet-mode-keymap
        "<tab>" #'emmet-expand-yas))

(provide 'init-web)
;;; init-web.el ends here
