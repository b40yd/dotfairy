;;; init-hydra.el ---                                  -*- lexical-binding: t; -*-

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

;;; Code:

(use-package hydra
  :defines (consult-imenu-config posframe-border-width)
  :functions childframe-completion-workable-p hydra-set-posframe-show-params
  :hook ((emacs-lisp-mode  . hydra-add-imenu)
         (after-load-theme . hydra-set-posframe-appearance))
  :init
  (with-eval-after-load 'consult-imenu
    (setq consult-imenu-config
          '((emacs-lisp-mode :toplevel "Functions"
                             :types ((?f "Functions" font-lock-function-name-face)
                                     (?h "Hydras"    font-lock-constant-face)
                                     (?m "Macros"    font-lock-function-name-face)
                                     (?p "Packages"  font-lock-constant-face)
                                     (?t "Types"     font-lock-type-face)
                                     (?v "Variables" font-lock-variable-name-face))))))
  (defun hydra-set-posframe-appearance ()
    "Set appearance of hydra."
    (when (childframe-completion-workable-p)
      (setq hydra-hint-display-type 'posframe)
      (setq hydra-posframe-show-params
            `(:left-fringe 8
              :right-fringe 8
              :internal-border-width ,posframe-border-width
              :internal-border-color ,(face-background 'posframe-border nil t)
              :background-color ,(face-background 'tooltip nil t)
              :foreground-color ,(face-foreground 'tooltip nil t)
              :lines-truncate t
              :poshandler posframe-poshandler-frame-center-near-bottom))))
  (hydra-set-posframe-appearance))

(use-package pretty-hydra
  :functions icons-displayable-p
  :bind ("C-c <f2>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . pretty-hydra-add-imenu)
  :init
  (defun pretty-hydra-add-imenu ()
    "Have hydras in `imenu'."
    (add-to-list 'imenu-generic-expression
                 '("Hydras" "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)" 2)))
  (with-no-warnings
    (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                        &key face height v-adjust)
      "Add an icon in the hydra title."
      (let ((face (or face 'mode-line-emphasis))
            (height (or height 1.0))
            (v-adjust (or v-adjust 0.0)))
        (concat
         (when (and (icons-displayable-p) icon-type icon-name)
           (let ((f (intern (format "nerd-icons-%s" icon-type))))
             (when (fboundp f)
               (concat
                (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                " "))))
         (propertize title 'face face))))

    ;; Global toggles
    (pretty-hydra-define toggles-hydra
      (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on")
       :color amaranth :quit-key "q")
      ("Basic"
       (("n" display-line-numbers-mode "line number" :toggle t)
        ("a" global-aggressive-indent-mode "aggressive indent *" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete *" :toggle t)
        ("e" electric-pair-mode "electric pair *" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines *" :toggle t)
        ("b" display-battery-mode "battery *" :toggle t)
        ("i" display-time-mode "time *" :toggle t)
        ("m" doom-modeline-mode "modern mode-line *" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line *" :toggle t)
        ("h p" show-paren-mode "parenthesis *" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" global-colorful-mode "color *" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" indent-bars-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo *" :toggle t))
       "Coding"
       (("f" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "current function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter *" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter *" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter *" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       ))))

(provide 'init-hydra)
