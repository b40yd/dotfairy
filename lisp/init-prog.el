;;; init-prog.el ---                                   -*- lexical-binding: t; -*-

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
;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                         ("<-" . ?←)
                                         ("->" . ?→)
                                         ("->>" . ?↠)
                                         ("=>" . ?⇒)
                                         ("map" . ?↦)
                                         ("/=" . ?≠)
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
                                         ("not" . ?¬)))
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

(provide 'init-prog)
;;; init-prog.el ends here
