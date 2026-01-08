;;; init-basic.el ---                                -*- lexical-binding: t; -*-

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

(require 'init-funcs)
(require 'init-ligatures)

(use-package rust-mode
  :mode ("\\.rs\\'" . rustic-mode)
  :init (setq rust-format-on-save t)
  :config
  (set-ligatures! 'rust-mode
    ;; Functional
    :def "fn"
    ;; Types
    :null "None"
    :true "true" :false "false"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    ;; Flow
    :in "in"
    :and "&&" :or "||"
    :for "for"
    :return "return")

  ;; HACK: `global-treesit-auto-mode' will override `rust-mode'.
  (define-derived-mode rustic-mode rust-mode "Rust"
    "Major mode for Rust code.

\\{rust-mode-map}")

  (when (treesit-available-p)
    (setq rust-mode-treesitter-derive t)
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-ts-mode) auto-mode-alist))))

(use-package rust-playground)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
