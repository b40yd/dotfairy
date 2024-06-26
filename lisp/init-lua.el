;;; init-lua.el ---                                   -*- lexical-binding: t; -*-

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

(require 'init-funcs)
;;; Code:

(use-package lua-mode
  :config

  (set-ligatures! 'lua-mode
    ;; Functional
    :def "function"
    ;; Types
    :null "nil"
    ;; Flow
    :and "and" :or "or"
    :not "not"
    :for "for"
    :return "return"))
(use-package moonscript
  :defer t
  :config
  ;;;###autoload
  (defun +lua-moonscript-fix-single-quotes-h ()
    "Single-quoted strings aren't treated as strings."
    ;; (modify-syntax-entry ?\" "\"" moonscript-mode-syntax-table)
    (modify-syntax-entry ?\' "\"" moonscript-mode-syntax-table))

  ;;;###autoload
  (defun +lua-moonscript-fontify-interpolation-h ()
    "Highlight interpolated expressions in moonscript strings."
    (font-lock-add-keywords
     nil '(("#{\\([^}]+\\)}"
            (0 font-lock-preprocessor-face t)
            (1 nil t)))))
  (add-hook! 'moonscript-mode-hook
             #'+lua-moonscript-fix-single-quotes-h
             #'+lua-moonscript-fontify-interpolation-h))

(provide 'init-lua)
;;; init-lua.el ends here
