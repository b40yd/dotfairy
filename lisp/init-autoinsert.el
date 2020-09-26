;;; init-autoinsert.el ---                                   -*- lexical-binding: t; -*-

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

;;
;; auto insert
;;
(defun my/autoinsert-yas-expand()
  "replace text in yasnippet template"
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))


(use-package autoinsert
  :ensure t
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (concat dotfairy-emacs-dir "private/templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode t)
  (define-auto-insert "\\.org$" ["template.org" my/autoinsert-yas-expand])
  (define-auto-insert "\\.js$" ["template.js" my/autoinsert-yas-expand])
  (define-auto-insert "\\.ts$" ["template.ts" my/autoinsert-yas-expand])
  (define-auto-insert "\\.html$" ["template.html" my/autoinsert-yas-expand])
  (define-auto-insert "\\.([Hh]|hh|hpp)$" ["template.h" my/autoinsert-yas-expand])
  (define-auto-insert "\\.([Cc]|cc|cpp)$" ["template.cc" my/autoinsert-yas-expand])
  (define-auto-insert "\\.sh$" ["template.sh" my/autoinsert-yas-expand])
  (define-auto-insert "\\.py$" ["template.py" my/autoinsert-yas-expand])
  (define-auto-insert "[mM]akefile$" ["Makefile" my/autoinsert-yas-expand])
  )

(provide 'init-autoinsert)
;;; init-autoinsert.el ends here
