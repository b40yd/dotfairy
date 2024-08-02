;;; init-pass.el ---                                   -*- lexical-binding: t; -*-

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
(use-package pass
  :config
  (setq pass-username-fallback-on-filename t))

(pcase dotfairy-complete
  ('ivy
  ;;;###autoload
   (defun +pass/ivy (arg)
     "Complete and act on password store entries."
     (interactive "P")
     (ivy-read "Pass: " (password-store-list)
               :action (if arg
                           #'password-store-url
                         #'password-store-copy)
               :caller '+pass/ivy))

   (after! ivy
     (ivy-add-actions
      '+pass/ivy
      '(("o" password-store-copy "copy password")
        ("e" password-store-edit "edit entry")
        ("u" +pass/copy-user "copy username")
        ("b" password-store-url "open url in browser")
        ("f" password-store-copy-field "get field")))))
  ('vertico

  ;;;###autoload
   (defun +pass/consult (arg pass)
     "TODO"
     (interactive
      (list current-prefix-arg
            (progn
              (require 'consult)
              (consult--read (password-store-list)
                             :prompt "Pass: "
                             :sort nil
                             :require-match t
                             :category 'pass))))
     (funcall (if arg
                  #'password-store-url
                #'password-store-copy)
              pass))))


(provide 'init-pass)
;;; init-pass.el ends here
