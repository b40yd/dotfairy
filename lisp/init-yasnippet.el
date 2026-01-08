;;; init-yasnippet.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2020-2026 b40yd

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

(defun set-yas-minor-mode! (modes)
  "Register minor MODES (one mode symbol or a list of them) with yasnippet so it
can have its own snippets category, if the folder exists."
  (dolist (mode (ensure-list modes))
    (let ((fn (intern (format "+snippets-register-%s-h" mode))))
      (fset fn (lambda () (yas-activate-extra-mode mode)))
      (add-hook (intern (format "%s-hook" mode)) fn))))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
