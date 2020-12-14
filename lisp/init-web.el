;;; init-web.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  7ym0n.q6e

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

(use-package web-mode
  :mode ("\\.html\\'")
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-enable-current-element-highlight t
        web-mode-enable-css-colorization t)
  (use-package company-web
    :config
    (add-to-list 'company-backends 'company-web-html)
    (add-to-list 'company-backends 'company-css))
  )

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (add-hook 'css-mode-hook (lambda()
                             (add-to-list (make-local-variable 'company-backends)
                                          '(company-css company-files company-yasnippet company-capf))))
  (setq css-indent-offset 4))

(use-package scss-mode
  :ensure t
  :mode "\\scss\\'")

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 4))

;; npm install -g typescript
(defun setup-tide-mode()
  "setup function for tide"
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))


(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

(use-package emmet-mode
  :ensure t
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda()
                               (setq emmet-indent-after-insert t))))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(use-package prettier-js
  :diminish
  :hook (rjsx-mode
         .
         prettier-js-mode))

(provide 'init-web)
;;; init-web.el ends here
