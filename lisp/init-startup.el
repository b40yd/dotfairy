;;; init-startup.el ---                                   -*- lexical-binding: t; -*-

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
(require 'init-package)

;; Base
(require 'init-basic)
(require 'init-hydra)
(require 'init-keybinds)

;; UI
(require 'init-ui)

;; editor
(require 'init-iedit)

;; Project
(require 'init-projectile)
(require 'init-ibuffer)
(require 'init-windows)
(require 'init-persp)
(require 'init-calendar)

;;checking
(require 'init-flycheck)

;; Complete
(require 'init-company)
(require 'init-ivy)
(require 'init-autoinsert)

;; Tools
(require 'init-lsp)
(require 'init-git)
(require 'init-eshell)
(require 'init-dired)
(require 'init-restclient)
(require 'init-docker)
(require 'init-vterm)

;; Language
(require 'init-clang)
(require 'init-go)
(require 'init-rust)
(require 'init-python)
(require 'init-web)
(require 'init-markdown)
(require 'init-json)
(require 'init-org)
(require 'init-yaml)
(require 'init-asm)
(require 'init-prog)

;; Debugger
(require 'init-debugger)


(provide 'init-startup)
;;; init-startup.el ends here
