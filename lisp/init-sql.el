;;; init-sql.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2022, 7ym0n, all rights reserved.

;; Author: 7ym0n <bb.qnyd@gmail.com>
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


;; pip3 install sqlparse
(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode))

(use-package ejc-sql :commands ejc-sql-mode ejc-connect)

(provide 'init-sql)
;;; init-sql.el ends here
