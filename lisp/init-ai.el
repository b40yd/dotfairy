;;; init-ai.el --- Initialize AI configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 b40yd
;;
;; Author: b40yd <b40yd@scanbuf.com>
;; Maintainer: b40yd <b40yd@scanbuf.com>
;; Created: July 24, 2025
;; Modified: July 24, 2025
;; Version: 0.0.1
;; Keywords:
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:

;;; Code:

;; A native shell experience to interact with ACP agents
(use-package agent-shell
  :diminish agent-shell-ui-mode)

(provide 'init-ai)
;;; init-ai.el ends here
