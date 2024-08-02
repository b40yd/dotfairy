;;; init-make.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2024, b40yd, all rights reserved.

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


(use-package makefile-executor)

;;;###autoload
(defun +make/run ()
  "Run a make task in the current project. If multiple makefiles are available,
you'll be prompted to select one."
  (interactive)
  (if (project-current)
      (makefile-executor-execute-project-target)
    (let ((makefile (cl-loop with buffer-file = (or buffer-file-name default-directory)
                             for file in (list "Makefile" "makefile")
                             if (locate-dominating-file buffer-file file)
                             return file)))
      (unless makefile
        (user-error "Cannot find a makefile in the current project"))
      (let ((default-directory (file-name-directory makefile)))
        (makefile-executor-execute-target makefile)))))

;;;###autoload
(defun +make/run-last ()
  "TODO"
  (interactive)
  (makefile-executor-execute-last nil))

(provide 'init-make)
;;; init-make.el ends here
