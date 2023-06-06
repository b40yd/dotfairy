;;; init-flymake.el ---                                   -*- lexical-binding: t; -*-

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
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-fringe-indicator-position 'right-fringe
              elisp-flymake-byte-compile-load-path load-path))

(use-package flymake-diagnostic-at-point
  :commands flymake-diagnostic-at-point-mode
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (when (and (childframe-workable-p)
             (require 'posframe nil t))
    (defvar flymake-posframe-buffer " *flymake-posframe-buffer*"
      "Name of the flymake posframe buffer.")
    (defun flymake-diagnostic-at-point-display-posframe (text)
      "Display the flymake diagnostic TEXT inside a child frame."
      (posframe-show
       flymake-posframe-buffer
       :string (propertize
                (concat flymake-diagnostic-at-point-error-prefix text)
                'face (if-let ((type (get-char-property (point) 'flymake-diagnostic)))
                          (pcase (flymake--diag-type type)
                            (:error 'error)
                            (:warning 'warning)
                            (:note 'success)
                            (_ 'default))
                        'default))
	   :left-fringe 4
	   :right-fringe 4
       :max-width (round (* (frame-width) 0.62))
       :max-height (round (* (frame-height) 0.62))
       :internal-border-width 1
       :internal-border-color (face-background 'posframe-border nil t)
       :background-color (face-background 'tooltip nil t))
      (unwind-protect
          (push (read-event) unread-command-events)
        (progn
          (posframe-hide flymake-posframe-buffer)
          (other-frame 0))))
    (setq flymake-diagnostic-at-point-display-diagnostic-function
          #'flymake-diagnostic-at-point-display-posframe)))


(provide 'init-flymake)
;;; init-flymake.el ends here
