;;; init-kill-ring.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n, all rights reserved.

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
;; Kill & Mark things easily
(use-package easy-kill-extras
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark-sexp)
         ([remap mark-word] . easy-mark-word)

         ;; Integrate `zap-to-char'
         ([remap zap-to-char] . easy-mark-to-char)
         ([remap zap-up-to-char] . easy-mark-up-to-char)

         ;; Integrate `expand-region'
         :map easy-kill-base-map
         ("o" . easy-kill-er-expand)
         ("i" . easy-kill-er-unexpand))
  :init (setq kill-ring-max 200
              save-interprogram-paste-before-kill t ; Save clipboard contents before replacement
              easy-kill-alist '((?w word           " ")
                                (?s sexp           "\n")
                                (?l list           "\n")
                                (?f filename       "\n")
                                (?d defun          "\n\n")
                                (?D defun-name     " ")
                                (?e line           "\n")
                                (?b buffer-file-name)

                                (?^ backward-line-edge "")
                                (?$ forward-line-edge "")
                                (?h buffer "")
                                (?< buffer-before-point "")
                                (?> buffer-after-point "")
                                (?f string-to-char-forward "")
                                (?F string-up-to-char-forward "")
                                (?t string-to-char-backward "")
                                (?T string-up-to-char-backward ""))))

(provide 'init-kill-ring)
;;; init-kill-ring.el ends here
