;;; init-flymake.el ---                                   -*- lexical-binding: t; -*-

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
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)
(require 'init-basic)

(use-package flymake
  :diminish
  :functions my-elisp-flymake-byte-compile
  :hook (prog-mode . flymake-mode)
  :custom
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe
        flymake-margin-indicator-position 'right-margin)

  :config
  ;; Check elisp with ``load-path''
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    ;; checkdoc-params: (fn args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile))


(use-package flyover
  :diminish
  :hook flymake-mode
  :custom
  (flyover-checkers '(flymake))
  (flyover-background-lightness 45)
  (flyover-icon-background-tint-percent 40)
  (flyover-display-mode 'show-only-on-same-line)
  (flyover-show-at-eol t)
  (flyover-virtual-line-type 'none)
  :hook flymake-mode)

(provide 'init-flymake)
;;; init-flymake.el ends here
