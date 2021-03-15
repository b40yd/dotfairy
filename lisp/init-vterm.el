;;; init-vterm.el ---                                -*- lexical-binding: t; -*-

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

(use-package vterm
  :if (executable-find "cmake")
  :bind (:map vterm-mode-map
              ("C-s" . counsel-grep-or-swiper)
              ("C-y" . vterm-yank))
  :bind ("C-c t m" . multi-term-hydra/body)
  :config

  ;; disable some unnecessary minor-modes in term-mode
  (add-hook 'vterm-mode-hook (lambda ()
                               (yas-minor-mode -1)
                               (setq-local global-hl-line-mode nil)

                               ;; Prevent premature horizontal scrolling
                               (setq-local hscroll-margin 0)))

  ;; vterm buffers are killed when the associated process is terminated
  (setq vterm-kill-buffer-on-exit t))

;; vterm-toggle: toggles between the vterm buffer and whatever buffer you are editing.
;; https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle
  :if (executable-find "cmake")
  :after vterm)

;; multi-vterm: manage multiple terminal windows easily within emacs
;; https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :after vterm
  :if (executable-find "cmake")
  :config
  ;; hydra for using multi-vterm
  (defhydra multi-term-hydra ()
    "multi-term"
    ("o" multi-vterm "new terminal")
    ("t" vterm-toggle-cd "toggle/open")
    ("n" multi-vterm-next "Next")
    ("p" multi-vterm-prev "Prev")
    ("d" multi-vterm-dedicated-toggle "Dedicated terminal")
    ("r" multi-vterm-projectile "vterm projectile")
    ("q" nil "Quit" :color blue)))

(provide 'init-vterm)
;;; init-vterm.el ends here
