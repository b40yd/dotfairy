;;; init-dict.el ---                                   -*- lexical-binding: t; -*-

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
(require 'init-keybinds)

;; A multi dictionaries interface
(when emacs/27
  (use-package fanyi
    :init
    (map! :leader
      (:prefix ("d" . "dictionaries")
       "f" #'fanyi-dwim
       "d" #'fanyi-dwim2
       "h" #'fanyi-from-history))
    :custom (fanyi-providers '(fanyi-haici-provider
                               fanyi-longman-provider)))
  (use-package go-translate
    :init
    (map! :leader
      (:prefix ("d" . "dictionaries")
       "g" #'gts-do-translate))
    (setq gts-translate-list '(("en" "zh") ("zh" "en")))))

;; Youdao Dictionary
(use-package youdao-dictionary
  :init
  (map! :leader
    (:prefix ("d" . "dictionaries")
     "y" #'youdao-dictionary-search-async
     :map youdao-dictionary-mode-map
     "h" #'my-youdao-dictionary-help
     "?" #'my-youdao-dictionary-help))
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra youdao-dictionary-hydra (:color blue)
        ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
        ("y" youdao-dictionary-play-voice-at-point "play voice at point")
        ("q" quit-window "quit")
        ("C-g" nil nil)
        ("h" nil nil)
        ("?" nil nil))
      (defun my-youdao-dictionary-help ()
        "Show help in `hydra'."
        (interactive)
        (let ((hydra-hint-display-type 'message))
          (youdao-dictionary-hydra/body))))

    (defun my-youdao-dictionary--posframe-tip (string)
      "Show STRING using posframe-show."
      (unless (posframe-workable-p)
        (error "Posframe not workable"))

      (let ((word (youdao-dictionary--region-or-word)))
        (if word
            (progn
              (with-current-buffer (get-buffer-create youdao-dictionary-buffer-name)
                (erase-buffer)
                (youdao-dictionary-mode)
                (insert (propertize "\n" 'face '(:height 0.5)))
                (insert string)
                (insert (propertize "\n" 'face '(:height 0.5)))
                (set (make-local-variable 'youdao-dictionary-current-buffer-word) word)))
          (posframe-show youdao-dictionary-buffer-name
                         :position (point)
                         :left-fringe 16
                         :right-fringe 16
                         :max-width (/ (frame-width) 2)
                         :max-height (/ (frame-height) 2)
                         :background-color (face-background 'tooltip nil t)
                         :internal-border-color (face-foreground 'posframe-border nil t)
                         :internal-border-width 1)
          (unwind-protect
              (push (read-event) unread-command-events)
            (progn
              (posframe-hide youdao-dictionary-buffer-name)
              (other-frame 0))))
        (message "Nothing to look up"))))
  (advice-add #'youdao-dictionary--posframe-tip
              :override #'my-youdao-dictionary--posframe-tip))
(provide 'init-dict)
;;; init-dict.el ends here
