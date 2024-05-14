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
                               fanyi-longman-provider))))
(use-package go-translate
  :init
  (setq gt-langs '(en zh)
        gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-side-window)
          (side . bottom)
          (slot . 1)
          (window-height . 0.4)))

  (map! :leader
    (:prefix ("d" . "dictionaries")
     "g" #'gt-do-translate
     "s" #'gt-do-setup))
  :config
  ;; Same behavior with `popper'
  (add-hook 'gt-buffer-render-output-hook
            (lambda ()
              "Focus in the dict result window."
              (when-let ((win (get-buffer-window gt-buffer-render-buffer-name)))
                (and (window-live-p win) (select-window win)))))
  (advice-add #'keyboard-quit :before
              (lambda (&rest _)
                "Close dict result window via `C-g'."
                (when-let ((win (get-buffer-window gt-buffer-render-buffer-name)))
                  (and (window-live-p win) (delete-window win)))))
  ;; Tweak child frame
  (defclass gt-posframe-pop-render (gt-buffer-render)
    ((width       :initarg :width        :initform 70)
     (height      :initarg :height       :initform 15)
     (forecolor   :initarg :forecolor    :initform nil)
     (backcolor   :initarg :backcolor    :initform nil)
     (padding     :initarg :padding      :initform 16)
     (bd-width    :initarg :bd-width     :initform 1)
     (bd-color    :initarg :bd-color     :initform nil))
    "Pop up a childframe to show the result.
The frame will disappear when do do anything but focus in it.
Manually close the frame with `q'.")

  (cl-defmethod gt-init ((render gt-posframe-pop-render) translator)
    (with-slots (width height bd-width forecolor backcolor bd-color padding) render
      (let ((inhibit-read-only t)
            (buf gt-posframe-pop-render-buffer))
        ;; create
        (unless (buffer-live-p (get-buffer buf))
          (posframe-show buf
                         :string "Loading..."
                         :timeout gt-posframe-pop-render-timeout
                         :width width
                         :height height
                         :min-width width
                         :min-height height
                         :foreground-color (or forecolor (face-foreground 'tooltip nil t))
                         :background-color (or backcolor (face-background 'tooltip nil t))
                         :internal-border-width bd-width
                         :border-color (or bd-color (face-background 'posframe-border nil t))
                         :left-fringe padding
                         :right-fringe padding
                         :position (point)
                         :poshandler gt-posframe-pop-render-poshandler))
        ;; render
        (gt-buffer-render-init buf render translator)
        (posframe-refresh buf)
        ;; setup
        (with-current-buffer buf
          (gt-buffer-render-key ("q" "Close") (posframe-delete buf))))))
  (setq gt-default-translator
        (if (childframe-workable-p)
            (gt-translator :engines (gt-youdao-dict-engine)
                           :render (gt-posframe-pop-render))
          (gt-translator :engines (list (gt-bing-engine)
                                        (gt-youdao-dict-engine)
                                        (gt-youdao-suggest-engine))
                         :render (gt-buffer-render))))
  (setq gt-preset-translators
        `((default . ,(gt-translator :taker   (cdar (gt-ensure-plain gt-preset-takers))
                                     :engines (cdar (gt-ensure-plain gt-preset-engines))
                                     :render  (cdar (gt-ensure-plain gt-preset-renders))))
          (simple . ,(gt-translator :engines (gt-youdao-dict-engine)
                                    :render (gt-posframe-pop-render)))
          (detailed . ,(gt-translator :engines (list (gt-bing-engine)
                                                     (gt-youdao-dict-engine)
                                                     (gt-youdao-suggest-engine))
                                      :render (gt-buffer-render)))
          (Text-Utility . ,(gt-text-utility
                            :taker (gt-taker :pick nil)
                            :render (gt-buffer-render))))))

;; OSX dictionary
(when IS-MAC
  (use-package osx-dictionary
    :init
    (map! :leader
      (:prefix ("d" . "dictionaries")
       "i" #'osx-dictionary-search-input
       "x" #'osx-dictionary-search-pointer))))

(provide 'init-dict)
;;; init-dict.el ends here
