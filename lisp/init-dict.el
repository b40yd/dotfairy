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
                               fanyi-youdao-thesaurus-provider
                               fanyi-etymon-provider
                               fanyi-longman-provider))))


(when emacs/28
  (use-package go-translate
    :commands (gt-bing-translate
               gt-bing-translate-dwim
               gt-youdao-dict-translate-dwim
               gt-youdao-dict-translate
               gt-multi-dict-translate-dwim
               gt-multi-dict-translate
               gt-do-setup
               gt-do-text-utility)
    :init
    (setq gt-langs '(en zh)
          gt-buffer-render-follow-p t
          gt-buffer-render-window-config
          '((display-buffer-reuse-window display-buffer-in-direction)
            (direction . bottom)
            (window-height . 0.4)))

    (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
          gt-pop-posframe-backcolor (face-background 'tooltip nil t))
    (when (facep 'posframe-border)
      (setq gt-pin-posframe-bdcolor (face-background 'posframe-border nil t)))

    (map! :leader
      (:prefix ("d" . "dictionaries")
       "g" #'gt-do-translate
       "s" #'gt-do-setup
       "b" #'gt-bing-translate-dwim
       "B" #'gt-bing-translate
       "m" #'gt-multi-dict-translate-dwim
       "M" #'gt-multi-dict-translate
       "y" #'gt-youdao-dict-translate-dwim
       "Y" #'gt-youdao-dict-translate
       "u" #'gt-do-text-utility))

    :config
    ;; Tweak child frame
    (with-no-warnings

      ;; Translators
      (setq gt-preset-translators
            `((default          . ,(gt-translator :taker   (cdar (gt-ensure-plain gt-preset-takers))
                                                  :engines (cdar (gt-ensure-plain gt-preset-engines))
                                                  :render  (cdar (gt-ensure-plain gt-preset-renders))))
              (youdao-dict      . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word :prompt t)
                                                  :engines (gt-youdao-dict-engine)
                                                  :render (gt-buffer-render)))
              (youdao-dict-dwim . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word)
                                                  :engines (gt-youdao-dict-engine)
                                                  :render (if (display-graphic-p)
                                                              (gt-posframe-pop-render
                                                               :frame-params (list :accept-focus nil
                                                                                   :width 70
                                                                                   :height 15
                                                                                   :left-fringe 16
                                                                                   :right-fringe 16
                                                                                   :border-width 1
                                                                                   :border-color gt-pin-posframe-bdcolor))
                                                            (gt-buffer-render))))
              (bing             . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word :prompt t)
                                                  :engines (gt-bing-engine)
                                                  :render (gt-buffer-render)))
              (bing-dwim        . ,(gt-translator :taker (gt-taker :langs '(en zh) :text 'word)
                                                  :engines (gt-bing-engine)
                                                  :render (if (display-graphic-p)
                                                              (gt-posframe-pop-render
                                                               :frame-params (list :accept-focus nil
                                                                                   :width 70
                                                                                   :height 15
                                                                                   :left-fringe 16
                                                                                   :right-fringe 16
                                                                                   :border-width 1
                                                                                   :border-color gt-pin-posframe-bdcolor))
                                                            (gt-buffer-render))))
              (multi-dict       . ,(gt-translator :taker (gt-taker :langs '(en zh) :prompt t)
                                                  :engines (list (gt-bing-engine)
                                                                 (gt-youdao-dict-engine)
                                                                 (gt-youdao-suggest-engine)
                                                                 (gt-google-engine))
                                                  :render (gt-buffer-render)))
              (multi-dict-dwim  . ,(gt-translator :taker (gt-taker :langs '(en zh))
                                                  :engines (list (gt-bing-engine)
                                                                 (gt-youdao-dict-engine)
                                                                 (gt-youdao-suggest-engine)
                                                                 (gt-google-engine))
                                                  :render (gt-buffer-render)))
              (Text-Utility     . ,(gt-text-utility :taker (gt-taker :pick nil)
                                                    :render (gt-buffer-render)))))
      (setq gt-default-translator (alist-get 'multi-dict-dwim gt-preset-translators))

      (defun gt--do-translate (dict)
        "Translate using DICT from preset tranlators."
        (gt-start (alist-get dict gt-preset-translators)))

      (defun gt-youdao-dict-translate ()
        "Translate using Youdao dictionary."
        (interactive)
        (gt--do-translate 'youdao-dict))

      (defun gt-youdao-dict-translate-dwim ()
        "Translate using Youdao dictionary without any prompt."
        (interactive)
        (gt--do-translate 'youdao-dict-dwim))

      (defun gt-bing-translate ()
        "Translate using Bing."
        (interactive)
        (gt--do-translate 'bing))

      (defun gt-bing-translate-dwim ()
        "Translate using Bing without any prompt."
        (interactive)
        (gt--do-translate 'bing-dwim))

      (defun gt-multi-dict-translate ()
        "Translate using multiple dictionaries."
        (interactive)
        (gt--do-translate 'multi-dict))

      (defun gt-multi-dict-translate-dwim ()
        "Translate using multiple dictionaries without any prompt."
        (interactive)
        (gt--do-translate 'multi-dict-dwim))

      (defun gt-do-text-utility ()
        "Handle the texts with the utilities."
        (interactive)
        (gt--do-translate 'Text-Utility)))))


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
