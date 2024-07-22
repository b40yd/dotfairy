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
    :commands (gt-do-translate
               gt-do-translate-prompt
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
       "G" #'gt-do-translate-prompt
       "p" #'gt-do-speak
       "s" #'gt-do-setup
       "u" #'gt-do-text-utility))
    :config
    (with-no-warnings
      (setq gt-preset-translators
            `((default . ,(gt-translator
                           :taker   (list (gt-taker :pick nil :if 'selection)
                                          (gt-taker :text 'paragraph :if '(Info-mode help-mode helpful-mode devdocs-mode))
                                          (gt-taker :text 'buffer :pick 'fresh-word :if 'read-only)
                                          (gt-taker :text 'word))
                           :engines (if (display-graphic-p)
                                        (list (gt-bing-engine :if 'not-word)
                                              (gt-youdao-dict-engine :if 'word))
                                      (list (gt-bing-engine :if 'not-word)
                                            (gt-youdao-dict-engine :if 'word)
                                            (gt-youdao-suggest-engine :if 'word)
                                            (gt-google-engine :if 'word)))
                           :render  (list (gt-posframe-pop-render :if (lambda (translator)
                                                                        (and (display-graphic-p)
                                                                             (not (derived-mode-p 'Info-mode 'help-mode 'helpful-mode 'devdocs-mode))
                                                                             (not (member (buffer-name) '("COMMIT_EDITMSG")))))
                                                                  :frame-params (list :accept-focus nil
                                                                                      :width 70
                                                                                      :height 15
                                                                                      :left-fringe 16
                                                                                      :right-fringe 16
                                                                                      :border-width 1
                                                                                      :border-color gt-pin-posframe-bdcolor))
                                          (gt-overlay-render :if 'read-only)
                                          (gt-insert-render :if (lambda (translator) (member (buffer-name) '("COMMIT_EDITMSG"))))
                                          (gt-buffer-render))))
              (multi-dict . ,(gt-translator :taker (gt-taker :prompt t)
                                            :engines (list (gt-bing-engine)
                                                           (gt-youdao-dict-engine)
                                                           (gt-youdao-suggest-engine :if 'word)
                                                           (gt-google-engine))
                                            :render (gt-buffer-render)))
              (Text-Utility . ,(gt-text-utility :taker (gt-taker :pick nil)
                                                :render (gt-buffer-render)))))

      (defun gt--do-translate (dict)
        "Translate using DICT from the preset tranlators."
        (gt-start (alist-get dict gt-preset-translators)))

      (defun gt-do-translate-prompt ()
        "Translate with prompt using the multiple dictionaries."
        (interactive)
        (gt--do-translate 'multi-dict))

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
