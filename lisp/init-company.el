;;; init-company.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  7ym0n.q6e

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

;; Settings for company
(use-package company
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-cancel
  :hook (after-init . global-company-mode)
  :bind (("C-M-i" . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :init
  (setq company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-box-show-single-candidate t
        company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match nil
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)

        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-cancel)
    (call-interactively 'company-yasnippet))

  :config
  ;; `yasnippet' integration
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))
      ;; Enable in current backends
      (my-company-enbale-yas)
      ;; Enable in `lsp-mode'
      (advice-add #'lsp--auto-configure :after #'my-company-enbale-yas)

      (defun my-lsp-fix-company-capf ()
        "Remove redundant `comapny-capf'."
        (setq company-backends
              (remove 'company-backends (remq 'company-capf company-backends))))
      (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fun command arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))

  ;; Better sorting and filtering
  (use-package company-prescient
    :init
    (if dotfairy-company-prescient
        (company-prescient-mode 1)))


  (if (display-graphic-p)
      (use-package company-box
        :diminish
        :defines company-box-icons-all-the-icons
        :hook (company-mode . company-box-mode)
        :init (setq company-box-enable-icon t
                    company-box-backends-colors nil
                    company-box-doc-delay 0.3)
        :config
        (with-no-warnings
          ;; Prettify icons
          (defun my-company-box-icons--elisp (candidate)
            (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
              (let ((sym (intern candidate)))
                (cond ((fboundp sym) 'Function)
                      ((featurep sym) 'Module)
                      ((facep sym) 'Color)
                      ((boundp sym) 'Variable)
                      ((symbolp sym) 'Text)
                      (t . nil)))))
          (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

          ;; Display borders and optimize performance
          (defun my-company-box--display (string on-update)
            "Display the completions."
            (company-box--render-buffer string on-update)

            (let ((frame (company-box--get-frame))
                  (border-color (face-foreground 'font-lock-comment-face nil t)))
              (unless frame
                (setq frame (company-box--make-frame))
                (company-box--set-frame frame))
              (company-box--compute-frame-position frame)
              (company-box--move-selection t)
              (company-box--update-frame-position frame)
              (unless (frame-visible-p frame)
                (make-frame-visible frame))
              (company-box--update-scrollbar frame t)
              (set-face-background 'internal-border border-color frame)
              (when (facep 'child-frame-border)
                (set-face-background 'child-frame-border border-color frame)))
            (with-current-buffer (company-box--get-buffer)
              (company-box--maybe-move-number (or company-box--last-start 1))))
          (advice-add #'company-box--display :override #'my-company-box--display)

          (setq company-box-doc-frame-parameters '((internal-border-width . 1)
                                                   (left-fringe . 8)
                                                   (right-fringe . 8)))

          (defun my-company-box-doc--make-buffer (object)
            (let* ((buffer-list-update-hook nil)
                   (inhibit-modification-hooks t)
                   (string (cond ((stringp object) object)
                                 ((bufferp object) (with-current-buffer object (buffer-string))))))
              (when (and string (> (length (string-trim string)) 0))
                (with-current-buffer (company-box--get-buffer "doc")
                  (erase-buffer)
                  (insert (propertize "\n" 'face '(:height 0.5)))
                  (insert string)
                  (insert (propertize "\n\n" 'face '(:height 0.5)))

                  ;; Handle hr lines of markdown
                  ;; @see `lsp-ui-doc--handle-hr-lines'
                  (with-current-buffer (company-box--get-buffer "doc")
                    (let (bolp next before after)
                      (goto-char 1)
                      (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                        (when (get-text-property next 'markdown-hr)
                          (goto-char next)
                          (setq bolp (bolp)
                                before (char-before))
                          (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                          (setq after (char-after (1+ (point))))
                          (insert
                           (concat
                            (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                            (propertize "\n" 'face '(:height 0.5))
                            (propertize " "
                                        'display '(space :height (1))
                                        'company-box-doc--replace-hr t
                                        'face `(:background ,(face-foreground 'font-lock-comment-face)))
                            (propertize " " 'display '(space :height (1)))
                            (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))

                  (setq mode-line-format nil
                        display-line-numbers nil
                        header-line-format nil
                        show-trailing-whitespace nil
                        cursor-in-non-selected-windows nil)
                  (current-buffer)))))
          (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

          ;; Display the border and fix the markdown header properties
          (defun my-company-box-doc--show (selection frame)
            (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                      (window-configuration-change-hook nil)
                      (inhibit-redisplay t)
                      (display-buffer-alist nil)
                      (buffer-list-update-hook nil))
              (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                             company-box--bottom
                                             company-selection
                                             (company-box--get-frame)
                                             (frame-visible-p (company-box--get-frame))))
                           (candidate (nth selection company-candidates))
                           (doc (or (company-call-backend 'quickhelp-string candidate)
                                    (company-box-doc--fetch-doc-buffer candidate)))
                           (doc (company-box-doc--make-buffer doc)))
                (let ((frame (frame-local-getq company-box-doc-frame))
                      (border-color (face-foreground 'font-lock-comment-face nil t)))
                  (unless (frame-live-p frame)
                    (setq frame (company-box-doc--make-frame doc))
                    (frame-local-setq company-box-doc-frame frame))
                  (set-face-background 'internal-border border-color frame)
                  (when (facep 'child-frame-border)
                    (set-face-background 'child-frame-border border-color frame))
                  (company-box-doc--set-frame-position frame)

                  ;; Fix hr props. @see `lsp-ui-doc--fix-hr-props'
                  (with-current-buffer (company-box--get-buffer "doc")
                    (let (next)
                      (while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
                        (when (get-text-property next 'company-box-doc--replace-hr)
                          (put-text-property next (1+ next) 'display
                                             '(space :align-to (- right-fringe 1) :height (1)))
                          (put-text-property (1+ next) (+ next 2) 'display
                                             '(space :align-to right-fringe :height (1)))))))

                  (unless (frame-visible-p frame)
                    (make-frame-visible frame))))))
          (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

          (defun my-company-box-doc--set-frame-position (frame)
            (-let* ((frame-resize-pixelwise t)

                    (box-frame (company-box--get-frame))
                    (box-position (frame-position box-frame))
                    (box-width (frame-pixel-width box-frame))
                    (box-height (frame-pixel-height box-frame))
                    (box-border-width (frame-border-width box-frame))

                    (window (frame-root-window frame))
                    ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                        (/ (frame-pixel-width) 2)
                                                                        (/ (frame-pixel-height) 2)))
                    (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

                    (x (- (+ (car box-position) box-width) border-width))
                    (space-right (- (frame-pixel-width) x))
                    (space-left (car box-position))
                    (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
                    (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
                    (width (+ text-width border-width fringe-left fringe-right))
                    (x (if (> width space-right)
                           (if (> space-left width)
                               (- space-left width)
                             space-left)
                         x))
                    (y (cdr box-position))
                    (bottom (+ company-box--bottom (frame-border-width)))
                    (height (+ text-height (* 2 border-width)))
                    (y (cond ((= x space-left)
                              (if (> (+ y box-height height) bottom)
                                  (+ (- y height) border-width)
                                (- (+ y box-height) border-width)))
                             ((> (+ y height) bottom)
                              (- (+ y box-height) height))
                             (t y))))
              (set-frame-position frame (max x 0) (max y 0))
              (set-frame-size frame text-width text-height t))))
        (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position)


        (when (icons-displayable-p)
          (declare-function all-the-icons-faicon 'all-the-icons)
          (declare-function all-the-icons-material 'all-the-icons)
          (declare-function all-the-icons-octicon 'all-the-icons)
          (setq company-box-icons-all-the-icons
                `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
                  (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
                  (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                  (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                  (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
                  (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
                  (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
                  (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                  (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                  (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                  (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
                  (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
                  (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
                  (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                  (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
                  (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
                  (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
                  (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
                  (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
                  (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
                  (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
                  (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
                  (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
                  (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
                  (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
                  (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
                  (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
                company-box-icons-alist 'company-box-icons-all-the-icons)))


    (use-package company-quickhelp
      :hook (global-company-mode . company-quickhelp-mode)
      :defines company-quickhelp-delay
      :bind (:map company-active-map
             ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
      :init (setq company-quickhelp-delay 0.0))
    ;; Display documentation for completion candidates in terminal
    (use-package company-quickhelp-terminal
      :defines company-quickhelp-delay
      :bind (:map company-active-map
             ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
      :hook ((global-company-mode . company-quickhelp-mode)
             (company-quickhelp-mode  . company-quickhelp-terminal-mode))
      :init (setq company-quickhelp-delay 0.3))))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)
(provide 'init-company)
;;; init-company.el ends here
