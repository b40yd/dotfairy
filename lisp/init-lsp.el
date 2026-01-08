;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 b40yd

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
(require 'init-const)
(require 'init-custom)


(pcase dotfairy-lsp
  ('eglot
   (use-package eglot
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode
                                                   'makefile-mode 'snippet-mode
                                                   'ron-mode)
                             (eglot-ensure))))
            ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
     :init
     (setq read-process-output-max (* 1024 1024)) ; 1MB
     (setq eglot-autoshutdown t
           eglot-events-buffer-size 0
           eglot-send-changes-idle-time 0.5))
   (use-package consult-eglot
     :after eglot
     :bind (:map eglot-mode-map
            ("C-M-." . consult-eglot-symbols))))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :diminish
     :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
     :autoload lsp-enable-which-key-integration
     :commands (lsp-format-buffer lsp-organize-imports +default/lsp-command-map)
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                             (lsp-deferred))))
            ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
            (lsp-mode . (lambda ()
                          ;; Integrate `which-key'
                          (lsp-enable-which-key-integration)

                          (defun lsp-format-buffer-no-error ()
                            (condition-case _ (lsp-format-buffer)
                              (lsp-capability-not-supported nil)))
                          (defun lsp-format-before-save ()
                            (when (and (boundp 'lsp-mode) lsp-mode dotfairy-lsp-format-on-save)
                              (lsp-format-buffer-no-error)))
                          ;; Format and organize imports
                          (when (and dotfairy-lsp-format-on-save
                                     (not (apply #'derived-mode-p dotfairy-lsp-format-on-save-ignore-modes)))
                            (add-hook 'before-save-hook #'lsp-format-before-save)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
     :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point)
            ([remap xref-find-definitions] . lsp-find-definition)
            ([remap xref-find-references] . lsp-find-references))
     :init (setq lsp-use-plists t
                 lsp-log-io nil

                 lsp-keymap-prefix "C-c l"
                 lsp-keep-workspace-alive nil
                 lsp-signature-auto-activate nil
                 lsp-modeline-code-actions-enable nil
                 lsp-modeline-diagnostics-enable nil
                 lsp-modeline-workspace-status-enable nil
                 lsp-inlay-hint-enable t

                 ;; For corfu
                 lsp-completion-provider :none

                 lsp-semantic-tokens-enable t
                 lsp-progress-spinner-type 'progress-bar-filled

                 lsp-enable-file-watchers nil
                 lsp-enable-folding nil
                 lsp-enable-symbol-highlighting nil
                 lsp-enable-text-document-color nil

                 lsp-enable-indentation nil
                 lsp-enable-on-type-formatting nil
                 lsp-lens-enable nil

                 lsp-session-file (concat dotfairy-etc-dir "lsp-session")
                 lsp-server-install-dir (concat dotfairy-etc-dir "lsp/")

                 ;; For diagnostics
                 lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

                 ;; For clients
                 lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
     :config
     (add-to-list 'auto-mode-alist '("\\.dir-locals\\.el\\'" . emacs-lisp-mode))
     (add-hook! 'dotfairy-escape-hook
       (defun +lsp-signature-stop-maybe-h ()
         "Close the displayed `lsp-signature'."
         (when lsp-signature-mode
           (lsp-signature-stop)
           t)))

     (defvar +lsp--default-read-process-output-max nil)
     (defvar +lsp--default-gcmh-high-cons-threshold nil)
     (defvar +lsp--optimization-init-p nil)

     (define-minor-mode +lsp-optimization-mode
       "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
       :global t
       :init-value nil
       (if (not +lsp-optimization-mode)
           ;; Only apply these settings once! A minor mode's body is triggered each
           ;; time it is called, even if it's already in the desired state.
           (when +lsp--optimization-init-p
             (setq-default read-process-output-max +lsp--default-read-process-output-max
                           gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                           +lsp--optimization-init-p nil))
         ;; See above.
         (unless +lsp--optimization-init-p
           (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)
                 +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
           (setq-default read-process-output-max (* 1024 1024))
           ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
           ;;        library, so we up the GC threshold to stave off GC-induced
           ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
           ;;        so we modify its variables rather than `gc-cons-threshold'
           ;;        directly.
           (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
           (when (bound-and-true-p gcmh-mode)
             (gcmh-set-high-threshold))
           (setq +lsp--optimization-init-p t))))

     (add-hook 'lsp-before-initialize-hook #'+lsp-optimization-mode)
     (add-hook! 'lsp-after-uninitialized-functions
       (defun +lsp--disable-optimization-mode-if-no-workspaces-h (_workspace)
         (unless (lsp--session-workspaces lsp--session)
           (+lsp-optimization-mode -1))))

     (use-package consult-lsp
       :init
       (map! :map lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
       :bind (:map lsp-mode-map
              ("C-M-." . consult-lsp-symbols)))

     ;;;###autoload
     (defun +default/lsp-command-map ()
       "Lazily invoke `lsp-command-map'."
       (interactive)
       (require 'lsp-mode)
       (map! :leader "c l" lsp-command-map)
       (dolist (leader-key (list dotfairy-leader-key dotfairy-leader-alt-key))
         (let ((lsp-keymap-prefix (concat leader-key " c l")))
           (lsp-enable-which-key-integration)))
       (setq prefix-arg current-prefix-arg
             unread-command-events
             (mapcar (lambda (e) (cons t e))
                     (vconcat (when (bound-and-true-p evil-this-operator)
                                (where-is-internal evil-this-operator
                                                   evil-normal-state-map
                                                   t))
                              (this-command-keys)))))

     (with-no-warnings
       ;;Select what codelenses should be enabled or not.
       (with-eval-after-load 'lsp-go
         (setq lsp-go-codelenses '((generate . t)
                                   (test . t)
                                   (tidy . t))))

       ;; Disable `lsp-mode' in `git-timemachine-mode'
       (defun my-lsp--init-if-visible (fn &rest args)
         (unless (bound-and-true-p git-timemachine-mode)
           (apply fn args)))
       (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

       ;; Enable `lsp-mode' in sh/bash/zsh
       (defun my-lsp-bash-check-sh-shell (&rest _)
         (and (memq major-mode '(sh-mode bash-ts-mode))
              (memq sh-shell '(sh bash zsh))))
       (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)
       (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))

       ;; Display icons
       (when (icons-displayable-p)
         (defun my-lsp-icons-get-by-file-ext (file-ext &optional feature)
           (when (and file-ext
                      (lsp-icons--enabled-for-feature feature))
             (nerd-icons-icon-for-extension file-ext)))
         (advice-add #'lsp-icons-get-by-file-ext :override #'my-lsp-icons-get-by-file-ext)

         (defvar lsp-symbol-alist
           '((misc          nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-warning-face)
             (document      nerd-icons-codicon "nf-cod-symbol_file" :face font-lock-string-face)
             (namespace     nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-type-face)
             (string        nerd-icons-codicon "nf-cod-symbol_string" :face font-lock-doc-face)
             (boolean-data  nerd-icons-codicon "nf-cod-symbol_boolean" :face font-lock-builtin-face)
             (numeric       nerd-icons-codicon "nf-cod-symbol_numeric" :face font-lock-builtin-face)
             (method        nerd-icons-codicon "nf-cod-symbol_method" :face font-lock-function-name-face)
             (field         nerd-icons-codicon "nf-cod-symbol_field" :face font-lock-variable-name-face)
             (localvariable nerd-icons-codicon "nf-cod-symbol_variable" :face font-lock-variable-name-face)
             (class         nerd-icons-codicon "nf-cod-symbol_class" :face font-lock-type-face)
             (interface     nerd-icons-codicon "nf-cod-symbol_interface" :face font-lock-type-face)
             (property      nerd-icons-codicon "nf-cod-symbol_property" :face font-lock-variable-name-face)
             (indexer       nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
             (enumerator    nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
             (enumitem      nerd-icons-codicon "nf-cod-symbol_enum_member" :face font-lock-builtin-face)
             (constant      nerd-icons-codicon "nf-cod-symbol_constant" :face font-lock-constant-face)
             (structure     nerd-icons-codicon "nf-cod-symbol_structure" :face font-lock-variable-name-face)
             (event         nerd-icons-codicon "nf-cod-symbol_event" :face font-lock-warning-face)
             (operator      nerd-icons-codicon "nf-cod-symbol_operator" :face font-lock-comment-delimiter-face)
             (template      nerd-icons-codicon "nf-cod-symbol_snippet" :face font-lock-type-face)))

         (defun my-lsp-icons-get-by-symbol-kind (kind &optional feature)
           (when (and kind
                      (lsp-icons--enabled-for-feature feature))
             (let* ((icon (cdr (assoc (lsp-treemacs-symbol-kind->icon kind) lsp-symbol-alist)))
                    (args (cdr icon)))
               (apply (car icon) args))))
         (advice-add #'lsp-icons-get-by-symbol-kind :override #'my-lsp-icons-get-by-symbol-kind)

         (setq lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                        :face 'lsp-headerline-breadcrumb-separator-face)))))

   (use-package lsp-ui
     :custom-face
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
     :pretty-hydra
     ((:title (pretty-hydra-title "LSP UI" 'faicon "nf-fa-rocket" :face 'nerd-icons-green)
       :color amaranth :quit-key ("q" "C-g"))
      ("Doc"
       (("d e" (progn
                 (lsp-ui-doc-enable (not lsp-ui-doc-mode))
                 (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
         "enable" :toggle lsp-ui-doc-mode)
        ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
         "signature" :toggle lsp-ui-doc-include-signature)
        ("d t" (setq lsp-ui-doc-position 'top)
         "top" :toggle (eq lsp-ui-doc-position 'top))
        ("d b" (setq lsp-ui-doc-position 'bottom)
         "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
        ("d p" (setq lsp-ui-doc-position 'at-point)
         "at point" :toggle (eq lsp-ui-doc-position 'at-point))
        ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
         "header" :toggle lsp-ui-doc-header)
        ("d f" (setq lsp-ui-doc-alignment 'frame)
         "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
        ("d w" (setq lsp-ui-doc-alignment 'window)
         "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
       "Sideline"
       (("s e" (progn
                 (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
                 (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
         "enable" :toggle lsp-ui-sideline-mode)
        ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
         "hover" :toggle lsp-ui-sideline-show-hover)
        ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
         "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
        ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
         "symbol" :toggle lsp-ui-sideline-show-symbol)
        ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
         "code actions" :toggle lsp-ui-sideline-show-code-actions)
        ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
         "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
       "Action"
       (("h" backward-char "←")
        ("j" next-line "↓")
        ("k" previous-line "↑")
        ("l" forward-char "→")
        ("C-a" mwim-beginning-of-code-or-line nil)
        ("C-e" mwim-end-of-code-or-line nil)
        ("C-b" backward-char nil)
        ("C-n" next-line nil)
        ("C-p" previous-line nil)
        ("C-f" forward-char nil)
        ("M-b" backward-word nil)
        ("M-f" forward-word nil)
        ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
     :bind (:map lsp-ui-mode-map
            ("M-<f6>" . lsp-ui-hydra/body)
            ("s-<return>" . lsp-ui-sideline-apply-code-actions)
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references))
     :hook ((lsp-mode . lsp-ui-mode)
            (after-load-theme . lsp-ui-set-doc-border))
     :init
     (setq lsp-ui-sideline-show-diagnostics nil
           lsp-ui-sideline-ignore-duplicate t
           lsp-ui-doc-delay 0.1
           lsp-ui-doc-show-with-cursor (not (display-graphic-p))
           lsp-ui-imenu-auto-refresh 'after-save
           lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                 ,(face-foreground 'font-lock-string-face)
                                 ,(face-foreground 'font-lock-constant-face)
                                 ,(face-foreground 'font-lock-variable-name-face)))
     ;; Set correct color to borders
     (defun lsp-ui-set-doc-border ()
       "Set the border color of lsp doc."
       (setq lsp-ui-doc-border
             (if (facep 'posframe-border)
                 (face-background 'posframe-border nil t)
               (face-background 'region nil t))))
     (lsp-ui-set-doc-border)
     (add-hook 'after-load-theme-hook #'lsp-ui-set-doc-border t)
     :config
     (with-no-warnings
       ;; Display peek in child frame if possible
       ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
       (defvar lsp-ui-peek--buffer nil)
       (defun lsp-ui-peek--peek-display (fn src1 src2)
         (if (childframe-workable-p)
             (-let* ((win-width (frame-width))
                     (lsp-ui-peek-list-width (/ (frame-width) 2))
                     (string (-some--> (-zip-fill "" src1 src2)
                               (--map (lsp-ui-peek--adjust win-width it) it)
                               (-map-indexed 'lsp-ui-peek--make-line it)
                               (-concat it (lsp-ui-peek--make-footer)))))
               (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
               (posframe-show lsp-ui-peek--buffer
                              :string (mapconcat 'identity string "")
                              :min-width (frame-width)
                              :internal-border-color (face-background 'posframe-border nil t)
                              :internal-border-width 1
                              :poshandler #'posframe-poshandler-frame-center))
           (funcall fn src1 src2)))
       (defun lsp-ui-peek--peek-destroy (fn)
         (if (childframe-workable-p)
             (progn
               (when (bufferp lsp-ui-peek--buffer)
                 (posframe-hide lsp-ui-peek--buffer))
               (setq lsp-ui-peek--last-xref nil))
           (funcall fn)))
       (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
       (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

       ;; Handle docs
       (defun my-lsp-ui-doc--handle-hr-lines nil
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
                             ;; :align-to is added with lsp-ui-doc--fix-hr-props
                             'display '(space :height (1))
                             'lsp-ui-doc--replace-hr t
                             'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
                 ;; :align-to is added here too
                 (propertize " " 'display '(space :height (1)))
                 (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
       (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))


   ;; `lsp-mode' and `treemacs' integration
   (use-package lsp-treemacs
     :after lsp-mode
     :bind (:map lsp-mode-map
            ("C-<f8>" . lsp-treemacs-errors-list)
            ("M-<f8>" . lsp-treemacs-symbols)
            ("s-<f8>" . lsp-treemacs-java-deps-list))
     :init (lsp-treemacs-sync-mode 1)
     :config
     (with-eval-after-load 'ace-window
       (when (boundp 'aw-ignored-buffers)
         (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
         (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers))))

   ;; Python
   (use-package lsp-pyright
     :functions lsp-pyright-format-buffer
     :hook (((python-mode python-ts-mode) . (lambda ()
                                              (require 'lsp-pyright)
                                              (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t))))
     :init
     (when (executable-find "basedpyright")
       (setq lsp-pyright-langserver-command "basedpyright"))
     (when (executable-find "python3")
       (setq lsp-pyright-python-executable-cmd "python3"))

     (defun lsp-pyright-format-buffer ()
       "Use `yapf' to format the buffer."
       (interactive)
       (when (and (executable-find "yapf") buffer-file-name)
         (call-process "yapf" nil nil nil "-i" buffer-file-name))))

   ;; Swift
   (use-package lsp-sourcekit)

   ;; Java
   (use-package lsp-java
     :hook ((java-mode java-ts-mode jdee-mode) . (lambda () (require 'lsp-java)))
     :config
     (setq lsp-java-workspace-dir (expand-file-name (concat dotfairy-local-dir "workspace/"))))))

(when dotfairy-lsp
  ;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang string)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                      "org-src-babel.tmp"))
           (pcase dotfairy-lsp
             ('eglot
              (when (fboundp 'eglot-ensure)
                (eglot-ensure)))
             ('lsp-mode
              (when (fboundp 'lsp-deferred)
                ;; Avoid headerline conflicts
                (setq-local lsp-headerline-breadcrumb-enable nil)
                (lsp-deferred)))
             (_
              (user-error "LSP:: invalid `dotfairy-lsp' type"))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      dotfairy-lsp (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defconst org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell")
    "The supported programming languages for interactive Babel.")
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang))))

(provide 'init-lsp)
;;; init-lsp.el ends here
