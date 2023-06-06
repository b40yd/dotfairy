;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 b40yd

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
(require 'init-keybinds)

(pcase dotfairy-lsp
  ('eglot
   (use-package eglot
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                             (eglot-ensure))))
            ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))))
  ('lsp-mode
   ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
   (setq read-process-output-max (* 1024 1024)) ;; 1MB
   (setenv "LSP_USE_PLISTS" "true")
   (use-package lsp-mode
     :ensure t
     :defines (lsp-diagnostics-disabled-modes
               lsp-clients-python-library-directories
               lsp-rust-server)

     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                             (lsp-deferred))))
            ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
            (lsp-mode . (lambda ()
                          ;; Integrate `which-key'
                          (lsp-enable-which-key-integration)

                          ;; Format and organize imports
                          (when (and dotfairy-lsp-format-on-save
                                     (not (apply #'derived-mode-p dotfairy-lsp-format-on-save-ignore-modes)))
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
     :autoload lsp-enable-which-key-integration
     :commands (lsp-format-buffer lsp-organize-imports)
     :bind (:map lsp-mode-map
            ;; ("C-c C-." . lsp-describe-thing-at-point)
            ([remap xref-find-definitions] . lsp-find-definition)
            ([remap xref-find-references] . lsp-find-references))

     :init
     (setq lsp-keymap-prefix "C-c l"
           lsp-keep-workspace-alive nil
           lsp-signature-auto-activate nil
           lsp-modeline-code-actions-enable nil
           lsp-modeline-diagnostics-enable nil
           lsp-modeline-workspace-status-enable nil
           lsp-eldoc-enable-hover nil
           lsp-signature-render-documentation nil
           lsp-headerline-breadcrumb-enable nil
           lsp-log-io nil
           lsp-idle-delay 0.500
           ;; For diagnostics
           lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

           lsp-clients-python-library-directories '("/usr/local/" "/usr/")

           lsp-semantic-tokens-enable t
           lsp-progress-spinner-type 'horizontal-breathing

           lsp-enable-file-watchers nil
           lsp-enable-folding nil
           ;; lsp-enable-symbol-highlighting nil
           lsp-enable-text-document-color nil

           lsp-enable-indentation nil
           lsp-enable-on-type-formatting nil
           lsp-session-file (concat dotfairy-etc-dir "lsp-session")
           lsp-server-install-dir (concat dotfairy-etc-dir "lsp/"))

     :config
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

       ;; Only display icons in GUI
       (defun my-lsp-icons-get-symbol-kind (fn &rest args)
         (and (icons-displayable-p) (apply fn args)))
       (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)

       (defun my-lsp-icons-get-by-file-ext (fn &rest args)
         (and (icons-displayable-p) (apply fn args)))
       (advice-add #'lsp-icons-get-by-file-ext :around #'my-lsp-icons-get-by-file-ext)

       (defun my-lsp-icons-get-by-file-ext (file-ext &optional feature)
         (when (and file-ext
                    (lsp-icons--enabled-for-feature feature))
           (nerd-icons-icon-for-extension file-ext)))
       (advice-add #'lsp-icons-get-by-file-ext :override #'my-lsp-icons-get-by-file-ext)
       (defvar lsp-symbol-alist
         '(
           (misc          nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-warning-face)
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
                                                      :face 'lsp-headerline-breadcrumb-separator-face))))
   ;;; Optionally: lsp-ui, company-lsp
   (use-package lsp-ui
     :ensure t
     :custom-face
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
     :pretty-hydra
     ((:title (pretty-hydra-title "LSP UI" 'faicon "nf-fa-rocket_launch" :face 'nerd-icons-green)
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
            ("C-<f6>" . lsp-ui-hydra/body)
            ("M-RET" . lsp-ui-sideline-apply-code-actions)
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references))
     :hook (lsp-mode . lsp-ui-mode)
     :init
     (setq lsp-ui-sideline-show-diagnostics nil
           lsp-ui-sideline-ignore-duplicate t
           lsp-ui-imenu-auto-refresh 'after-save
           lsp-ui-doc-delay 0.1)
     ;; Set correct color to borders
     (defun my-lsp-ui-doc-set-border ()
       "Set the border color of lsp doc."
       (setq lsp-ui-doc-border
             (if (facep 'posframe-border)
                 (face-background 'posframe-border nil t)
               (face-background 'region nil t))))
     (my-lsp-ui-doc-set-border)
     (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
     :config
     (with-no-warnings
       ;; Display peek in child frame if possible
       ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
       (defvar lsp-ui-peek--buffer nil)
       (defun lsp-ui-peek--peek-display (fn src1 src2)
         (if (childframe-completion-workable-p)
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
         (if (childframe-completion-workable-p)
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
                             'face `(:background ,(face-foreground 'font-lock-comment-face)))
                 ;; :align-to is added here too
                 (propertize " " 'display '(space :height (1)))
                 (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
       (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))

   ;; Ivy integration
   (pcase dotfairy-complete
     ('ivy
      (use-package lsp-ivy
        :after lsp-mode
        :ensure t
        :bind (:map lsp-mode-map
               ([remap xref-find-apropos] . lsp-ivy-workspace-symbol))))
     ('vertico
      (use-package consult-lsp
        :init
        (map! :map lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))))
   (use-package lsp-treemacs
     :after lsp-mode
     :bind (:map lsp-mode-map
            ("C-<f8>" . lsp-treemacs-errors-list)
            ("M-<f8>" . lsp-treemacs-symbols)
            ("s-<f8>" . lsp-treemacs-java-deps-list))
     ;; :init (lsp-treemacs-sync-mode 1)
     :config
     (with-eval-after-load 'ace-window
       (when (boundp 'aw-ignored-buffers)
         (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
         (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers))))

   ;; Python: pyright
   (use-package lsp-pyright
     :after lsp-mode
     :init
     (require 'lsp-pyright)
     ;; git clone https://github.com/microsoft/python-type-stubs $HOME/.emacs.d/.local/etc/python-type-stubs
     (let ((python-type-stubs (expand-file-name "python-type-stubs" dotfairy-etc-dir)))
       (if (not (file-exists-p python-type-stubs))
           (dotfairy-exec-process "git" "clone" "https://github.com/microsoft/python-type-stubs" python-type-stubs))
       (setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
       (setq lsp-pyright-stub-path python-type-stubs)))

   ;; Swift
   (use-package lsp-sourcekit)

   ;; Java
   (use-package lsp-java
     :hook (java-mode . (lambda () (require 'lsp-java)))
     :config
     (setq lsp-java-workspace-dir (expand-file-name (concat dotfairy-local-dir "workspace/"))))))

(when (memq dotfairy-lsp '(lsp-mode eglot))
  ;; Settings for company
  (use-package company
    :diminish
    :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
    :bind (("C-M-i" . company-complete)
           :map company-mode-map
           ("<backtab>" . company-yasnippet)
           :map company-active-map
           ("<backtab>" . my-company-yasnippet))
    :hook (after-init . global-company-mode)
    :init
    (setq company-tooltip-align-annotations t
          company-tooltip-limit 12
          company-idle-delay 0
          company-echo-delay (if (display-graphic-p) nil 0)
          company-minimum-prefix-length 1
          company-icon-margin 3
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-global-modes '(not erc-mode message-mode help-mode
                                     gud-mode eshell-mode shell-mode)
          company-backends '((company-capf :with company-yasnippet)
                             (company-dabbrev-code company-keywords company-files)
                             company-dabbrev))
    :config
    (with-no-warnings
      ;; Company anywhere
      ;; @see https://github.com/zk-phi/company-anywhere
      (defun company-anywhere-after-finish (completion)
        (when (and (stringp completion)
                   (looking-at "\\(?:\\sw\\|\\s_\\)+")
                   (save-match-data
                     (string-match (regexp-quote (match-string 0)) completion)))
          (delete-region (match-beginning 0) (match-end 0))))
      (add-hook 'company-after-completion-hook 'company-anywhere-after-finish)

      (defun company-anywhere-grab-word (_)
        (buffer-substring (point) (save-excursion (skip-syntax-backward "w") (point))))
      (advice-add 'company-grab-word :around 'company-anywhere-grab-word)

      (defun company-anywhere-grab-symbol (_)
        (buffer-substring (point) (save-excursion (skip-syntax-backward "w_") (point))))
      (advice-add 'company-grab-symbol :around 'company-anywhere-grab-symbol)

      (defun company-anywhere-dabbrev-prefix (_)
        (company-grab-line (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)" company-dabbrev-char-regexp) 1))
      (advice-add 'company-dabbrev--prefix :around 'company-anywhere-dabbrev-prefix)

      (defun company-anywhere-capf (fn command &rest args)
        (if (eq command 'prefix)
            (let ((res (company--capf-data)))
              (when res
                (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
                      (prefix (buffer-substring-no-properties (nth 1 res) (point))))
                  (cond
                   (length (cons prefix length))
                   (t prefix)))))
          (apply fn command args)))
      (advice-add 'company-capf :around 'company-anywhere-capf)

      (defun company-anywhere-preview-show-at-point (pos completion)
        (when (and (save-excursion
                     (goto-char pos)
                     (looking-at "\\(?:\\sw\\|\\s_\\)+"))
                   (save-match-data
                     (string-match (regexp-quote (match-string 0)) completion)))
          (move-overlay company-preview-overlay (overlay-start company-preview-overlay) (match-end 0))
          (let ((after-string (overlay-get company-preview-overlay 'after-string)))
            (when after-string
              (overlay-put company-preview-overlay 'display after-string)
              (overlay-put company-preview-overlay 'after-string nil)))))
      (advice-add 'company-preview-show-at-point :after 'company-anywhere-preview-show-at-point)

      ;; `yasnippet' integration
      (with-eval-after-load 'yasnippet
        (defun my-company-yasnippet ()
          "Hide the current completeions and show snippets."
          (interactive)
          (company-cancel)
          (call-interactively 'company-yasnippet))

        (defun company-backend-with-yas (backend)
          "Add `yasnippet' to company backend."
          (if (and (listp backend) (member 'company-yasnippet backend))
              backend
            (append (if (consp backend) backend (list backend))
                    '(:with company-yasnippet))))

        (defun my-company-enbale-yas (&rest _)
          "Enable `yasnippet' in `company'."
          (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

        (defun my-lsp-fix-company-capf ()
          "Remove redundant `comapny-capf'."
          (setq company-backends
                (remove 'company-backends (remq 'company-capf company-backends))))
        (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

        (defun my-company-yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
          "Enable yasnippet but disable it inline."
          (if (eq cmd  'prefix)
              (when-let ((prefix (funcall fn 'prefix)))
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
              (funcall fn cmd arg))))
        (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))

    ;; Better sorting and filtering
    (use-package prescient
      :commands prescient-persist-mode
      :init
      (setq prescient-save-file
            (expand-file-name "var/prescient-save.el" dotfairy-cache-dir))
      (prescient-persist-mode 1))

    (use-package company-prescient
      :init (company-prescient-mode 1))

    ;; Icons and quickhelp
    (use-package company-box
      :diminish
      :bind (:map company-active-map
             ([remap company-show-doc-buffer] . company-box-doc-manually))
      :hook (company-mode . company-box-mode)
      :init (setq company-box-enable-icon t
                  company-box-backends-colors nil
                  company-box-doc-delay 0.1)
      :config
      (with-no-warnings
        ;; Prettify icons
        (defun my-company-box-icons--elisp (candidate)
          (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
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

        (setq company-box-doc-frame-parameters '((vertical-scroll-bars . nil)
                                                 (horizontal-scroll-bars . nil)
                                                 (internal-border-width . 1)
                                                 (left-fringe . 8)
                                                 (right-fringe . 8)))

        (defun my-company-box-doc--make-buffer (object)
          (let* ((buffer-list-update-hook nil)
                 (inhibit-modification-hooks t)
                 (string (cond ((stringp object) object)
                               ((bufferp object) (with-current-buffer object (buffer-string))))))
            (when (and string (length> (string-trim string) 0))
              (with-current-buffer (company-box--get-buffer "doc")
                (erase-buffer)
                (insert (propertize "\n" 'face '(:height 0.5)))
                (insert string)
                (insert (propertize "\n\n" 'face '(:height 0.5)))

                ;; Handle hr lines of markdown
                ;; @see `lsp-ui-doc--handle-hr-lines'
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
                                    'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
                        (propertize " " 'display '(space :height (1)))
                        (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5))))))))

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
                  ;; (box-border-width (frame-border-width box-frame))

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
            (set-frame-size frame text-width text-height t)))
        (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position)

        (when (icons-displayable-p)
          (defvar company-box-icons-nerd
            `((Unknown       . ,(nerd-icons-codicon "nf-cod-symbol_namespace"))
              (Text          . ,(nerd-icons-codicon "nf-cod-symbol_string"))
              (Method        . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
              (Function      . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
              (Constructor   . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-lpurple))
              (Field         . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'nerd-icons-lblue))
              (Variable      . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'nerd-icons-lblue))
              (Class         . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'nerd-icons-orange))
              (Interface     . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'nerd-icons-lblue))
              (Module        . ,(nerd-icons-codicon "nf-cod-symbol_namespace" :face 'nerd-icons-lblue))
              (Property      . ,(nerd-icons-codicon "nf-cod-symbol_property"))
              (Unit          . ,(nerd-icons-codicon "nf-cod-symbol_key"))
              (Value         . ,(nerd-icons-codicon "nf-cod-symbol_numeric" :face 'nerd-icons-lblue))
              (Enum          . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'nerd-icons-orange))
              (Keyword       . ,(nerd-icons-codicon "nf-cod-symbol_keyword"))
              (Snippet       . ,(nerd-icons-codicon "nf-cod-symbol_snippet"))
              (Color         . ,(nerd-icons-codicon "nf-cod-symbol_color"))
              (File          . ,(nerd-icons-codicon "nf-cod-symbol_file"))
              (Reference     . ,(nerd-icons-codicon "nf-cod-symbol_misc"))
              (Folder        . ,(nerd-icons-codicon "nf-cod-folder"))
              (EnumMember    . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'nerd-icons-lblue))
              (Constant      . ,(nerd-icons-codicon "nf-cod-symbol_constant"))
              (Struct        . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'nerd-icons-orange))
              (Event         . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'nerd-icons-orange))
              (Operator      . ,(nerd-icons-codicon "nf-cod-symbol_operator"))
              (TypeParameter . ,(nerd-icons-codicon "nf-cod-symbol_class"))
              (Template      . ,(nerd-icons-codicon "nf-cod-symbol_snippet"))))
          (setq company-box-icons-alist 'company-box-icons-nerd)
          ))))

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
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang))))

(provide 'init-lsp)
;;; init-lsp.el ends here
