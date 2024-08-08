;;; init-vertico.el --                                 -*- lexical-binding: t; -*-

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
(require 'init-const)
(require 'init-funcs)


(use-package vertico
  :commands (+vertico/embark-preview)
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))

  :init
  (defadvice! +vertico-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))


  ;;;###autoload
  (defvar embark-quit-after-action)
  (defun +vertico/embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (if (fboundp 'embark-dwim)
          (save-selected-window
            (let (embark-quit-after-action)
              (embark-dwim)))
        (user-error "Embark not installed, aborting..."))))

;;;###autoload
  (defun +vertico/enter-or-preview ()
    "Enter directory or embark preview on current candidate."
    (interactive)
    (when (> 0 vertico--index)
      (user-error "No vertico session is currently active"))
    (if (and (let ((cand (vertico--candidate)))
               (or (string-suffix-p "/" cand)
                   (and (vertico--remote-p cand)
                        (string-suffix-p ":" cand))))
             (not (equal vertico--base ""))
             (eq 'file (vertico--metadata-get 'category)))
        (vertico-insert)
      (condition-case _
          (+vertico/embark-preview)
        (user-error (vertico-directory-enter)))))


  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! +vertico--suppress-completion-help-a (fn &rest args)
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args)))

  (map! :map vertico-map
    "M-RET" #'vertico-exit-input
    "M-." #'+vertico/embark-preview
    "C-j"   #'vertico-next
    "C-M-j" #'vertico-next-group
    "C-k"   #'vertico-previous
    "C-M-k" #'vertico-previous-group
    "C-h" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
    "C-l" (cmds! (eq 'file (vertico--metadata-get 'category)) #'+vertico/enter-or-preview)))

(use-package orderless
  :ensure t
  :config

  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Annotation
     ((string-prefix-p "&" pattern) `(orderless-annotation . ,(substring pattern 1)))
     ((string-suffix-p "&" pattern) `(orderless-annotation . ,(substring pattern 0 -1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

  (defun +vertico-basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  (defun +vertico-basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator #'orderless-escapable-split-on-space)
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;; Support Pinyin
(use-package pinyinlib
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(when (childframe-completion-workable-p)
  (use-package vertico-posframe
    :hook (vertico-mode . vertico-posframe-mode)
    :init (setq vertico-posframe-poshandler
                #'posframe-poshandler-frame-center-near-bottom
                vertico-posframe-parameters
                '((left-fringe  . 8)
                  (right-fringe . 8)))))

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (pushnew! marginalia-command-categories
            '(persp-switch-to-buffer . buffer)))

(use-package consult
  :defer t
  :bind (
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)
         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g"   . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l"   . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L"   . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :commands (+vertico/consult-fd-or-find)
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap goto-line]                     #'consult-goto-line
    [remap Info-search]                   #'consult-info
    [remap isearch-forward]               #'consult-line
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop)
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (defvar dotfairy-projects--fd-version nil)
  (require 'orderless nil t)
  (if IS-WINDOWS
      (progn
        (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
        (add-to-list 'process-coding-system-alist '("explorer" gbk . gbk))
        (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))



  (autoload 'consult--multi "consult")

  ;;;###autoload
  (defun +vertico/consult-fd-or-find (&optional dir initial)
    "Runs consult-fd if fd version > 8.6.0 exists, consult-find otherwise.
See URL `https://github.com/minad/consult/issues/770'."
    (interactive "P")
    ;; TODO this condition was adapted from a similar one in lisp/doom-projects.el, to be replaced with a more robust check post v3
    (if (when-let*
            ((bin (cl-find-if (dotfairy-rpartial #'executable-find t)
                              (list "fdfind" "fd")))
             (version (with-memoization dotfairy-projects--fd-version
                        (cadr (split-string (cdr (dotfairy-call-process bin "--version"))
                                            " " t))))
             ((ignore-errors (version-to-list version))))
          ;; TODO remove once fd 8.6.0 is widespread enough to be the minimum version for doom
          (version< "8.6.0" version))
        (consult-fd dir initial)
      (consult-find dir initial)))

  (defadvice! +vertico--consult-recent-file-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before (list #'consult-recent-file #'consult-buffer)
    (recentf-mode +1))

  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-fd-args
        '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
          "--color=never"
          ;; https://github.com/sharkdp/fd/issues/839
          "--full-path --absolute-path"
          "--hidden --exclude .git"
          (if (featurep :system 'windows) "--path-separator=/")))

  (consult-customize
   consult-line consult-line-multi :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 1.0 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   consult-theme
   :initial (thing-at-point 'symbol t)
   :preview-key (list "C-SPC" :debounce 0.5 'any))

  (when (featurep 'org)
    (defvar +vertico--consult-org-source
      (list :name     "Org Buffer"
            :category 'buffer
            :narrow   ?o
            :hidden   t
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :new
            (lambda (name)
              (with-current-buffer (get-buffer-create name)
                (insert "#+title: " name "\n\n")
                (org-mode)
                (consult--buffer-action (current-buffer))))
            :items
            (lambda ()
              (mapcar #'buffer-name
                      (if (featurep 'org)
                          (org-buffer-list)
                        (seq-filter
                         (lambda (x)
                           (eq (buffer-local-value 'major-mode x) 'org-mode))
                         (buffer-list)))))))
    (add-to-list 'consult-buffer-sources '+vertico--consult-org-source 'append)))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  ;; TODO: Replace with `tramp-container--completion-function' when we drop
  ;;   support for <29
  (defun +vertico--consult-dir-container-hosts (host)
    "Get a list of hosts from HOST."
    (cl-loop for line in (cdr
                          (ignore-errors
                            (apply #'process-lines +vertico-consult-dir-container-executable
                                   (append +vertico-consult-dir-container-args (list "ps")))))
             for cand = (split-string line "[[:space:]]+" t)
             collect (format "/%s:%s:/" host (car (last cand)))))

  (defun +vertico--consult-dir-podman-hosts ()
    (let ((+vertico-consult-dir-container-executable "podman"))
      (+vertico--consult-dir-container-hosts "podman")))

  (defun +vertico--consult-dir-docker-hosts ()
    (let ((+vertico-consult-dir-container-executable "docker"))
      (+vertico--consult-dir-container-hosts "docker")))

  (defvar +vertico--consult-dir-source-tramp-podman
    `(:name     "Podman"
      :narrow   ?p
      :category file
      :face     consult-file
      :history  file-name-history
      :items    ,#'+vertico--consult-dir-podman-hosts)
    "Podman candidate source for `consult-dir'.")

  (defvar +vertico--consult-dir-source-tramp-docker
    `(:name     "Docker"
      :narrow   ?d
      :category file
      :face     consult-file
      :history  file-name-history
      :items    ,#'+vertico--consult-dir-docker-hosts)
    "Docker candidate source for `consult-dir'.")

  (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-podman t)
  (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-docker t)

  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

(use-package consult-flyspell
  :bind ("M-g s" . consult-flyspell))

(use-package consult-yasnippet
  :bind ("M-g y" . consult-yasnippet)
  :init (map! [remap yas-insert-snippet] #'consult-yasnippet))

(use-package embark
  :defer t
  :commands (+vertico/embark-export-write)
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command 'embark-prefix-help-command)
  (map! [remap describe-bindings] #'embark-bindings
        "C-c ;"               #'embark-act  ; to be moved to :config default if accepted
        (:map minibuffer-local-map
         "C-c ;"               #'embark-act
         "C-c C-;"           #'embark-export
         "C-c C-l"           #'embark-collect
         :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write)
        (:leader
         :desc "Actions" "a" #'embark-act)) ; to be moved to :config default if accepted
  :config
  (require 'consult)
  (defadvice! +vertico--embark-which-key-prompt-a (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    :around #'embark-completing-read-prompter
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (defun +vertico-embark-target-package-fn ()
    "Targets dotfairy's package! statements and returns the package name"
    (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'org-mode))
      (save-excursion
        (when (and (search-backward "(" nil t)
                   (looking-at "(\\s-*package!\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*"))
          (let ((pkg (match-string 1)))
            (set-text-properties 0 (length pkg) nil pkg)
            `(package . ,pkg))))))
  (defun +vertico-embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators)
  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  (let ((pos (or (cl-position
                  'embark-target-file-at-point
                  embark-target-finders)
                 (length embark-target-finders))))
    (cl-callf2
        cons
        '+vertico-embark-target-package-fn
        (nthcdr pos embark-target-finders)))
  ;; export write
  (use-package embark-consult
    :ensure t
    :after (embark consult)
    :demand
    :config
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

  (defun +vertico/embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (let* ((edit-command
            (pcase-let ((`(,type . ,candidates)
                         (run-hook-with-args-until-success 'embark-candidate-collectors)))
              (pcase type
                ('consult-grep #'wgrep-change-to-wgrep-mode)
                ('file #'wdired-change-to-wdired-mode)
                ('consult-location #'occur-edit-mode)
                (x (user-error "embark category %S doesn't support writable export" x)))))
           (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
      (embark-export)))

  (defun +vertico/embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))

  (map! (:map embark-file-map
         :desc "Open target with sudo"        "s"   #'dotfairy/sudo-find-file
         (:when (featurep 'magit)
          :desc "Open magit-status of target" "g"   #'+vertico/embark-magit-status))))


;;;###autoload
(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.
:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((proj-root (or (project-root (project-current)) default-directory))
         (directory (or in proj-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--path-separator /   --smart-case --no-heading "
                  "--with-filename --line-number --search-zip "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'shell-quote-argument args " ")))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query (or query
                    (when (dotfairy-region-active-p)
                      (regexp-quote (dotfairy-thing-at-point-or-region)))))
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial _function)
          (consult--async-split-style)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                     :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt #'consult--ripgrep-make-builder directory query)))

(defun +vertico/project-search (&optional arg initial-query directory)
  "Peforms a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

(defun +vertico/project-search-from-cwd (&optional arg initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+vertico/project-search arg initial-query default-directory))

(defun +vertico/search-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(provide 'init-vertico)
;;; init-vertico.el ends here
