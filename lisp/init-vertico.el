;;; init-vertico.el --                                 -*- lexical-binding: t; -*-

;; Copyright © 2022, 7ym0n, all rights reserved.

;; Author: 7ym0n <bb.qnyd@gmail.com>
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
(require 'init-keybinds)


(use-package vertico
  :hook (after-init . vertico-mode)
  :commands (+vertico/embark-preview)
  :config
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

  (defvar +vertico/find-file-in--history nil)
;;;###autoload
  (defun +vertico/find-file-in (&optional dir initial)
    "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
    (interactive)
    (require 'consult)
    (let* ((default-directory (or dir default-directory))
           (prompt-dir (consult--directory-prompt "Find" default-directory))
           (cmd (split-string-and-unquote +vertico-consult-fd-args " ")))
      (find-file
       (consult--read
        (split-string (cdr (apply #'dotfairy-call-process cmd)) "\n" t)
        :prompt default-directory
        :sort nil
        :initial (if initial (shell-quote-argument initial))
        :add-history (thing-at-point 'filename)
        :category 'file
        :history '(:input +vertico/find-file-in--history)))))

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
      (apply fn args))))

(use-package orderless
  :ensure t
  :config
  (defvar +vertico-company-completion-styles '(basic partial-completion orderless)
    "Completion styles for company to use.
The completion/vertico module uses the orderless completion style by default,
but this returns too broad a candidate set for company completion. This variable
overrides `completion-styles' during company completion sessions.")

  (defadvice! +vertico--company-capf--candidates-a (fn &rest args)
    "Highlight company matches correctly, and try default completion styles before
orderless."
    :around #'company-capf--candidates
    (let ((orderless-match-faces [completions-common-part])
          (completion-styles +vertico-company-completion-styles))
      (apply fn args)))

  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
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
        orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package consult
  :defer t
  :bind (("C-s" . consult-line))
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop)
  :config
  (require 'orderless nil t)
  (if IS-WINDOWS
      (progn
        (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
        (add-to-list 'process-coding-system-alist '("explorer" gbk . gbk))
        (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))



  (autoload 'consult--multi "consult")
  :config
  (defvar +vertico-consult-fd-args nil
    "Shell command and arguments the vertico module uses for fd.")

  (defadvice! +vertico--consult-recent-file-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))

  (setq consult-project-root-function #'dotfairy-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  (unless +vertico-consult-fd-args
    (setq +vertico-consult-fd-args
          (if dotfairy-projectile-fd-binary
              (format "%s --color=never -i -H -E .git --regex %s"
                      dotfairy-projectile-fd-binary
                      (if IS-WINDOWS "--path-separator=/" ""))
            consult-find-args)))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")
  (consult-customize
   consult-theme
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
  :bind (([remap list-directory] . consult-dir))
  :config
  (defun +vertico--consult-dir-docker-hosts ()
    "Get a list of hosts from docker."
    (when (if (>= emacs-major-version 29)
              (require 'tramp-container nil t)
            (setq-local docker-tramp-use-names t)
            (require 'docker-tramp nil t))
      (let ((hosts)
            (docker-query-fn #'docker-tramp--parse-running-containers))
        (when (>= emacs-major-version 29)
          (setq docker-query-fn #'tramp-docker--completion-function))
        (dolist (cand (funcall docker-query-fn))
          (let ((user (unless (string-empty-p (car cand))
                        (concat (car cand) "@")))
                (host (car (cdr cand))))
            (push (concat "/docker:" user host ":/") hosts)))
        hosts)))
  (defvar +vertico--consult-dir-source-tramp-docker
    `(:name     "Docker"
      :narrow   ?d
      :category file
      :face     consult-file
      :history  file-name-history
      :items    ,#'+vertico--consult-dir-docker-hosts)
    "Docker candiadate source for `consult-dir'.")

  (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-docker t)

  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

(use-package consult-flycheck
  :after (consult flycheck))


(use-package embark
  :defer t
  :commands (+vertico/embark-export-write)
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-c ;" . embark-act)
         :map minibuffer-local-map
         ("C-c ;" . embark-act)
         ("C-c C-l" . embark-export)
         ("C-c C-e" . +vertico/embark-export-write))
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
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

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (advice-add #'marginalia--project-root :override #'dotfairy-project-root)
  (pushnew! marginalia-command-categories
            '(+default/find-file-under-here . file)
            '(dotfairy/find-file-in-emacsd . project-file)
            '(dotfairy/find-file-in-other-project . project-file)
            '(flycheck-error-list-set-filter . builtin)
            '(persp-switch-to-buffer . buffer)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))

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
  (let* ((project-root (or (dotfairy-project-root) default-directory))
         (directory (or in project-root))
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
