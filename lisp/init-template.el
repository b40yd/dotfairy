;;; init-template.el ---                                   -*- lexical-binding: t; -*-

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

;;
;; auto insert
;;
(require 'init-const)

(defun +file-templates--set (pred plist)
  (if (null (car-safe plist))
      (setq +file-templates-alist
            (delq (assoc pred +file-templates-alist)
                  +file-templates-alist))
    (push `(,pred ,@plist) +file-templates-alist)))

;;;###autodef
(defun set-file-template! (pred &rest plist)
  "Register a file template.

PRED can either be a regexp string or a major mode symbol. PLIST may contain
these properties:

  :when FUNCTION
    Provides a secondary predicate. This function takes the filename as an
    argument and is executed from within the target buffer. If it returns nil,
    this rule will be skipped over.
  :trigger STRING|FUNCTION
    If a string, this is the yasnippet trigger keyword used to trigger the
      target snippet.
    If a function, this function will be run in the context of the buffer to
      insert a file template into. It is given no arguments and must insert text
      into the current buffer manually.
    If omitted, `+file-templates-default-trigger' is used.
  :mode SYMBOL
    What mode to get the yasnippet snippet from. If omitted, either PRED (if
    it's a major-mode symbol) or the mode of the buffer is used.
  :project BOOL
    If non-nil, ignore this template if this buffer isn't in a project.
  :ignore BOOL
    If non-nil, don't expand any template for this file and don't test any other
    file template rule against this buffer.

\(fn PRED &key WHEN TRIGGER MODE PROJECT IGNORE)"
  (declare (indent defun))
  (defer-until! (boundp '+file-templates-alist)
    (+file-templates--set pred plist)))

;;;###autodef
(defun set-file-templates! (&rest templates)
  "Like `set-file-template!', but can register multiple file templates at once.

\(fn &rest (PRED &key WHEN TRIGGER MODE PROJECT IGNORE))"
  (defer-until! (boundp '+file-templates-alist)
    (dolist (template templates)
      (+file-templates--set (car template) (cdr template)))))


;;
;;; Library

;;;###autoload
(cl-defun +file-templates--expand (pred &key project mode trigger ignore _when)
  "Auto insert a yasnippet snippet into current file and enter insert mode (if
evil is loaded and enabled)."
  (when (and pred (not ignore))
    (when (if project (dotfairy-project-p) t)
      (unless mode
        (setq mode
              (if (and (symbolp pred) (not (booleanp pred)))
                  pred
                major-mode)))
      (unless mode
        (user-error "Couldn't determine mode for %s file template" pred))
      (unless trigger
        (setq trigger +file-templates-default-trigger))
      (if (functionp trigger)
          (funcall trigger)
        (require 'yasnippet)
        (unless yas-minor-mode
          (yas-minor-mode-on))
        (when (and yas-minor-mode
                   (when-let
                       (template (cl-find trigger (yas--all-templates (yas--get-snippet-tables mode))
                                          :key #'yas--template-key :test #'equal))
                     (yas-expand-snippet (yas--template-content template)))
                   (and (featurep 'evil) evil-local-mode)
                   (and yas--active-field-overlay
                        (overlay-buffer yas--active-field-overlay)
                        (overlay-get yas--active-field-overlay 'yas--field)))
          (evil-change-state 'insert))))))


;;
;;; Commands

;;;###autoload
(defun +file-templates/insert-license ()
  "Insert a license file template into the current file."
  (interactive)
  (require 'yasnippet)
  (unless (gethash 'text-mode yas--tables)
    (yas-reload-all t))
  (let ((templates
         (let (yas-choose-tables-first ; avoid prompts
               yas-choose-keys-first)
           (cl-loop for tpl in (yas--all-templates (yas--get-snippet-tables 'text-mode))
                    for uuid = (yas--template-uuid tpl)
                    if (string-prefix-p "__license-" uuid)
                    collect (cons (string-remove-prefix "__license-" uuid) tpl)))))
    (when-let (uuid (yas-choose-value (mapcar #'car templates)))
      (yas-expand-snippet (cdr (assoc uuid templates))))))

;;;###autoload
(defun +file-templates/debug ()
  "Tests the current buffer and outputs the file template rule most appropriate
for it. This is used for testing."
  (interactive)
  (cl-destructuring-bind (pred &rest plist &key trigger mode &allow-other-keys)
      (or (cl-find-if #'+file-template-p +file-templates-alist)
          (user-error "Found no file template for this file"))
    (if (or (functionp trigger)
            (cl-find trigger
                     (yas--all-templates
                      (yas--get-snippet-tables
                       mode))
                     :key #'yas--template-key :test #'equal))
        (message "Found %s" (cons pred plist))
      (message "Found rule, but can't find associated snippet: %s" (cons pred plist)))))


(defvar +file-templates-dir
  (expand-file-name "private/templates/" (file-name-directory dotfairy-emacs-dir))
  "The path to a directory of yasnippet folders to use for file templates.")

(defvar +file-templates-default-trigger "__"
  "The default yasnippet trigger key (a string) for file template rules that
don't have a :trigger property in `+file-templates-alist'.")

(defvar +file-templates-inhibit nil
  "If non-nil, inhibit file template expansion.")

(defvar +file-templates-alist
  '(;; General
    (gitignore-mode)
    (dockerfile-mode)
    ("/docker-compose\\.yml$" :mode yaml-mode)
    ("/Makefile$"             :mode makefile-gmake-mode)
    ;; elisp
    ("/\\.dir-locals\\.el$")
    (emacs-lisp-mode :trigger "__package")
    (snippet-mode)
    ;; C/C++
    ("/main\\.c\\(?:c\\|pp\\)$"   :trigger "__main.cpp"    :mode c++-mode)
    ("/win32_\\.c\\(?:c\\|pp\\)$" :trigger "__winmain.cpp" :mode c++-mode)
    ("\\.c\\(?:c\\|pp\\)$"        :trigger "__cpp" :mode c++-mode)
    ("\\.h\\(?:h\\|pp\\|xx\\)$"   :trigger "__hpp" :mode c++-mode)
    ("\\.h$" :trigger "__h" :mode c-mode)
    (c-mode  :trigger "__c")
    ;; direnv
    ("/\\.envrc$" :trigger "__envrc" :mode direnv-envrc-mode)
    ;; go
    ("/main\\.go$" :trigger "__main.go" :mode go-mode :project t)
    (go-mode :trigger "__.go")
    ;; web-mode
    ("/normalize\\.scss$" :trigger "__normalize.scss" :mode scss-mode)
    ("/master\\.scss$" :trigger "__master.scss" :mode scss-mode)
    ("\\.html$" :trigger "__.html" :mode web-mode)
    (scss-mode)
    ;; java
    ("/main\\.java$" :trigger "__main" :mode java-mode)
    ("/build\\.gradle$" :trigger "__build.gradle" :mode android-mode)
    ("/src/.+\\.java$" :mode java-mode)
    ;; javascript
    ("/package\\.json$"        :trigger "__package.json" :mode json-mode)
    ("/bower\\.json$"          :trigger "__bower.json" :mode json-mode)
    ("/gulpfile\\.js$"         :trigger "__gulpfile.js" :mode js-mode)
    ("/webpack\\.config\\.js$" :trigger "__webpack.config.js" :mode js-mode)
    ;; Lua
    ("/main\\.lua$" :trigger "__main.lua" :mode love-mode)
    ("/conf\\.lua$" :trigger "__conf.lua" :mode love-mode)
    ;; Markdown
    (markdown-mode)
    ;; Markdown
    (nxml-mode)
    ;; Nix
    ("/shell\\.nix$" :trigger "__shell.nix")
    (nix-mode)
    ;; Org
    (org-mode
     :trigger "__"
     :mode org-mode)
    (org-journal-mode :ignore t)
    (org-mode)
    ;; Python
    ;; TODO ("tests?/test_.+\\.py$" :trigger "__" :mode nose-mode)
    ;; TODO ("/setup\\.py$" :trigger "__setup.py" :mode python-mode)
    (python-mode)
    ;; Rust
    ("/Cargo\\.toml$" :trigger "__Cargo.toml" :mode rust-mode)
    ("/main\\.rs$" :trigger "__main.rs" :mode rust-mode)
    ;; Shell scripts
    ("\\.zunit$" :trigger "__zunit" :mode sh-mode)
    (fish-mode)
    (sh-mode)
    ;; Solidity
    (solidity-mode :trigger "__sol"))
  "An alist of file template rules. The CAR of each rule is either a major mode
symbol or regexp string. The CDR is a plist. See `set-file-template!' for more
information.")


(defun +file-templates-in-emacs-dirs-p (file)
  "Returns t if FILE is in Dotfairy or your private directory."
  (or (file-in-directory-p file dotfairy-user-dir)
      (file-in-directory-p file dotfairy-emacs-dir)))

(defun +file-template-p (rule)
  "Return t if RULE applies to the current buffer."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (or (and (symbolp pred)
                  (eq major-mode pred))
             (and (stringp pred)
                  (stringp buffer-file-name)
                  (string-match-p pred buffer-file-name)))
         (or (not (plist-member plist :when))
             (funcall (plist-get plist :when)
                      buffer-file-name))
         rule)))

(defun +file-templates-check-h ()
  "Check if the current buffer is a candidate for file template expansion. It
must be non-read-only, empty, and there must be a rule in
`+file-templates-alist' that applies to it."
  (and (not +file-templates-inhibit)
       buffer-file-name        ; this buffer represents a file and
       (not buffer-read-only)  ; ...isn't read-only
       (bobp) (eobp)           ; ...is empty
       (not (member (substring (buffer-name) 0 1) '("*" " ")))  ; ...isn't a "special" buffer
       (not (bound-and-true-p org-capture-current-plist))  ; ...isn't an org-capture buffer
       (not (file-exists-p buffer-file-name))  ; ...is a new file
       (not (buffer-modified-p))    ; ...hasn't been modified
       (null (buffer-base-buffer))  ; ...isn't an indirect clone
       (when-let (rule (cl-find-if #'+file-template-p +file-templates-alist))
         (apply #'+file-templates--expand rule))))


;;
;;; Bootstrap
(after! yasnippet
  (add-to-list 'yas-snippet-dirs '+file-templates-dir 'append #'eq)
  ;; Ensure file templates in `+file-templates-dir' are visible
  (yas-reload-all))

(defcustom dotfairy-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defun dotfairy-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'dotfairy-switch-buffer-hook)))

(add-hook 'dotfairy-switch-buffer-hook #'+file-templates-check-h) ;
;; Initialize `dorfairy-switch-buffer-hook'
(add-hook 'window-buffer-change-functions #'dotfairy-run-switch-buffer-hooks-h)
;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
(add-hook 'server-visit-hook #'dotfairy-run-switch-buffer-hooks-h)

(provide 'init-template)
;;; init-template.el ends here
