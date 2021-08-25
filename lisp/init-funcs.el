;;; init-funcs.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n.q6e, all rights reserved.

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
(require 'cl-lib)
(require 'init-const)
(require 'init-custom)
(require 'init-proxy)
(require 'init-files)

(defun dotfairy--theme-name (theme)
  "Return internal THEME name."
  (or (alist-get theme dotfairy-theme-alist) theme 'doom-one))

(defun dotfairy--load-theme (theme)
  "Disable others and enable new one."
  (when theme
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (message "Loaded theme `%s'" theme)))

(defun dotfairy-load-theme (theme &optional no-save)
  "Load color THEME. Save to `custom-file' if NO-SAVE is nil."
  ;; Set option
  (dotfairy-set-variable 'dotfairy-theme theme no-save)
  (dotfairy--load-theme (dotfairy--theme-name theme)))

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and display-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(defun make-custom-directory()
  "Create custom directory."
  (if (not (file-directory-p dotfairy-local-dir))
      (make-directory dotfairy-local-dir))
  (if (not (file-directory-p dotfairy-org-dir))
      (make-directory dotfairy-org-dir))
  (if (not (file-directory-p (concat dotfairy-org-dir "roam/")))
      (make-directory (concat dotfairy-org-dir "roam/")))
  (if (not (file-directory-p dotfairy-etc-dir))
      (make-directory dotfairy-etc-dir))
  (if (not (file-directory-p dotfairy-private-dir))
      (make-directory dotfairy-private-dir))
  (if (not (file-directory-p dotfairy-cache-dir))
      (make-directory dotfairy-cache-dir))
  )

(defun dotfairy-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.
Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).
REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern (completing-read "Select package archives: "
                             (mapcar #'car dotfairy-package-archives-alist)))))
  ;; Set option
  (dotfairy-set-variable 'dotfairy-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'dotfairy-set-package-archives #'set-package-archives)

;; Refer to https://emacs-china.org/t/elpa/11192
(defun dotfairy-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.
Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((urls (mapcar
                (lambda (url)
                  (concat url "archive-contents"))
                (mapcar #'cdr
                        (mapcar #'cadr
                                (mapcar #'cdr
                                        dotfairy-package-archives-alist)))))
         (durations (mapcar
                     (lambda (url)
                       (let ((start (current-time)))
                         (message "Fetching %s..." url)
                         (cond ((executable-find "curl")
                                (call-process "curl" nil nil nil "--max-time" "10" url))
                               ((executable-find "wget")
                                (call-process "wget" nil nil nil "--timeout=10" url))
                               (t (user-error "curl or wget is not found")))
                         (float-time (time-subtract (current-time) start))))
                     urls))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            dotfairy-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'horizontal
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (url) (url-host (url-generic-parse-url url))) urls) "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "%s" urls)
    (message "%s" durations)
    (message "%s is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;;###autoload
(defun dotfairy/toggle-indent-style ()
  "Switch between tabs and spaces indentation style in the current buffer."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))

(defvar editorconfig-lisp-use-default-indent)
;;;###autoload
(defun dotfairy/set-indent-width (width)
  "Change the indentation size to WIDTH of the current buffer.
The effectiveness of this command is significantly improved if you have
editorconfig or dtrt-indent installed."
  (interactive
   (list (if (integerp current-prefix-arg)
             current-prefix-arg
           (read-number "New indent size: "))))
  (setq tab-width width)
  (setq-local standard-indent width)
  (when (boundp 'evil-shift-width)
    (setq evil-shift-width width))
  (cond ((require 'editorconfig nil t)
         (let (editorconfig-lisp-use-default-indent)
           (editorconfig-set-indentation nil width)))
        ((require 'dtrt-indent nil t)
         (when-let (var (nth 2 (assq major-mode dtrt-indent-hook-mapping-list)))
           (set var width))))
  (message "Changed indentation to %d" width))

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function and macros in BODY.
Intended as a simpler version of `cl-letf' and `cl-macrolet'.
BINDINGS is either a) a list of, or a single, `defun' or `defmacro'-ish form, or
b) a list of (PLACE VALUE) bindings as `cl-letf*' would accept.
TYPE is either `defun' or `defmacro'. NAME is the name of the function. If an
original definition for NAME exists, it can be accessed as a lexical variable by
the same name, for use with `funcall' or `apply'. ARGLIST and BODY are as in
`defun'.
\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defmacro))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) (macroexpand body))
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defun `(cl-letf* ((,(car rest) (symbol-function #',(car rest)))
                                  ((symbol-function #',(car rest))
                                   (lambda ,(cadr rest) ,@(cddr rest))))
                         (ignore ,(car rest))
                         ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

;;;###autoload
(defun +default/new-buffer ()
  "TODO"
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (funcall (default-value 'major-mode)))))


(defun dotfairy-region-active-p ()
  "Return non-nil if selection is active."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

;;;###autoload
(defun dotfairy-region-beginning ()
  "Return beginning position of selection."
  (declare (side-effect-free t))
  (region-beginning))

;;;###autoload
(defun dotfairy-region-end ()
  "Return end position of selection."
  (declare (side-effect-free t))
  (region-end))

;;;###autoload
(defun dotfairy-thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.
Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.
NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((dotfairy-region-active-p)
         (buffer-substring-no-properties
          (dotfairy-region-beginning)
          (dotfairy-region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (if (memq (xref-find-backend) '(eglot elpy nox))
             (thing-at-point 'symbol t)
           ;; A little smarter than using `symbol-at-point', though in most
           ;; cases, xref ends up using `symbol-at-point' anyway.
           (xref-backend-identifier-at-point (xref-find-backend))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))


(defun dotfairy-set-prettify (prettify-alist)
  "Set up symbol prettification."
  (progn
    (let ((prettify-setup (lambda (alist)
                            (dolist (x alist nil)
                              (push x prettify-symbols-alist))
                            )))
      (funcall prettify-setup dotfairy-prettify-symbols-alist)
      (funcall prettify-setup prettify-alist))
    ;; When you get to the right edge, it goes back to how it normally prints
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    )
  (prettify-symbols-mode)
  )

(provide 'init-funcs)
;;; init-funcs.el ends here
