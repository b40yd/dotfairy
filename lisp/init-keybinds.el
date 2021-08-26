;;; init-keybinds.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2021, 7ym0n, all rights reserved.

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
(require 'init-custom)
(require 'init-funcs)

(defvar dotfairy-leader-key "SPC"
  "The leader prefix key for users.")

(defvar dotfairy-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for users.")

(defvar dotfairy-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar dotfairy-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar dotfairy-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

;; Key Modifiers
(with-no-warnings
  (cond
   (IS-MAC
    ;; mac-* variables are used by the special emacs-mac build of Emacs by
    ;; Yamamoto Mitsuharu, while other builds use ns-*.
    (setq mac-command-modifier      'super
          ns-command-modifier       'super
          mac-option-modifier       'meta
          ns-option-modifier        'meta
          ;; Free up the right option for character composition
          mac-right-option-modifier 'none
          ns-right-option-modifier  'none))
   (IS-WINDOWS
    (setq w32-pass-lwindow-to-system nil
          w32-pass-rwindow-to-system nil
          w32-pass-apps-to-system nil
          w32-lwindow-modifier 'super
          w32-apps-modifier 'hyper)
    (w32-register-hot-key [s-t]))))

(use-package general
  :init
  (general-auto-unbind-keys)
  ;; Convenience aliases
  (defalias 'define-key! #'general-def)
  (defalias 'undefine-key! #'general-unbind))


(defun dotfairy-rpartial (fn &rest args)
  "Return a partial application of FUN to right-hand ARGS.
ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun dotfairy-lookup-key (keys &rest keymaps)
  "Like `lookup-key', but search active keymaps if KEYMAP is omitted."
  (if keymaps
      (cl-some (dotfairy-rpartial #'lookup-key keys) keymaps)
    (cl-loop for keymap
             in (append (cl-loop for alist in emulation-mode-map-alists
                                 append (mapcar #'cdr
                                                (if (symbolp alist)
                                                    (if (boundp alist) (symbol-value alist))
                                                  alist)))
                        (list (current-local-map))
                        (mapcar #'cdr minor-mode-overriding-map-alist)
                        (mapcar #'cdr minor-mode-map-alist)
                        (list (current-global-map)))
             if (keymapp keymap)
             if (lookup-key keymap keys)
             return it)))

(defun dotfairy-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun dotfairy-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun dotfairy-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defun dotfairy--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.
If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (dotfairy-enlist (dotfairy-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun dotfairy--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (dotfairy--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "dotfairy--setq-%s-for-%s-h"
                                          var mode))))))


(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.
This macro accepts, in order:
  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).
\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (dotfairy--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'dotfairy-unquote rest)
                     (dotfairy-enlist (dotfairy-unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.
Takes the same arguments as `add-hook!'.
If N and M = 1, there's no benefit to using this macro over `remove-hook'.
\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.
\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (dotfairy--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(remove-hook ',hook #',fn) ; ensure set order
            collect `(add-hook ',hook #',fn))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.
\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (dotfairy--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))


;;; Definers
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (dotfairy-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.
This has the same signature as `defadvice!' an exists as an easy undefiner when
testing advice (when combined with `rotate-text').
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (dotfairy-enlist ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

(defvar dotfairy-disabled-packages ()
  "A list of packages that should be ignored by `use-package!' and `after!'.")

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.
PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:
- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.
This is a wrapper around `eval-after-load' that:
1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p dotfairy-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              ;; We intentionally avoid `with-eval-after-load' to prevent eager
              ;; macro expansion from pulling (or failing to pull) in autoloaded
              ;; macros/packages.
              `(eval-after-load ',package ',(macroexp-progn body))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))


;; HACK `map!' uses this instead of `define-leader-key!' because it consumes
;; 20-30% more startup time, so we reimplement it ourselves.
(defmacro dotfairy--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (dotfairy-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key dotfairy-leader-map (general--kbd ,key)
                       ,bdef)
                    forms))
            (when-let (desc (cadr (memq :which-key udef)))
              (prependq!
               wkforms `((which-key-add-key-based-replacements
                           (general--concat t dotfairy-leader-alt-key ,key)
                           ,desc)
                         (which-key-add-key-based-replacements
                           (general--concat t dotfairy-leader-key ,key)
                           ,desc))))))))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.
Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.
See `dotfairy-leader-key' and `dotfairy-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'dotfairy-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.
Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.
See `dotfairy-localleader-key' and `dotfairy-localleader-alt-key' to change the
localleader prefix."
  (if (featurep 'evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix dotfairy-localleader-key
        :non-normal-prefix dotfairy-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix dotfairy-localleader-alt-key
      ,@args)))

;; We use a prefix commands instead of general's :prefix/:non-normal-prefix
;; properties because general is incredibly slow binding keys en mass with them
;; in conjunction with :states -- an effective doubling of Dotfairy's startup time!
(define-prefix-command 'dotfairy/leader 'dotfairy-leader-map)
(define-key dotfairy-leader-map [override-state] 'all)

;; Bind `dotfairy-leader-key' and `dotfairy-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'after-init-hook
           (defun dotfairy-init-leader-keys-h ()
             "Bind `dotfairy-leader-key' and `dotfairy-leader-alt-key'."
             (let ((map general-override-mode-map))
               (if (not (featurep 'evil))
                   (progn
                     (cond ((equal dotfairy-leader-alt-key "C-c")
                            (set-keymap-parent dotfairy-leader-map mode-specific-map))
                           ((equal dotfairy-leader-alt-key "C-x")
                            (set-keymap-parent dotfairy-leader-map ctl-x-map)))
                     (define-key map (kbd dotfairy-leader-alt-key) 'dotfairy/leader))
                 (evil-define-key* '(normal visual motion) map (kbd dotfairy-leader-key) 'dotfairy/leader)
                 (evil-define-key* '(emacs insert) map (kbd dotfairy-leader-alt-key) 'dotfairy/leader))
               (general-override-mode +1))))


;;
;;; Packages
(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  (which-key-mode)
  :config
  (defvar dotfairy--initial-which-key-replacement-alist which-key-replacement-alist)
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)

  (which-key-add-key-based-replacements dotfairy-leader-key "<leader>")
  (which-key-add-key-based-replacements dotfairy-localleader-key "<localleader>"))


;;
;;; `map!' macro
(defvar dotfairy-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun dotfairy--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.
For example, :nvi will map to (list 'normal 'visual 'insert). See
`dotfairy-evil-state-alist' to customize this."
  (cl-loop for l across (dotfairy-keyword-name keyword)
           if (assq l dotfairy-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))


;; specials
(defvar dotfairy--map-forms nil)
(defvar dotfairy--map-fn nil)
(defvar dotfairy--map-batch-forms nil)
(defvar dotfairy--map-state '(:dummy t))
(defvar dotfairy--map-parent-state nil)
(defvar dotfairy--map-evil-p nil)
(after! evil (setq dotfairy--map-evil-p t))

(defun dotfairy--map-process (rest)
  (let ((dotfairy--map-fn dotfairy--map-fn)
        dotfairy--map-state
        dotfairy--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (dotfairy--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (dotfairy--map-commit)
                  (setq dotfairy--map-fn 'dotfairy--define-leader-key))
                 (:localleader
                  (dotfairy--map-commit)
                  (setq dotfairy--map-fn 'define-localleader-key!))
                 (:after
                  (dotfairy--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (dotfairy--map-set :keymaps `(quote ,(dotfairy-enlist (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (dotfairy-enlist (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (dotfairy--map-nested (list (intern (dotfairy-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (dotfairy-enlist (pop rest))
                    (let ((keymap (intern (format "dotfairy-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            dotfairy--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (dotfairy-enlist (pop rest))
                    (dotfairy--map-set (if dotfairy--map-fn :infix :prefix)
                                       prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          dotfairy--map-forms)))
                 (_
                  (condition-case _
                      (dotfairy--map-def (pop rest) (pop rest)
                                         (dotfairy--map-keyword-to-states key)
                                         desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((dotfairy--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (dotfairy--map-commit)
    (macroexp-progn (nreverse (delq nil dotfairy--map-forms)))))

(defun dotfairy--map-append-keys (prop)
  (let ((a (plist-get dotfairy--map-parent-state prop))
        (b (plist-get dotfairy--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun dotfairy--map-nested (wrapper rest)
  (dotfairy--map-commit)
  (let ((dotfairy--map-parent-state (dotfairy--map-state)))
    (push (if wrapper
              (append wrapper (list (dotfairy--map-process rest)))
            (dotfairy--map-process rest))
          dotfairy--map-forms)))

(defun dotfairy--map-set (prop &optional value)
  (unless (equal (plist-get dotfairy--map-state prop) value)
    (dotfairy--map-commit))
  (setq dotfairy--map-state (plist-put dotfairy--map-state prop value)))

(defun dotfairy--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (dotfairy-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state dotfairy--map-batch-forms)))
  t)

(defun dotfairy--map-commit ()
  (when dotfairy--map-batch-forms
    (cl-loop with attrs = (dotfairy--map-state)
             for (state . defs) in dotfairy--map-batch-forms
             if (or dotfairy--map-evil-p (not state))
             collect `(,(or dotfairy--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) dotfairy--map-forms))
    (setq dotfairy--map-batch-forms nil)))

(defun dotfairy--map-state ()
  (let ((plist
         (append (list :prefix (dotfairy--map-append-keys :prefix)
                       :infix  (dotfairy--map-append-keys :infix)
                       :keymaps
                       (append (plist-get dotfairy--map-parent-state :keymaps)
                               (plist-get dotfairy--map-state :keymaps)))
                 dotfairy--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.
If evil isn't loaded, evil-specific bindings are ignored.
Properties
  :leader [...]                   an alias for (:prefix dotfairy-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]
  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.
States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')
  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').
  These must be placed right before the key string.
  Do
    (map! :leader :desc \"Description\" :n \"C-c\" #'dosomething)
  Don't
    (map! :n :leader :desc \"Description\" \"C-c\" #'dosomething)
    (map! :leader :n :desc \"Description\" \"C-c\" #'dosomething)"
  (dotfairy--map-process rest))

(provide 'init-keybinds)
;;; init-keybinds.el ends here
