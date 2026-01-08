;;; init-ligatures.el ---                                   -*- lexical-binding: t; -*-

;; Copyright © 2024, b40yd, all rights reserved.

;; Author: b40yd <b40yd@scanbuf.com>
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

(defvar +ligatures-extra-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    :quote         "“"
    :quote_end     "”"
    :id            "🆔"
    :title         "📓"
    :headers       "☰"
    :archive       "📦"
    :author        "👤"
    :creator       "💁"
    :date          "📆"
    :description   "⸙"
    :end           "🔚"
    :email         "📧"
    :options       "⛭"
    :setupfile     "⛮"
    :properties    ""
    :tags          "🏷"
    :result        "💻"
    :checkbox      ""
    :indeterminate ""
    :checkboxed    ""
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    :list          "𝕃"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :union         "∪"
    :intersect     "∩"
    :diff          "∖"
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•"
    :shr           "≫"
    :shl           "≪"
    )
  "Maps identifiers to symbols, recognized by `set-ligatures'.

This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

(defvar +ligatures-alist
  '((prog-mode "|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
               ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
               "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
               "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
               "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
               "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
               "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
               "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
               ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
               "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
               "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
               "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
               "\\\\" "://")
    (t))
  "A alist of ligatures to enable in specific modes.")

(defvar +ligatures-in-modes nil
  "List of major modes where ligatures should be enabled.")
(make-obsolete-variable '+ligatures-in-modes "Use `ligature-ignored-major-modes' instead" "24.10.0")

(defvar +ligatures-prog-mode-list nil
  "A list of ligatures to enable in all `prog-mode' buffers.")
(make-obsolete-variable '+ligatures-prog-mode-list "Use `+ligatures-alist' instead" "3.0.0")

(defvar +ligatures-all-modes-list nil
  "A list of ligatures to enable in all buffers.")
(make-obsolete-variable '+ligatures-all-modes-list "Use `+ligatures-alist' instead" "3.0.0")

(defvar +ligatures-extra-alist '((t))
  "A map of major modes to symbol lists (for `prettify-symbols-alist').")

(defvar +ligatures-extras-in-modes t
  "List of major modes where extra ligatures should be enabled.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols' and assigned with `set-ligatures!'. This variable
controls where these are enabled.

  If t, enable it everywhere (except `fundamental-mode').
  If the first element is not, enable it in any mode besides what is listed.
  If nil, don't enable these extra ligatures anywhere (though it's more
efficient to remove the `+extra' flag from the :ui ligatures module instead).")


(defun +ligatures--enable-p (modes)
  "Return t if ligatures should be enabled in this buffer depending on MODES."
  (unless (eq major-mode 'fundamental-mode)
    (or (eq modes t)
        (if (eq (car modes) 'not)
            (not (apply #'derived-mode-p (cdr modes)))
          (apply #'derived-mode-p modes)))))

(defun +ligatures-init-extra-symbols-h ()
  "Set up `prettify-symbols-mode' for the current buffer.

Overwrites `prettify-symbols-alist' and activates `prettify-symbols-mode' if
(and only if) there is an associated entry for the current major mode (or a
parent mode) in `+ligatures-extra-alist' AND the current mode (or a parent mode)
isn't disabled in `+ligatures-extras-in-modes'."
  (when after-init-time
    (when-let*
        (((+ligatures--enable-p +ligatures-extras-in-modes))
         (symbols
          (if-let* ((symbols (assq major-mode +ligatures-extra-alist)))
              (cdr symbols)
            (cl-loop for (mode . symbols) in +ligatures-extra-alist
                     if (derived-mode-p mode)
                     return symbols))))
      (setq prettify-symbols-alist
            (append symbols
                    ;; Don't overwrite global defaults
                    (default-value 'prettify-symbols-alist)))
      (when (bound-and-true-p prettify-symbols-mode)
        (prettify-symbols-mode -1))
      (prettify-symbols-mode +1))))

;;;###autodef
(defun set-ligatures! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

MODES is a major mode symbol or a list of them.
PLIST is a property list whose keys must match keys in
`+ligatures-extra-symbols', and whose values are strings representing the text
to be replaced with that symbol.

If the car of PLIST is nil, then unset any
pretty symbols and ligatures previously defined for MODES.

For example, the rule for emacs-lisp-mode is very simple:

(set-ligatures! \\='emacs-lisp-mode
  :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
associated with :lambda in `+ligatures-extra-symbols'.

Pretty symbols can be unset for emacs-lisp-mode with:

(set-ligatures! \\='emacs-lisp-mode nil)

Note that this will keep all ligatures in `+ligatures-prog-mode-list' active, as
`emacs-lisp-mode' is derived from `prog-mode'."
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (ensure-list modes))
        (delq! mode +ligatures-extra-alist 'assq))
    (let ((results))
      (while plist
        (let ((key (pop plist)))
          (when-let (char (plist-get +ligatures-extra-symbols key))
            (push (cons (pop plist) char) results))))
      (dolist (mode (ensure-list modes))
        (setf (alist-get mode +ligatures-extra-alist)
              (if-let* ((old-results (alist-get mode +ligatures-extra-alist)))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))

;;;###autodef
(defun set-font-ligatures! (modes &rest ligatures)
  "Associates string patterns with ligatures in certain major-modes.

MODES is a major mode symbol or a list of them.
LIGATURES is a list of ligatures that should be handled by the font,
like \"==\" or \"-->\". LIGATURES is a list of strings.

For example, the rule for emacs-lisp-mode is very simple:

(set-font-ligatures! \\='emacs-lisp-mode \"->\")

This will ligate \"->\" into the arrow of choice according to your font.

All font ligatures for emacs-lisp-mode can be unset with:

(set-font-ligatures! \\='emacs-lisp-mode nil)

However, ligatures for any parent modes (like `prog-mode') will still be in
effect, as `emacs-lisp-mode' is derived from `prog-mode'."
  (declare (indent defun))
  (after! ligature
    (if (or (null ligatures) (equal ligatures '(nil)))
        (dolist (table ligature-composition-table)
          (let ((modes (ensure-list modes))
                (tmodes (car table)))
            (cond ((and (listp tmodes) (cl-intersection modes tmodes))
                   (let ((tmodes (cl-nset-difference tmodes modes)))
                     (setq ligature-composition-table
                           (if tmodes
                               (cons tmodes (cdr table))
                             (delete table ligature-composition-table)))))
                  ((memq tmodes modes)
                   (setq ligature-composition-table (delete table ligature-composition-table))))))
      (ligature-set-ligatures modes ligatures))))

(setq prettify-symbols-unprettify-at-point 'right-edge)

(add-hook! 'after-init-hook :append
  (defun +ligatures-init-h ()
    (add-hook 'after-change-major-mode-hook #'+ligatures-init-extra-symbols-h)))

(cond
 ;; The emacs-mac build of Emacs appears to have built-in support for ligatures,
 ;; using the same composition-function-table method
 ;; https://bitbucket.org/mituharu/emacs-mac/src/26c8fd9920db9d34ae8f78bceaec714230824dac/lisp/term/mac-win.el?at=master#lines-345:805
 ;; so use that instead if this module is enabled.
 ((if (featurep :system 'macos)
      (fboundp 'mac-auto-operator-composition-mode))
  (add-hook 'after-init-hook #'mac-auto-operator-composition-mode 'append))

 ;; This module does not support Emacs 27 and less, but if we still try to
 ;; enable ligatures, it will end up in catastrophic work-loss errors, so we
 ;; leave the check here for safety.
 ((and (> emacs-major-version 27)
       (or (featurep 'ns)
           (featurep 'harfbuzz))
       (featurep 'composite))   ; Emacs loads `composite' at startup

  (after! ligature
    ;; DEPRECATED: For backwards compatibility. Remove later.
    (with-no-warnings
      (when +ligatures-prog-mode-list
        (setf (alist-get 'prog-mode +ligatures-alist) +ligatures-prog-mode-list))
      (when +ligatures-all-modes-list
        (setf (alist-get t +ligatures-alist) +ligatures-all-modes-list)))
    (dolist (lig +ligatures-alist)
      (ligature-set-ligatures (car lig) (cdr lig))))
  ))

(provide 'init-ligatures)
;;; init-ligatures.el ends here
