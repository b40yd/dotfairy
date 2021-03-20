;;; init-hydra.el
;;;
;;; (c) 7ym0n, https://gitlab.com/7ym0n/dotfairy
;;;

(use-package pretty-hydra
  :bind (("C-c c v" . editor-plus/body))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  (pretty-hydra-define editor-plus (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
                                           :color amaranth :quit-key "q")
    ("Move"
     (("a" nav/beginning-of-line "nav/beginning-of-line")
      ("A" nav/beginning-of-line-and-exit "nav/beginning-of-line-and-exit")
      ("e" nav/end-of-line "nav/end-of-line")
      ("E" nav/end-of-line-and-exit "nav/end-of-line-and-exit")
      ("l" forward-char "forward-char")
      ("<right>" forward-char "forward-char")
      ("h" backward-char "backward-char")
      ("<left>" backward-char "backward-char")
      ("j" next-line "next-line")
      ("<down>" next-line "next-line")
      ("k" previous-line "previous-line")
      ("<up>" previous-line "previous-line")
      ("[" backward-sexp "backward-sexp")
      ("]" forward-sexp "forward-sexp")
      ("{" nav/beginning-of-buffer-and-exit "nav/beginning-of-buffer-and-exit")
      ("}" nav/end-of-buffer-and-exit "nav/end-of-buffer-and-exit"))

     "Editor+"
     (("rc" clear-reactangle "clear-rectangle")
      ("rd" delete-rectangle "delete-rectangle")
      ("rw" copy-rectangle-as-kill "copy-rectangle")
      ("rk" kill-rectangle "kill-rectangle")
      ("v" er/expand-region "expand-region")
      ("V" er/contract-region "contract-region")
      ("y" yank-rectangle "yank-rectangle")
      ("u" undo "undo")
      ("xs" save-buffer "save-buffer")
      ("xd" find-lisp-find-dired "find-lisp-find-dired")
      ("<M-up>" drag-stuff-up "move-up-rectangle")
      ("<M-down>" drag-stuff-down "move-down-rectangle")
      ("Df" hungry-delete-forward "delete-forward-whitespace")
      ("Db" hungry-delete-backward "delete-backward-whitespace")
      ("ESC" (if (iedit-mode) iedit-mode) "quit-iedit-mode")
      ("SPC" set-mark-command "set-mark-command"))

     ""
     (("se" iedit-mode "iedit-mode")
      (";" iedit-toggle-selection "iedit-toggle-selection")
      ("<tab>" iedit-next-occurrence "iedit-next-occurrence")
      ("C-?" iedit-help-for-occurrences "iedit-help-for-occurrences")
      ("mc" iedit-toggle-case-sensitive "iedit-toggle-case-sensitive")
      ("mg" iedit-apply-global-modification "iedit-apply-global-modification")
      ("f" iedit-restrict-function "iedit-restrict-function")
      ("i" iedit-restrict-current-line "iedit-restrict-current-line")
      ("mn" iedit-expand-down-to-occurrence "iedit-expand-down-to-occurrence")
      ("mp" iedit-expand-up-to-occurrence "iedit-expand-up-to-occurrence")
      ("m{" iedit-expand-up-a-line "iedit-expand-up-a-line")
      ("m}" iedit-expand-down-a-line "iedit-expand-down-a-line")
      ("md" iedit-blank-occurrences "iedit-blank-occurrences")
      ("m<" iedit-goto-first-occurrence "iedit-goto-first-occurrence")
      ("m>" iedit-goto-last-occurrence "iedit-goto-last-occurrence")
      ("mB" iedit-toggle-buffering "iedit-toggle-buffering"))
     ""
     (("mD" iedit-delete-occurrences "iedit-delete-occurrences")
      ("mL" iedit-downcase-occurrences "iedit-downcase-occurrences")
      ("mN" iedit-number-occurrences "iedit-number-occurrences")
      ("mR" iedit-replace-occurrences "iedit-replace-occurrences")
      ("mU" iedit-upcase-occurrences "iedit-upcase-occurrences"))

     )))

(provide 'init-hydra)
