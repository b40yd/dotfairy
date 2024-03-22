;;; init-debugger.el ---                                   -*- lexical-binding: t; -*-

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
(require 'init-custom)

(setq gdb-show-main t
      gdb-many-windows t)

(pcase dotfairy-lsp
  ('lsp-mode
   (use-package dape
     :bind (("<f5>" . dape)
            ("M-<f5>" . dape-hydra/body))
     :custom (dape-buffer-window-arrangment 'right)
     :pretty-hydra
     ((:title (pretty-hydra-title "Debug" 'codicon "nf-cod-debug")
       :color pink :quit-key ("q" "C-g"))
      ("Stepping"
       (("n" dape-next "next")
        ("s" dape-step-in "step in")
        ("o" dape-step-out "step out")
        ("c" dape-continue "continue")
        ("p" dape-pause "pause")
        ("k" dape-kill "kill")
        ("r" dape-restart "restart")
        ("D" dape-disconnect-quit "disconnect"))
       "Switch"
       (("m" dape-read-memory "memory")
        ("t" dape-select-thread "thread")
        ("w" dape-watch-dwim "watch")
        ("S" dape-select-stack "stack")
        ("i" dape-info "info")
        ("R" dape-repl "repl"))
       "Breakpoints"
       (("b" dape-breakpoint-toggle "toggle")
        ("l" dape-breakpoint-log "log")
        ("e" dape-breakpoint-expression "expression")
        ("B" dape-breakpoint-remove-all "clear"))
       "Debug"
       (("d" dape "dape")
        ("Q" dape-quit "quit" :exit t))))
     :config
     ;; Save buffers on startup, useful for interpreted languages
     (add-hook 'dape-on-start-hooks
               (defun dape--save-on-start ()
                 (save-some-buffers t t)))
     ;; Display hydra on startup
     (add-hook 'dape-on-start-hooks #'dape-hydra/body))))

(provide 'init-debugger)
;;; init-debugger.el ends here
