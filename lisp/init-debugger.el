;;; init-debugger.el ---                                   -*- lexical-binding: t; -*-

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
(require 'init-custom)

(setq gdb-show-main t
      gdb-many-windows t)

(pcase dotfairy-lsp
  ('lsp-mode
   (use-package dap-mode
     :ensure t
     :defines dap-python-executable
     :diminish
     :bind (:map lsp-mode-map
            ("<f5>" . dap-debug)
            ("S-<f5>" . dap-hydra))

     :hook ((after-init . dap-auto-configure-mode)
            (dap-stopped . (lambda (_args) (dap-hydra)))
            (python-mode . (lambda () (require 'dap-python)))
            (ruby-mode . (lambda () (require 'dap-ruby)))
            (go-mode . (lambda () (require 'dap-dlv-go)))
            (java-mode . (lambda () (require 'dap-java)))
            (rustic-mode . (lambda ()
                             (require 'dap-lldb)
                             (require 'dap-cpptools)))
            (swift-mode . (lambda ()
                            (require 'dap-lldb)))
            ((c-mode c++-mode objc-mode) . (lambda ()
                                             (require 'dap-lldb)
                                             (require 'dap-gdb-lldb)))
            (php-mode . (lambda () (require 'dap-php)))
            ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
            (powershell-mode . (lambda () (require 'dap-pwsh))))
     :init
     (when (executable-find "python3")
       (setq dap-python-executable "python3")))))

(provide 'init-debugger)
;;; init-debugger.el ends here
