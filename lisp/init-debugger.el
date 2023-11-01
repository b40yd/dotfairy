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
   (use-package dap-mode
     :defines dap-python-executable
     :functions dap-hydra/nil
     :diminish
     :bind (:map lsp-mode-map
            ("<f5>" . dap-debug)
            ("M-<f5>" . dap-hydra))
     :hook ((after-init . dap-auto-configure-mode)
            (dap-stopped . (lambda (_) (dap-hydra)))
            (dap-terminated . (lambda (_) (dap-hydra/nil)))
            ((python-mode python-ts-mode)            . (lambda ()
                                                         (require 'dap-python)
                                                         (setq dap-python-debugger 'debugpy)))
            ((ruby-mode ruby-ts-mode)                . (lambda () (require 'dap-ruby)))
            ((go-mode go-ts-mode)                    . (lambda () (require 'dap-dlv-go)))
            ((java-mode java-ts-mode jdee-mode)      . (lambda () (require 'dap-java)))
            ((c-mode c-ts-mode c++-mode c++-ts-mode) . (lambda () (require 'dap-cpptools)))
            ((objc-mode swift-mode)                  . (lambda () (require 'dap-lldb)))
            (php-mode                                . (lambda () (require 'dap-php)))
            (elixir-mode                             . (lambda () (require 'dap-elixir)))
            ((js-mode js2-mode js-ts-mode)           . (lambda () (require 'dap-chrome)))
            (powershell-mode                         . (lambda () (require 'dap-pwsh))))
     :init
     (when (executable-find "python3")
       (setq dap-python-executable "python3"))))
  ('eglot
   (use-package dape
     :ensure nil
     :quelpa (dape :fetcher github :repo "svaante/dape")
     :config
     (setq dape-inline-variables t))))

(provide 'init-debugger)
;;; init-debugger.el ends here
