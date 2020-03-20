;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specific settings - MacOS X
;; Tangled from init.org - do not modify directly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
             :if (memq window-system '(mac ns))
             :ensure t
             :config (exec-path-from-shell-initialize))

(provide 'settings-os-darwin
