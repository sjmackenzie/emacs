  (setq load-path (cons (concat (getenv "OZHOME") "/share/elisp")
                        load-path))
  (setq auto-mode-alist
        (append '(("\\.oz\\'" . oz-mode)
	  	  ("\\.ozm$" . ozm-mode)
                  ("\\.ozg\\'" . oz-gump-mode))
                auto-mode-alist))
  (setq interpreter-mode-alist (cons '("oz" . oz-mode) interpreter-mode-alist))
  ;; autoload "oz.el" if the functions oz-mode or run-oz are executed
  (autoload 'run-oz "mozart" "Run Mozart as a sub-process." t)
  (autoload 'oz-mode "oz" "Major mode for editing Oz code." t)
  (autoload 'ozm-mode "mozart" "Major mode for displaying Oz machine code." t)
  (autoload 'oz-gump-mode "oz" "Major mode for editing Oz code with embedded Gump specifications." t)
  (autoload 'oz-new-buffer "oz" "" t)
  
  (provide 'oz)

