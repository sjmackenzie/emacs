(provide 'my_config)

(setq inhibit-splash-screen t)

;; Tweaking editing environment
;; This assumes hypotheticalabs/emacs is symlinked to ~/emacs
(setq-default show-trailing-whitespace t)
(add-to-list 'default-frame-alist '(alpha . (100 100)))
(add-to-list 'load-path "~/emacs/erlang") ;; Configuration for Erlang mode
(add-to-list 'load-path "~/emacs/flymake") ;; Flymake syntax checker
(add-to-list 'load-path "~/emacs/git") ;; Git mode
(add-to-list 'load-path "~/emacs/anything") ;; Quicksilver-like switcher
;;(add-to-list 'load-path "~/emacs/distel") ;; Distel package
(add-to-list 'load-path "~/emacs/whitespace") ;; Whitespace package
(add-to-list 'load-path "~/emacs/autosave") ;; Autosave config
(add-to-list 'load-path "~/emacs/custom_keys") ;; Custom keys config
(add-to-list 'load-path "~/emacs/objx") ;; objective c and j config
(add-to-list 'load-path "~/emacs/escreen") ;; escreen
(add-to-list 'load-path "~/emacs/oz-flymake") ;; oz-flymake
(add-to-list 'load-path "~/emacs/oz") ;; mozart oz

(require 'erlang_mode_config) ;; Loading Erlang mode
(require 'flymake_config) ;; Loading flymake
(require 'anything-config)
;;(require 'distel_config) ;; Loading distel
(require 'whitespace_config) ;; Loading whitespace
(require 'git) ;; Loading Git mode
(require 'autosave_config) ;; Configures autosaving
(require 'custom_keys_config) ;; custom key bindings
(require 'objj-mode)
(require 'escreen)
(require 'oz-flymake)
(require 'oz)
