(provide 'oz-flymake)
;;
;; flymake for Oz
;;

;; adjust the path to ozflymake(.bat) here
(defun flymake-oz-executable ()
  "~/emacs/oz-flymake/ozflymake"
)

;; change to "nil" if you don't want the global procedure arity check
(defun flymake-oz-check-arity ()
  "t"
)

(require 'flymake)
(defun flymake-oz-init ()
  (condition-case ()
      (let* ((temp-file   (flymake-init-create-temp-buffer-copy
			   'flymake-create-temp-inplace))
	     (local-file  (file-relative-name
			   temp-file
			   (file-name-directory buffer-file-name))))
	(list (flymake-oz-executable) (list local-file (flymake-oz-check-arity))))
    (error nil)
    ))

(add-to-list 'flymake-allowed-file-name-masks
	     '("\\.oz\\'" flymake-oz-init))
;; Oz compiler
(add-to-list 'flymake-err-line-patterns
	     '("\\(error.*\\|warning.*\\) in file \"\\(.*\\)\", line \\([0-9]+\\), column \\([0-9]+\\)" 2 3 4 1))
;; if the compiler crashes:
(add-to-list 'flymake-err-line-patterns
     '("\\(Error: unhandled exception .*\\) pos\('\\([^']*\\)' \\([0-9]+\\) \\([0-9]+\\)" 2 3 4 1))

;; show flymake error as message in mini buffer
(defun flymake-display-first-err-as-message ()
  (interactive)
  (condition-case nil
      (let* ((line-no             (flymake-current-line-no))
	     (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
	     (count               (length line-err-info-list)))
	(if (> count 0)
	    (let ((line                (flymake-ler-text (nth (1- count) line-err-info-list))))
	      (message (concat "Flymake: " line))
	      )
	  (if (string= (substring (current-message) 0 8) "Flymake:")
	      (message nil)
	    )
	  )
    )))

;; jump to the next error and show error message
(defun oz-flymake-show-next-error()
  (interactive)
  (flymake-mode t)
  (flymake-goto-next-error)
  (flymake-display-first-err-as-message)
  )


;; activate flymake for Oz buffers and define key to jump to next error
(add-hook 'oz-mode-hook
          (function (lambda ()
		      (condition-case nil
			  (flymake-mode t) ;; fails for "Oz" buffer
			(error nil)
			)
                      (define-key oz-mode-map [C-tab] 'oz-flymake-show-next-error)
		      ;; refresh the error message all the time
		      (run-at-time nil 0.5 'flymake-display-first-err-as-message)
                      )))

;; use underlining instead of background color
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "orange")))))

;;
;; end flymake for Oz
;;
