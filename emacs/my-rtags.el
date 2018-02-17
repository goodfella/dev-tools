;; This requires that the rtags lisp installation has been added to
;; 'load-path
(load-library "rtags")

;; Map Alt-. to tag jump
(define-key c-mode-base-map "\M-." (lambda () (interactive) (progn
							      (rtags-find-symbol-at-point))))

;; Map Alt-* to location stack pop
(define-key c-mode-base-map "\M-*" (lambda () (interactive) (rtags-location-stack-back)))
