(defun my-common-mode-hook ()
  (interactive)
  (linum-mode)
  ;; show which function i'm in
  (which-function-mode)
  ;; show column number
  (column-number-mode))

(defun my-c++-lineup-inclass (langelem)
  "Properly lines up members in a struct or enum class"
  (let ((inclass (assoc 'inclass  c-syntactic-context)))
    (save-excursion (goto-char (c-langelem-pos inclass))
		    (if (or (looking-at "struct") (looking-at "typedef struct")
			    (looking-at "enum class")) '+ '+))))

(defun my-c++-mode()
  "Sets up my style"
  (interactive)
  (c-set-style "ellmentel")
  (linum-mode)
  (setq c-basic-offset 4)
  (setq-default indent-tabs-mode nil)
  (c-set-offset 'inclass 'my-c++-lineup-inclass)
  (c-set-offset 'friend '-)
  (c-set-offset 'access-label '0)
  (message "Using my C++ Mode"))
