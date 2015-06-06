(defun my-common-mode-hook ()
  (interactive)
  (linum-mode)
  ;; show which function i'm in
  (which-function-mode)
  ;; show column number
  (column-number-mode))
