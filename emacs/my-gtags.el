(defun my-gtags-add-path ()
  "Adds a gtags path to GTAGSLIBPATH"
  (interactive)
  (let
      ((path (file-name-as-directory (expand-file-name (read-directory-name "gtags-path: "))))
       (gtagslibpath (getenv "GTAGSLIBPATH")))
    (setenv "GTAGSLIBPATH"
	    (if (> (length gtagslibpath) 0)
		(mapconcat (lambda (s) (format "%s" s)) (delete-dups (append (make-list 1 path) (parse-colon-path gtagslibpath))) path-separator)
	      path))))
