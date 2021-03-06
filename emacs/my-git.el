(defun my-git-top-dir (dirpath)
  "Returns the top level git directory associated with a path"
  (let ((dirpath (expand-file-name (file-truename dirpath))))
    (when (file-directory-p dirpath)
      (let* ((default-directory (file-name-as-directory dirpath))
	     (gitdir (shell-command-to-string "git rev-parse --show-cdup 2> /dev/null | tr -d '\n'")))
	(when (not (string= gitdir ""))
	  (expand-file-name gitdir dirpath))))))

(defun my-git-show ()
  "Opens a file maintained in a git repository at the given revision"
  (interactive)
  (let*
      ((path (read-file-name "path: "))
       (committish (read-from-minibuffer "committish: "))
       (gittopdir (my-git-top-dir (file-name-directory path)))
       (relpath (file-relative-name path gittopdir))
       (buffername (format "%s:%s" relpath committish)))
    (if gittopdir
	(let* ((default-directory gittopdir))
	  (call-process "git" nil buffername t "--no-pager" "show" (format "%s:%s" committish relpath))
	  (switch-to-buffer buffername))
      (message (format "path: %s, is not under git's control" path)))))
