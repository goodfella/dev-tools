(defun empty-string-p (string)
  "Returns true if a string is empty or nil"
  (or (null string)
      (zerop (length string))))

(defun workday (from-string &optional optional-start-date)
  "Returns a work day given a start date and an org-mode from-string"
  (let* ((start-date (format-time-string "%Y-%m-%d" (if (not (empty-string-p optional-start-date))
							(org-time-string-to-time optional-start-date)
						      nil)))
	 (current start-date)
	 (end-date (org-read-date nil nil from-string nil (org-time-string-to-time current)))
	 (days 0))
    ;; Loop from start to end (skipping weekends)
    (while (string< current end-date)
      (progn
	(let ((decoded-time (decode-time (org-time-string-to-time current))))
	  (if (or (= (nth 6 decoded-time) 0) (= (nth 6 decoded-time) 6))
	      ;; Add an extra day for Saturday and Sunday
	      (setq end-date (org-read-date nil nil "++1" nil (org-time-string-to-time end-date))))
	  (setq days (1+ days))))
      (setq current (org-read-date nil nil "++1" nil (org-time-string-to-time current))))

    ;; Check if end date falls on a weekend
    (let ((decoded-time (decode-time (org-time-string-to-time current))))
      (if (= (nth 6 decoded-time) 0)
	  (setq days (1+ days))
	(if (= (nth 6 decoded-time) 6)
	    (setq days (+ days 2)))))
    (org-read-date nil nil (concat "++" (number-to-string days)) nil (org-time-string-to-time start-date))))


(defun completion-date (hours &optional optional-start-date)
  "Returns the workday a task will be completed given th number of hours it takes"
  (let* ((num-hours (string-to-number (nth 0 (split-string hours ":"))))
	 (days (if (= (% num-hours 8) 0)
		   (/ num-hours 8)
		 (+ (/ num-hours 8) 1))))
    (message "%d " days)
    (workday (concat "++" (number-to-string days)) optional-start-date)))