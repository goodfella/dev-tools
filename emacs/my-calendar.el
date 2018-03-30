(require 'org)

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


(defun completion-date (hours &optional optional-effectiveness optional-people optional-start-date)
  "
  Returns the workday a task will be completed

  hours: The number of hours the task takes
  optional-effectiveness: Decimal representation of effectivenes
  optional-start-date: The day the task is started
  "
  (let* ((num-hours (if (stringp hours)
			(string-to-number (nth 0 (split-string hours ":")))
		      hours))

	 (effectiveness (if optional-effectiveness
			    (if (stringp optional-effectiveness)
				(if (empty-string-p optional-effectiveness)
				    1
				  (string-to-number optional-effectiveness))
			      optional-effectiveness)
			  1))
	 (people (if optional-people
		     (if (stringp optional-people)
			 (if (empty-string-p optional-people)
			     1
			   (string-to-number optional-people))
		       optional-people)
		   1))
	 (hours-per-day (floor (* 8 effectiveness)))
	 (days (floor (/ (if (= (% num-hours hours-per-day) 0)
			     (/ num-hours hours-per-day)
			   (+ (/ num-hours hours-per-day) 1))) people)))
    (workday (concat "++" (number-to-string days)) optional-start-date)))
