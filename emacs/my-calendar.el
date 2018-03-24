(defun workday (start-date from-string)
  "Returns the date given a start date and an org-mode from-string"
  (let ((current start-date)
	(end-date (org-read-date nil nil from-string nil (org-time-string-to-time start-date))))

    ;; Loop from start to end (skipping weekends)
    (while (string< current end-date)
      (let ((decoded-time (decode-time (org-time-string-to-time current))))
	(if (or (= (nth 6 decoded-time) 0) (= (nth 6 decoded-time) 6))
	    ;; Skip weekend days
	    (setq current (org-read-date nil nil "++1" nil (org-time-string-to-time current)))))
      (setq current (org-read-date nil nil "++1" nil (org-time-string-to-time current))))

    ;; Check if the end date falls on a weekend day and skip the appropriate number of days
    (let ((decoded-time (decode-time(org-time-string-to-time current))))
      (if (= (nth 6 decoded-time) 0)
	  ;; Skip sunday
	  (setq current (org-read-date nil nil "++1" nil (org-time-string-to-time current)))
	(if (= (nth 6 decoded-time) 6)
	    ;; Skip saturday and sunday
	    (setq current (org-read-date nil nil "++2" nil (org-time-string-to-time current))))))
    current)) 
