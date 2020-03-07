;; Octaspire dotfiles - Various configuration files
;; Copyright 2017, 2018, 2020  www.octaspire.com
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
(require 'cl-lib)
(require 'notifications)
(setq octaspire/ledger/timelog "~/.ledger.timelog")
(setq octaspire/ledger/time-format "%Y-%m-%d %H:%M:%S")

(setq octaspire/ledger/tasks
      '(TODO
	FILL-HERE
	TASK-NAMES))

(defun octaspire/ledger/balance-for-day ()
  (interactive)
  (let ((day (org-read-date)))
    (with-output-to-temp-buffer (concat "*timelog/" day "*")
      (princ (shell-command-to-string
	      (concat "ledger --file "
		      octaspire/ledger/timelog
		      " -p "
		      day
		      " balance"))))))

(defun octaspire/ledger/clock-in ()
  (interactive)
  (when (octaspire/ledger/ensure-clocked-out-or-empty)
    (let* ((task (completing-read "clock-in: " octaspire/ledger/tasks))
	   (line (concat "i " (format-time-string octaspire/ledger/time-format) " " task "\n")))
      (write-region
       line
       nil
       octaspire/ledger/timelog
       t)
      (message "clocked-in task %s" task))))

(defun octaspire/ledger/clock-out ()
  (interactive)
  (when (octaspire/ledger/ensure-clocked-in)
    (let ((current-task (octaspire/ledger/get-current-task))
	  (line (concat "o " (format-time-string octaspire/ledger/time-format) "\n")))
      (write-region
       line
       nil
       octaspire/ledger/timelog
       t)
      (message "clocked out from task %s" current-task))))

(defun octaspire/ledger/clock-change ()
  (interactive)
  (when (octaspire/ledger/clock-out)
    (octaspire/ledger/clock-in)))

(global-set-key (kbd "C-c C-l i")  'octaspire/ledger/clock-in)
(global-set-key (kbd "C-c C-l o")  'octaspire/ledger/clock-out)
(global-set-key (kbd "C-c C-l c")  'octaspire/ledger/clock-change)
(global-set-key (kbd "C-c C-l b")  'octaspire/ledger/balance-for-day)
(global-set-key (kbd "C-c C-l t")  'octaspire/ledger/task-show)

(defun octaspire/ledger/get-last-line ()
  (with-temp-buffer
    (insert-file-contents octaspire/ledger/timelog)
    (goto-char (buffer-size))
    (beginning-of-line)
    (if (> (buffer-size) 3)
	(buffer-substring-no-properties (point) (buffer-size))
      "")))

(defun octaspire/ledger/get-nth-token-from-last-line (index)
  (let* ((line (octaspire/ledger/get-last-line))
	 (tokens (split-string line)))
    (elt tokens index)))

(defun octaspire/ledger/get-current-type ()
  (when-let (result (octaspire/ledger/get-nth-token-from-last-line 0))
    (string-to-char result)))

(defun octaspire/ledger/get-current-task ()
  (octaspire/ledger/get-nth-token-from-last-line 3))

(defun octaspire/ledger/task-show ()
  (interactive)
  (message "Current task is: %s" (octaspire/ledger/get-current-task)))

(defun octaspire/ledger/get-current-state ()
  "Read the current logged in/out or empty state from timelog."
  (cl-case (octaspire/ledger/get-current-type)
    (?i 'clocked-in)
    (?o 'clocked-out)
    (t  'timelog-empty)))

(defun octaspire/ledger/ensure-clocked-in ()
  "Ensure that Ledger timelog is in clocked-in state."
  (let ((status (octaspire/ledger/get-current-state)))
    (cl-ecase status
      ('timelog-empty (octaspire/notify-warning
		       '(octaspire/ledger)
		       "You are not clocked-in"
		       "Ledger timelog is currently empty.
You are <b>NOT clocked-in</b>.")
		      nil)
      ('clocked-in t)
      ('clocked-out (octaspire/notify-warning
		     '(octaspire/ledger)
		     "You are not clocked-in"
		     "Ledger timelog is currently <b>clocked-out</b>.
You should be clocked-in.")
		    nil))))

(defun octaspire/ledger/ensure-clocked-out-or-empty ()
  "Ensure that Ledger timelog is in clocked-out state."
  (let ((status (octaspire/ledger/get-current-state)))
    (cl-ecase status
      ('timelog-empty t)
      ('clocked-in (octaspire/notify-warning
		    '(octaspire/ledger)
		    "You are not clocked-out"
		    "Ledger timelog is currently <b>clocked-in</b>.
You should be clocked-out.")
		   nil)
      ('clocked-out t))))

(add-hook 'kill-emacs-query-functions
	  (lambda ()
	    (unless (octaspire/ledger/ensure-clocked-out-or-empty)
	      (when (yes-or-no-p "You are NOT clocked-out. Clock out before exit? ")
		(octaspire/ledger/clock-out)))
	    t))

(run-at-time "1 min" 60 'octaspire/ledger/ensure-clocked-in)
