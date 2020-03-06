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
  (let* ((task (completing-read "clock-in: " octaspire/ledger/tasks))
	 (line (concat "i " (format-time-string octaspire/ledger/time-format) " " task "\n")))
    (write-region
     line
     nil
     octaspire/ledger/timelog
     t)
    (message "clocked-in task %s" task)))

(defun octaspire/ledger/clock-out ()
  (interactive)
  (let ((line (concat "o " (format-time-string octaspire/ledger/time-format) "\n")))
    (write-region
     line
     nil
     octaspire/ledger/timelog
     t)
    (message "clocked out")))

(defun octaspire/ledger/clock-change ()
  (interactive)
  (octaspire/ledger/clock-out)
  (octaspire/ledger/clock-in))

(global-set-key (kbd "C-c C-l i")  'octaspire/ledger/clock-in)
(global-set-key (kbd "C-c C-l o")  'octaspire/ledger/clock-out)
(global-set-key (kbd "C-c C-l c")  'octaspire/ledger/clock-change)
(global-set-key (kbd "C-c C-l b")  'octaspire/ledger/balance-for-day)

(defun octaspire/ledger/ensure-clocked-in ()
  "Ensure that Ledger timelog is in clocked-in state."
  (with-temp-buffer
    (insert-file-contents octaspire/ledger/timelog)
    (goto-char (buffer-size))
    (beginning-of-line)
    (let* ((start (point))
	   (line (buffer-substring-no-properties start (buffer-size))))
      (unless (char-equal (elt line 0) ?i)
	(display-warning '(octaspire/ledger) "--- NOT CLOCKED-IN ---")
	(notifications-notify
	 :title "Not Clocked-In"
	 :body "You are currently <b>NOT</b> clocked in.")))))

(run-at-time "1 min" 60 'octaspire/ledger/ensure-clocked-in)
