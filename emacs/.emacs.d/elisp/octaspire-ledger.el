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
(setq octaspire/ledger/timelog "~/.ledger.timelog")

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
	 (line (concat "i " (format-time-string "%Y-%m-%d %H:%M:%S") " " task "\n")))
    (write-region
     line
     nil
     octaspire/ledger/timelog
     t)
    (message line)))

(defun octaspire/ledger/clock-out ()
  (interactive)
  (let ((line (concat "o " (format-time-string "%Y-%m-%d %H:%M:%S") "\n")))
    (write-region
     line
     nil
     octaspire/ledger/timelog
     t)
    (message line)))

(defun octaspire/ledger/clock-change ()
  (interactive)
  (octaspire/ledger/clock-out)
  (octaspire/ledger/clock-in))

(global-set-key (kbd "C-c C-l i")  'octaspire/ledger/clock-in)
(global-set-key (kbd "C-c C-l o")  'octaspire/ledger/clock-out)
(global-set-key (kbd "C-c C-l c")  'octaspire/ledger/clock-change)
(global-set-key (kbd "C-c C-l b")  'octaspire/ledger/balance-for-day)