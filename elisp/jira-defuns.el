;;; jira-defuns.el --- Personal org-jira defuns -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Emmanuel GALLOIS
;;
;; Author: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Maintainer: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 25 Mar 2021
;;
;; URL: https://github.com/undx/
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:


;;;###autoload
(defun org-jira-get-issues-headonly (issues)
  "Get list of ISSUES, head only.

The default behavior is to return issues assigned to you and unresolved.

With a prefix argument, allow you to customize the jql.  See
`org-jira-get-issue-list'."

  (interactive
   (org-jira-get-issue-list))

  (let* ((issues-file (expand-file-name "issues-headonly.org" (org-jira--ensure-working-dir)))
         (issues-headonly-buffer (or (find-buffer-visiting issues-file)
                                     (find-file issues-file))))
    (with-current-buffer issues-headonly-buffer
      (widen)
      (delete-region (point-min) (point-max))

      (mapc (lambda (issue)
              (let ((issue-id (org-jira-get-issue-key issue))
                    (issue-status-org (org-jira-get-org-keyword-from-status (org-jira-find-value issue 'fields 'status 'name)))
                    (issue-priority (org-jira-get-org-priority-cookie-from-issue  (org-jira-find-value issue 'fields 'priority 'name)))
                    (issue-summary (org-jira-get-issue-summary issue)))
                (org-jira-insert (format "** %s %s[[jira:%s]] %s\n" issue-status-org issue-priority issue-id issue-summary))))
            issues)
      (switch-to-buffer issues-headonly-buffer))))


(defun undx/org-jira-get-issues-from-custom-jql (&optional jql-list)
  "Get JQL-LIST list of issues from a custom JQL and PROJ-KEY.

The PROJ-KEY will act as the file name, while the JQL will be any
valid JQL to populate a file to store PROJ-KEY results in.

Please note that this is *not* concurrent or race condition
proof.  If you try to run multiple calls to this function, it
will mangle things badly, as they rely on globals DEFAULT-JQL and
ORG-JIRA-PROJ-KEY-OVERRIDE being set before and after running."
  (interactive)
  (let* ((jl (or jql-list org-jira-custom-jqls))
         (uno (car jl))
         (filename (cl-getf uno :filename))
         (limit (cl-getf uno :limit))
         (jql (replace-regexp-in-string "[\n]" " " (cl-getf uno :jql))))
    (setq org-jira-proj-key-override filename)
    (jiralib-do-jql-search jql limit (org-jira-get-issues-from-custom-jql-callback filename jl))))

(provide 'jira-defuns)
;;; jira-defuns.el ends here
