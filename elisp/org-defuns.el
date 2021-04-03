;;; org-defuns.el --- org-mode personal defuns -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Emmanuel GALLOIS
;;
;; Author: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Maintainer: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 28 Mar 2021
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

;;;; Reformatting an Org buffer
;; I picked up this little gem in the org mailing list. A function that reformats the current buffer by regenerating the
;; text from its internal parsed representation. Quite amazing.
(defun undx/org-reformat-buffer ()
  "Reformat org buffer."
  (interactive)
  (when (y-or-n-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))


(provide 'org-defuns)
;;; org-defuns.el ends here
