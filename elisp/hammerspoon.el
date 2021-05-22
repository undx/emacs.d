;;; hammerspoon.el --- description -*- lexical-binding: t; -*-
;;
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
;;
;; Author: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Maintainer: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 12 Apr 2021
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
;;===> hammerspoon-shell
;; Quick and dirty shell with interactive history search and persistence
;; Just drop into your ~/.emacs file.
;;
;; A hammerspoon buffer is any lua buffer visiting a pathname like
;;    **/*hammerspoon**/*.lua
;; Usage: M-x hammerspoon-shell, or Hyper-s in a hammerspoon buffer.
;; In any hammerspoon buffer, Hyper-c runs dofile(file) on the visited file.
;;
;; Tip: to reload a Spoon "MySpoon" without hs.reload:
;; package.loaded.MySpoon=false hs.spoons.use("MySpoon",{config={debug=true})
;;
;;; Code:

(defvar hammerspoon-buffer nil)

(add-hook 'lua-mode-hook
          (lambda ()
            (when (string-match "hammerspoon" buffer-file-name)
              (local-set-key (kbd "H-s") #'hammerspoon-shell)
              (local-set-key
               (kbd "H-c")
               (lambda ()
                 (interactive)
                 (save-buffer)
                 (let ((name buffer-file-name))
                   (unless (and (boundp 'hammerspoon-buffer)
                                (buffer-live-p hammerspoon-buffer))
                     (hammerspoon-shell))
                   (with-current-buffer hammerspoon-buffer
                     (goto-char (point-max))
                     (insert (concat "dofile(\"" name "\")"))
                     (comint-send-input))))))))

(defun hammerspoon-shell ()
  (interactive)
  (if (and hammerspoon-buffer (comint-check-proc hammerspoon-buffer))
      (pop-to-buffer hammerspoon-buffer)
    (setq hammerspoon-buffer (make-comint "hammerspoon"
                                          "/usr/local/bin/hs" nil "-C"))
    (let* ((process (get-buffer-process hammerspoon-buffer))
           (history-file "~/.hammerspoon/.hs-history"))
      (pop-to-buffer hammerspoon-buffer)
      (turn-on-comint-history history-file)
      (local-set-key (kbd "<down>") (lambda() (interactive)
                                      (comint-move-or-history nil)))
      (local-set-key (kbd "<up>") (lambda() (interactive)
                                    (comint-move-or-history 'up))))))

;; Comint configs and extensions
(setq comint-input-ring-size 1024
      comint-history-isearch 'dwim)

(defun comint-move-or-history (up &optional arg)
  "History if at process mark, move otherwise."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (proc-pos (if proc (marker-position (process-mark proc))))
         (arg (or arg 1))
         (arg (if up arg (- arg))))
    (if (and proc
             (if up
                 (= (line-number-at-pos) (line-number-at-pos proc-pos))
               (= (line-number-at-pos) (line-number-at-pos (point-max)))))
        (comint-previous-input arg)
      (forward-line (- arg)))))

(defun comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun turn-on-comint-history (&optional file)
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (or file
                (format "~/.emacs.d/inferior-%s-history"
                        (process-name process))))
      (comint-read-input-ring)
      ;; Ensure input ring gets written
      (add-hook 'kill-buffer-hook 'comint-write-input-ring nil t)
      (set-process-sentinel process
                            #'comint-write-history-on-exit))))

(defun comint-write-input-ring-all-buffers ()
  "Ensure all input rings get written on exit."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (comint-write-input-ring)))
        (buffer-list)))

(add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)

(provide 'hammerspoon)
;;; hammerspoon.el ends here
