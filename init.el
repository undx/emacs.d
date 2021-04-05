;; init.el --- Description my init  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Emmanuel GALLOIS
;;
;; Author: Emmanuel GALLOIS <https://github.com/undx>
;; Maintainer: Emmanuel GALLOIS <emmanuel.gallois@gmail.com>
;; Created: March 02, 2021
;; Modified: March 02, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/undx/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;; startup settings
;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless (daemonp)
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist' since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'doom--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist doom--initial-file-name-handler-alist)
    (message "Emacs ready in %s with %d garbage collections."
             (format "%.4f seconds" (float-time (time-subtract after-init-time before-init-time)))
             gcs-done)
    (unless (and (fboundp 'server-running-p)
                 (server-running-p))
      (server-start)))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)
  (add-hook 'after-init-hook #'(lambda ()
                                 ;; restore after startup
                                 ;; Make gc pauses faster by decreasing the threshold
                                 (setq gc-cons-threshold (* 2 1000 1000)
                                       gc-cons-percentage 0.1)))
  )
;; Ensure Doom is running out of this file's directory
;;(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))
;;;; debugging
;; To debug a lisp function use debug-on-entry. You step in with d and over with e
;;(use-package esup :ensure t)
;; (setq debug-on-message "Invalid face")
;;(setq debug-on-message "test")
;;(defun test ()  (interactive)      (message "test"))
;;
;; (defadvice message (before who-said-that activate)
;;   "Find out who said that thing. and say so."
;;   (let ((trace nil) (n 1) (frame nil))
;;     (while (setq frame (backtrace-frame n))
;;       (setq n     (1+ n)
;;             trace (cons (cadr frame) trace)) )
;;     (ad-set-arg 0 (concat "<<%S>>: " (ad-get-arg 0)))
;;     (ad-set-args 1 (cons trace (ad-get-args 1))) ))
;; (defadvice message (before when-was-that activate)
;;   "Add timestamps to `message' output."
;;   (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T %Z] ")
;;     (ad-get-arg 0)) ))
;; (ad-disable-advice 'message 'before 'who-said-that)
;; (ad-disable-advice 'message 'before 'when-was-that)
;; (ad-update 'message)

;;;; packages bootstrap
;; With use-package, there is multiple ways to load on demand:
;; :commands to add callable that will trigger loading the package
;; :bind, :bind*, :bind-keymap and :bind-keymap* to bind key sequences to the global keymap or a specific keymap.
;; :mode and :interpreter establish a deferred binding with the auto-mode-alist and interpreter-mode-alist.
;; :hook allows adding functions onto the package hook
;; :defer is the most generic one, all the previous keyword imply :defer. You can specify a number of second of idle to load the package.
;; Install if needed and Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Enable use-package
(eval-when-compile
  (require 'use-package))
(use-package diminish
  :ensure t)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(use-package auto-package-update
  :ensure t
  :defer t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results nil)
  ;; at startup (auto-package-update-maybe)
  (auto-package-update-at-time "13:00")
  )
(use-package benchmark-init
  :ensure t
  :unless (memq window-system '(mac ns))
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;;
;; Automated error hunting: M-x bug-hunter-init-file RET e
;; Interactive hunt: M-x bug-hunter-init-file RET i
;;
;; The Bug Hunter will start a separate Emacs instance several times, and then it will ask you each time whether that
;; instance presented the problem you have. After doing this about 5–12 times, you’ll be given the results.
;;
;; Assertion hunt: M-x bug-hunter-init-file RET a (featurep 'cl) RET
;;
;; Are you getting obscure errors when trying to open “.tex” files?
;;   Don’t despair! Just use (and (find-file "dummy.tex") nil) as the assertion.
;; Did ox-html stop working due to some arcane misconfiguration?
;;   Just write an assertion that does an export and checks the result.
;; Does some random command suddenly bind itself to C-j and you can’t figure out why?
;;   (eq (key-binding "\n") 'unwanted-command) is the assertion for you!
(use-package bug-hunter
  :ensure t
  :defer t)
;;
;;; extra load path
(add-to-list 'load-path "~/.emacs.d/elisp/")
;;; base settings

(defmacro comment (&rest BODY)
  "Comment 'BODY', ie one or more s-expressions."
  nil)

;; sane defaults
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'set-goal-column  'disabled nil)
(put 'erase-buffer     'disabled nil)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
;; encoding
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix)
      ;; user informations
      user-full-name "Emmanuel GALLOIS"
      user-mail-address "emmanuel.gallois@gmail.com"
      ;; default fill-column
      fill-column 120
      ;; avoid line truncation
      truncate-partial-width-windows nil
      confirm-kill-emacs nil
      size-indication-mode t
      require-final-newline t
      subword-mode t
      ;; blobs, amount of data read from processes
      read-process-output-max (* 1024 1024)
      ;; see documentation
      enable-local-variables :all
      ;; mark all themes as safe, since we can't persist now
      custom-safe-themes t
      ;; littering
      delete-by-moving-to-trash t
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      ;; custom file is never re-used but can still be consulted if needed
      custom-file (make-temp-file (format "emacs_custom_%s_" (format-time-string "%Y-%m-%d_%H%M%S")))
      ;; less noise when compiling elisp
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      ;; show when gc triggers : nil or t
      garbage-collection-messages nil
      ;; inhibit some stuff
      inhibit-startup-screen t
      ring-bell-function 'ignore
      )
;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
(setq-default fill-column 120
              major-mode
              (lambda ()
                (if buffer-file-name
                    (fundamental-mode)
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
;; detects long lines and takes appropriate action
(global-so-long-mode)
;; persistent-scratch
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-autosave-mode 1)
  (when (file-exists-p persistent-scratch-save-file)
    (persistent-scratch-restore)))
;; for my fish memory
(use-package which-key
  :ensure t
  :defer 5
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))
;;; UI
;;;; interface
;;;;
;;(tool-bar-mode 0)
;;(scroll-bar-mode -1)
(menu-bar-mode t)
(column-number-mode)
(show-paren-mode t)
;; (global-hl-line-mode t) ;; highlight current line
(setq-default linum-format "%5d")
(global-linum-mode)
;;
(setq indicate-buffer-boundaries 'left
      indicate-empty-lines t
      show-paren-delay 0.1
      show-paren-style 'parenthesis) ;; parenthesis, expression, mixed.
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains? org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
(add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines nil)))
;; whitespace
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook
                  org-mode-hook
                  conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column nil
        whitespace-style '(face tabs newline space-mark trailing tab-mark newline-mark)
        whitespace-display-mappings
        '((space-mark nil)
          (newline-mark 10 [172 10])
          (tab-mark 9 [187 9] [9655 9] [92 9])
          ))
  (setq-default show-trailing-whitespace nil)
  (defun no-trailing-whitespace ()
    (setq-local show-trailing-whitespace nil))
  :diminish whitespace-mode)
;; to put in specific modes
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format '((which-func-mode ("%f (%I) " which-func-format " "))))

;;;; theme
;;my custom theme inspired by Git@gitlab.com:fommil/emacs-intellij-theme.git
(require 'intellij-theme)
(load-theme 'intellij t)
;; cusor-type nil, box, hollow, bar, hbar
(setq cursor-type 'bar)
(setq-default cursor-in-non-selected-windows 'nil)

(set-face-attribute 'default nil     :font "JetBrains Mono" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Jetbrains Mono" :height 120)
(set-face-attribute 'variable-pitch nil :font "Ubuntu Mono" :height 120)
;;;; FringeMark
;; Left . Right
(set-fringe-mode '(30 . 20))
;;;; pretty symbols
;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)
;; Alist of symbol prettifications.
;; Each element looks like (SYMBOL . CHARACTER), where the symbol matching SYMBOL (a string, not a
;; regexp) will be shown as CHARACTER instead.
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                       ("#+END_SRC" . "«")
                                       ("#+begin_src" . "»")
                                       ("#+end_src" . "«")
                                       ("#+NAME:" . "»»")
                                       ("#+name:" . "»»")
                                       ("lambda"  . "λ")
                                       ("->" . "→")
                                       ("->>" . "↠")))
;;;; ligatures
(let ((ligatures `((?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
                   (?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
                   (?*  . ,(regexp-opt '("*>" "***" "*/")))
                   (?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<"
                                         "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
                                         "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
                   (?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
                   (?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
                   (?!  . ,(regexp-opt '("!==" "!!" "!=")))
                   (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
                   (?&  . ,(regexp-opt '("&&&" "&&")))
                   (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
                   (?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
                   (?+  . ,(regexp-opt '("+++" "+>" "++")))
                   (?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
                   (?\{ . ,(regexp-opt '("{|")))
                   (?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
                   (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
                   (?\; . ,(regexp-opt '(";;")))
                   (?_  . ,(regexp-opt '("_|_" "__")))
                   (?\\ . ,(regexp-opt '("\\" "\\/")))
                   (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
                   (?$  . ,(regexp-opt '("$>")))
                   (?^  . ,(regexp-opt '("^=")))
                   (?\] . ,(regexp-opt '("]#"))))))
  (dolist (char-regexp ligatures)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(comment
 (appendq! +ligatures-extra-symbols
           `(
             :checkbox      "☐"
             :pending       "◼"
             :checkedbox    "☑"
             :list_property "∷"
             :em_dash       "—"
             :ellipses      "…"
             :arrow_right   "→"
             :arrow_left    "←"
             :title         "𝙏"
             :subtitle      "𝙩"
             :author        "𝘼"
             :date          "𝘿"
             :property      "☸"
             :options       "⌥"
             :latex_class   "🄲"
             :latex_header  "⇥"
             :beamer_header "↠"
             :attr_latex    "🄛"
             :attr_html     "🄗"
             :begin_quote   "❮"
             :end_quote     "❯"
             :caption       "☰"
             :header        "›"
             :results       "🠶"
             :begin_export  "⏩"
             :end_export    "⏪"
             :properties    "⚙"
             :end           "∎"
             :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
             :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
             :priority_c   ,(propertize "■"  'face 'all-the-icons-yellow)
             :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
             :priority_e   ,(propertize "❓"  'face 'all-the-icons-blue)))
 (set-ligatures! 'org-mode
                 :merge t
                 :checkbox      "[ ]"
                 :pending       "[-]"
                 :checkedbox    "[X]"
                 :list_property "::"
                 :em_dash       "---"
                 :ellipsis      "..."
                 :arrow_right   "->"
                 :arrow_left    "<-"
                 :title         "#+title:"
                 :subtitle      "#+subtitle:"
                 :author        "#+author:"
                 :date          "#+date:"
                 :property      "#+property:"
                 :options       "#+options:"
                 :latex_class   "#+latex_class:"
                 :latex_header  "#+latex_header:"
                 :beamer_header "#+beamer_header:"
                 :attr_latex    "#+attr_latex:"
                 :attr_html     "#+attr_latex:"
                 :begin_quote   "#+begin_quote"
                 :end_quote     "#+end_quote"
                 :caption       "#+caption:"
                 :header        "#+header:"
                 :begin_export  "#+begin_export"
                 :end_export    "#+end_export"
                 :results       "#+RESULTS:"
                 :property      ":PROPERTIES:"
                 :end           ":END:"
                 :priority_a    "[#A]"
                 :priority_b    "[#B]"
                 :priority_c    "[#C]"
                 :priority_d    "[#D]"
                 :priority_e    "[#E]")
 (plist-put +ligatures-extra-symbols :name "⁍")
 )

;;;;; specific font sizes
;; (dolist
;;   (buf (list " *Messages*"  "*Completions*"))
;; (when (get-buffer buf)
;;   (with-current-buffer buf
;;     (setq-local face-remapping-alist '((default (:height 3.9)))))))
;;(defun my-minibuffer-setup () (set (make-local-variable 'face-remapping-alist) '((default :height 0.9))))
;;
;; (defun my-buffer-smaller-face ()
;;   "Sets a fixed width (monospace) font in current buffer"
;;   (interactive)
;;   (setq buffer-face-mode-face '(:family "JetBrainsMono" :height 90))
;;   (buffer-face-mode)
;;   )
;;(face-remap-add-relative 'default '(:foreground "black" :background "yellow"))
;; (with-current-buffer (get-buffer " *Echo Area 0*"); the leading space character is correct (setq-local face-remapping-alist '((default (:height 0.9) ))))
;;   (with-current-buffer (get-buffer "*Messages*") (setq-local face-remapping-alist '((default (:height 0.7) :background "white smoke"))))
;;(with-current-buffer (get-buffer "*Messages*") (setq-local face-remapping-alist '((default (:height 0.7) :background "white smoke"))))
;; (add-hook 'minibuffer-setup-hook 'my-buffer-smaller-face)
;; (add-hook 'messages-buffer-setup-hook 'my-buffer-smaller-face)
;; (add-hook 'help-mode-hook 'my-buffer-smaller-face)
;; (add-hook 'messages-buffer-mode-hook 'my-minibuffer-setup)
;; (add-hook 'messages-buffer-mode-hook (lambda () (message "Messages mode hook called!") (text-scale-decrease 3)))

;;;; eye-candy
;; rainbow
(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (prog-mode emacs-lisp-interaction-mode emacs-lisp-mode))
;; rainbow-delimiter
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))
;; beacon
(use-package beacon
  :ensure t
  :defer 1
  :diminish
  :config
  (setq beacon-blink-delay '0.5
        beacon-blink-when-focused t
        beacon-blink-when-buffer-changes t
        beacon-color  "gold")
  (beacon-mode))
;; icons
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
;;; window management
;;
;; C-x o Switch active window
;; C-x 0 Deletes the active window
;; C-x 1 Deletes other windows
;; C-x 2 Split window below
;; C-x 3 Split window right
;;
;; C-x 4 Other window management
;;
;; C-x 4 C-f Finds a file in the other window
;; C-x 4 d Opens M-x dired in the other window
;; C-x 4 C-o Displays a buffer in the other window
;; C-x 4 b Switches the buffer in the other window and makes it the active window
;; C-x 4 0 Kills the buffer and window
;; C-x 4 p Run project command in the other window
;;
;; C-x 5 frame management
;;
;; C-x 5 2 Create a new frame
;; C-x 5 b Switch buffer in other frame
;; C-x 5 0 Delete active frame
;; C-x 5 1 Delete other frames
;; C-x 5 C-f Finds a file in the other frame
;; C-x 5 p Run project command in the other frame
;; C-x 5 d Opens M-x dired in the other frame
;; C-x 5 C-o Displays a buffer in the other frame
;;
;; C-x t Tab management
;;
;; Tab Bar ( tab-bar-mode )
;;
;; Same as workspaces or projects in modern IDE, groups window configurations (frame layout).
;;
;; C-x t 2 Create a new tab
;; C-x t 0 Close the current tab
;; C-x t RET Select tab by name
;; C-x t o, C-<tab> Next Tab
;; C-S-<tab> Previous Tab
;; C-x t r Rename Tab
;; C-x t m Move tab one position to the right
;; C-x t p ... Run project command in other tab
;; C-x t t Execute command in other tab
;; C-x t 1 Close all other tabs
;; C-x t C-f, C-x t f Find file in other tab
;; C-x t b Switch to buffer in other tab
;; C-x t d Open Dired in other tab
;;
;; M-x tab-list Shows an interactive tab list
;; M-x tab-undo Undoes a closed tab for each invocation
;; M-x tab-recent Switch to the last visited tab
;;
;; see tab-bar-history-mode
;; (global-set-key (kbd "M-[") 'tab-bar-history-back)
;; (global-set-key (kbd "M-]") 'tab-bar-history-forward)
;;
;; Tab Line ( tab-line-mode )
;;
;; Same as browser tabs
;; C-x <left>  Select previous buffer
;; C-x <right> Select next buffer
;;


;; M-m (back-to-indentation) Move point to the first non-whitespace character on this line.

;;
;; Notions
;; side-window:
;; side: top bottom left right
;;
;; slot: placement location. if 2 windows have the same slot (and placement),
;; new one will replace the other.
;; LEFT CENTER RIGHT
;; -n      0   +n
;;
;; window-parameters: additional parameters to control window behavior
;;  - no-other-window: do not switch to this window. Not selectable with =C-x o=.
;;  - no-delete-other-windows: this will not be deletable by =C-x 1=.
;;  window-purpose
;;(use-package! window-purpose  :config              (purpose-mode))

(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*" "*Org Select*") (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.33)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))
;; https://erick.navarro.io/blog/save-and-restore-window-configuration-in-emacs/
(defvar window-snapshots '())
(defun save-window-snapshot ()
  "Save the current window configuration into `window-snapshots` alist."
  (interactive)
  (let ((key (read-string "Enter a name for the snapshot: ")))
    (setf (alist-get key window-snapshots) (current-window-configuration))
    (message "%s window snapshot saved!" key)))
(defun get-window-snapshot (key)
  "Given a KEY return the saved value in `window-snapshots` alist."
  (let ((value (assoc key window-snapshots)))
    (cdr value)))
(defun restore-window-snapshot ()
  "Restore a window snapshot from the 'window-snapshots' alist."
  (interactive)
  (let* ((snapshot-name (completing-read "Choose snapshot: " (mapcar #'car window-snapshots)))
	 (snapshot (get-window-snapshot snapshot-name)))
    (if snapshot
	(set-window-configuration snapshot)
      (message "Snapshot %s not found" snapshot-name))))
;; winner-mode remembers your window settings and lets you undo and redo with C-c <left> and C-c <right>.
(winner-mode)
;; uniquify
(use-package uniquify
  :config
  (setq
   uniquify-separator "/"               ;; The separator in buffer names.
   uniquify-buffer-name-style 'forward) ;; names/in/this/style
  )
;; https://github.com/abo-abo/ace-window
;; You can swap windows by calling ace-window with a prefix argument C-u.
;; You can delete the selected window by calling ace-window with a double prefix argument, i.e. C-u C-u.
;; Change the action midway
;; You can also start by calling ace-window and then decide to switch the action to delete or swap etc. By default the bindings are:
;; x - delete window
;; m - swap windows
;; M - move window
;; c - copy window
;; j - select buffer
;; n - select the previous window
;; u - select buffer in the other window
;; c - split window fairly, either vertically or horizontally
;; v - split window vertically
;; b - split window horizontally
;; o - maximize current window
;; ? - show these command bindings
(use-package ace-window
  :ensure t
  :defer t
  :custom-face
  ;;  (aw-leading-char-face nil :foreground "blue" :weight 'bold :height 4.0)
  :config
  (set-face-attribute 'aw-leading-char-face nil :foreground "blue" :weight 'bold :height 4.0)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help)
          (?b balance-windows)))
  (when (package-installed-p 'hydra)
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("j" shrink-window "shrink vertical")
      ("k" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
    (defhydra hydra-window-scroll (:color red)
      "Scroll other window"
      ("n" (lambda () ((interactive) (scroll-other-window 1)))       "scroll")
      ("p" (lambda () ((interactive) (scroll-other-window-down 1)))   "scroll down"))
    ;;
    (add-to-list 'aw-dispatch-alist '(?= hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?- hydra-window-scroll/body) t)
    )
  :bind
  (("C-o" . 'ace-window)
   ("C-c w <up>"    . 'buf-move-up)
   ("C-c w <down>"  . 'buf-move-down)
   ("C-c w <left>"  . 'buf-move-left)
   ("C-c w <right>" . 'buf-move-right))
  :custom
  ;;(aw-ignore-current t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;;; editing
(setq-default  indent-tabs-mode nil
               ;; Make tab key do indent first then completion.
               tab-always-indent 'complete
               ;;
               history-length 1000
               )
(setq tab-width 2)
;; save cursor position
(save-place-mode 1)
;; savehist
(savehist-mode t)
;;(add-hook prog-mode-hook (lambda() (setq display-line-numbers-type t)))
;;
(use-package recentf
  :defer t
  :custom
  (recentf-max-saved-items 200))
;; aggressive-indent
(use-package aggressive-indent
  :ensure t
  :diminish "ai"
  :hook (prog-mode . aggressive-indent-mode)
  ;;  :config (aggressive-indent-mode))
  )
;; outshine - https://github.com/alphapapa/outshine
(use-package outshine
  :ensure t
  :defer t
  :after outline
  :diminish "os"
  :config
  (unbind-key "M-<up>" outline-mode-map)
  (unbind-key "M-<down>" outline-mode-map)
  (unbind-key "M-<left>" outshine-mode-map)
  (unbind-key "M-<right>" outshine-mode-map)
  :bind
  (:map outshine-mode-map
        ("<S-iso-lefttab>" . outshine-cycle-buffer))
  :hook (prog-mode . outshine-mode))
;;;; smartparens
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :defer t
  :diminish "sp"
  :config
  ;;(require 'smartparens-config)
  (smartparens-global-mode)
  (setq sp-show-enclosing-pair-commands t)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  ;; lisp modes
  (sp-with-modes sp-lisp-modes
                 ;; disable ', it's the quote character! this is in default config
                 ;; (sp-local-pair "'" nil :actions nil)
                 ;; also only use the pseudo-quote inside strings where it
                 ;; serves as hyperlink.
                 (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
                 ;;
                 (sp-local-pair "(" ")" :wrap "C-("))
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  ;; indent after inserting any kinds of parens
  (defun pair-newline-and-indent (id action context)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
  ;; |foobar
  ;; hit C-(
  ;; becomes (|foobar)
  ;; (sp-local-pair "(" nil :bind "C-("))
  ;; (sp-pair "'" "'" :actions '(wrap))          ;; only use '' pair for wrapping
  ;; (sp-pair "%" "%" :actions '(insert))        ;; only use %% pair for auto insertion, never for wrapping
  ;; (sp-pair "(" ")" :actions '(wrap insert))   ;; use () pair for both actions. This is default for each new pair
  ;; (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)           ;; no '' pair in emacs-lisp-mode
  ;; (sp-local-pair 'markdown-mode "`" nil :actions '(insert))       ;; only use ` for auto insertion in markdown-mode
  ;; (sp-local-pair 'latex-mode "\\"" nil :actions '(:rem insert))   ;; do not use \" for insert action but use it for any other action
  ;; The '' pair will autopair UNLESS the point is right after a word,
  ;; in which case you want to insert a single apostrophe.
  ;; (sp-pair "'" nil :unless '(sp-point-after-word-p))
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  ;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  )
;;
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m m" . #'mc/edit-lines )
         ("C-c m d" . #'mc/mark-all-dwim )))
;; expand-region
(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))
;; duplicate-thing
(use-package duplicate-thing
  :ensure t
  :defer t
  :diminish "dt"
  :init
  (defun my-duplicate-thing ()
    "Duplicate thing at point without changing the mark."
    (interactive)
    (save-mark-and-excursion (duplicate-thing 1)))
  ;;:bind (("C-c u" . my-duplicate-thing) ("C-c C-u" . my-duplicate-thing))
  )
;; drag-stuff
(use-package drag-stuff
  :ensure t
  :defer t
  :diminish "ds"
  :config
  (drag-stuff-global-mode t)
  )
;; yasnippet
(use-package yasnippet
  :ensure t
  :ensure yasnippet-snippets
  :defer 5
  :diminish "yas"
  :custom (yas-prompt-functions '(yas-completing-prompt))
  :config
  (yas-global-mode)
  (when (package-installed-p 'hydra)
    (defhydra hydra-yasnippet (:color blue :hint nil)
      "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
      ("d" yas-load-directory)
      ("e" yas-activate-extra-mode)
      ("i" yas-insert-snippet)
      ("f" yas-visit-snippet-file :color blue)
      ("n" yas-new-snippet)
      ("t" yas-tryout-snippet)
      ("l" yas-describe-tables)
      ("g" yas-global-mode)
      ("m" yas-minor-mode)
      ("a" yas-reload-all))
    )
  )
;; (dolist (hook '(prog-mode-hook text-mode-hook))
;;   (add-hook hook 'turn-on-column-number-mode)
;;   (add-hook hook 'turn-off-line-number-mode)
;;   (add-hook hook 'linum-mode))
;; magit
(use-package magit
  :ensure t
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g" . magit-status))
(use-package forge
  :ensure t
  :after magit)
;; https://gitlab.com/pidu/git-timemachine
;; M-x git-timemachine-toggle
;; p Visit previous historic version
;; n Visit next historic version
;; w Copy the abbreviated hash of the current historic version
;; W Copy the full hash of the current historic version
;; g Goto nth revision
;; t Goto revision by selected commit message
;; q Exit the time machine.
;; b Run magit-blame on the currently visited revision (if magit available).
;; c Show current commit using magit (if magit available).
(use-package git-timemachine
  :ensure t
  )
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  ;;  (setq diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode t)
  )
;; undo-tree
(use-package undo-tree
  :ensure t
  :defer 5
  :diminish "UT"
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))
;; hydra
(use-package hydra
  :ensure t)
;;;; key-chord
;; The command 'key-chord-describe' lists currently defined key chords.
(use-package key-chord
  :ensure t
  :defer t
  :init
  (key-chord-define-global "''"     "`'\C-b")
  (key-chord-define-global ",,"     'indent-for-comment)
  (key-chord-define-global "qq"     "the ")
  (key-chord-define-global "QQ"     "The ")
  (key-chord-define-global ",."     "<>\C-b")
  (key-chord-define-global "hj"     'undo)
  :config
  (key-chord-mode))
;;; macros
(use-package elmacro
  :ensure t
  :defer t)
(defhydra hydra-macro (:hint nil :color pink :pre
                             (when defining-kbd-macro
                               (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))
;;; defuns
(require 'misc-defuns)
(use-package async :ensure t :defer t)

;;; search
;;;; isearch
;; Emacs 24.4 has a new command isearch-forward-symbol-at-point 【Alt+s .】.
;; It'll interactive search the word under cursor.
;; (【Ctrl+s】 for next occurrence, 【Ctrl+r】 for previous occurrence.)
;;emacs search ＆ highlight commands key	command
;; 【Alt+s .】	isearch-forward-symbol-at-point
;; 【Alt+s _】	isearch-forward-symbol
;; 【Alt+s o】	occur (same as list-matching-lines)
;; 【Alt+s w】	isearch-forward-word
;; 【Alt+s h .】	highlight-symbol-at-point
;; 【Alt+s h f】	hi-lock-find-patterns
;; 【Alt+s h l】	highlight-lines-matching-regexp
;; 【Alt+s h p】	highlight-phrase
;; 【Alt+s h r】	highlight-regexp
;; 【Alt+s h u】	unhighlight-regexp
;; 【Alt+s h w】	hi-lock-write-interactive-patterns
;; 【Ctrl+s】	isearch-forward
;; Quitting Isearch – Many KeySequences quit Isearch. One that many people use is ‘RET’. You cannot use ‘RET’ to search for the end of a line – use ‘C-q C-j’ for that. The cursor is left on the last match.
;; ‘C-g’ – Abort the search, putting back the cursor to its initial position.
;; ‘C-s’ – Repeat the search as many times as you want throughout the buffer.
;; ‘C-w’ – Select the (rest of the) word the TextCursor is on as the search string; repeating ‘C-w’ appends more words to the search string.
;; ‘M-s C-e’ – Select the text up to the end of the line as the search string (this was bound to ‘C-y’ up until Emacs 24.1).
;; ‘M-s h r’ – Highlight regular expression (‘highlight-regexp’)
;; ‘M-s h u’ – Unhighlight regular expression
;; ‘M-s o’ – call ‘occur’ with the current search term
;; ‘C-y’ – Yank (paste) the text last copied to the kill-buffer (clipboard) to the end of the search string (this was bound to ‘M-y’ up until Emacs 24.1).
;; ‘M-n’, ‘M-p’ – Re-use a previous search string. Repeat to choose older strings. ‘M-n’ moves forward in the search history; ‘M-p’ moves backward.
;; ‘M-TAB’ – Complete the current search string against all previous search strings.
;; ‘M-c’ – Toggle search case-sensitivity.
;; ‘M-r’ – Toggle between regular-expression searching and literal-string searching.
;; ‘M-e’ – Pause to edit the search string. Searching is disabled until you explicitly resume it with ‘C-j’ (or ‘C-s’ or ‘C-r’).
;; ‘M-%’ – Start query replace using the current string.
;; ‘C-M-%’ – Start a regular-expression query replace using the current search string.
;;
;;
;;;; occur
;;;;;
;; occur-mode keybindings
;; g - re-run command, ie refresh
;; r - occur-rename-buffer
;; q - quit window
;; e - occur-edit-mode  / M-x occur-cease-edit to stop
;; C-o - occur-mode-display-occurrence : stay in occur window but display occurence
;; C-c C-c    occur-mode-goto-occurrence
;; C-c C-f    next-error-follow-minor-mode
;;
;; M-x multi-occur-in-matching-buffers
;;
;;(define-key isearch-mode-map (kbd "C-o") (lambda () (interactive) (let ((case-fold-search isearch-case-fold-search)) (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(defun undx/occur-mode-hook-fn ()
  "Occur customizations."
  (interactive)
  (occur-rename-buffer))
(add-hook 'occur-mode-hook #'undx/occur-mode-hook-fn)
(define-key occur-mode-map (kbd "n") #'next-logical-line)
(define-key occur-mode-map (kbd "p") #'previous-logical-line)
(defun undx/recenter-line-near-top-fn ()
  "Move current line near top."
  (interactive)
  (let ((recenter-positions '(5)))
    (recenter-top-bottom)))
(add-hook 'occur-mode-find-occurrence-hook #'undx/recenter-line-near-top-fn)


(defun occur-multi-occur ()
  "Start 'multi-occur for the current search term on all buffers with the first matching buffer's major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode
    (with-current-buffer (car (nth 2 occur-revert-arguments)) major-mode))
   (car occur-revert-arguments)))
(define-key occur-mode-map "m" 'occur-multi-occur)


(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their 'major-mode is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; global key for `multi-occur-in-this-mode' - you should change this.
(global-set-key (kbd "C-<f2>") 'multi-occur-in-this-mode)

;; (defun rename-occur-buffer ()
;;   "Rename *Occur* buffers more descriptively."
;;   (interactive) ; if you want to do it manually
;;   (save-excursion
;;     (save-match-data
;;       (goto-char (point-min))
;;       (when
;;           (search-forward-regexp
;;            "^[0-9]+ matches for \"\\(.*\\)\" in buffer: \\(.*\\)$"
;;            (line-end-position)
;;            nil)
;;         (rename-buffer
;;          (format "*Occur %s: %s*" (match-string 2) (match-string 1)))))))
;; (add-hook 'occur-mode-hook #'rename-occur-buffer) ; if you want to do it automatically


;; (defun rename-occur-buffer-term ()
;;   (rename-buffer (format "*Occur: %s*" (car regexp-history))))
;; (add-hook 'occur-hook #'rename-occur-buffer-term)
;;
;;


;;; navigation
;;
;; You can use M-. to jump to definitions, and M-, to jump back to places you've looked from.
;; There's M-?, which can be used to find references in a project.

;; You can also use the mark ring to go back to places you've marked (for
;; instance with C-SPC, or by using isearch) before, but this is limited to a
;; buffer. You can pop the ring with C-u C-SPC. This can be handy, when you need
;; to import something you're about to use before you get back to what you were
;; writing as well.
;;;; projectile
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-completion-system 'ivy
        projectile-mode-line-function
        '(lambda () (format " P[%s]" (or (projectile-project-name) "-"))))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))
;;
;;;; ivy, counsel
(use-package ivy
  :ensure t
  :ensure ivy-hydra
  :init
  (setq ivy-height 15
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-count-format "(%d/%d) "
        ivy-virtual-abbreviate 'abbreviate
        ivy-re-builders-alist '(
                                (ivy-switch-buffer . ivy--regex-plus)
                                (swiper            . ivy--regex-plus)
                                (t                 . ivy--regex-fuzzy))
        )
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . #'ivy-resume)
         ("C-c s"   . #'swiper-thing-at-point)
         ("C-c ."   . #'swiper-thing-at-point)
         ("C-s"     . #'swiper)
         ("C-c u"   . #'swiper-all)
         ))
;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :ensure t
  :hook (ivy-mode . ivy-rich-mode)
  :custom (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-modify-column 'ivy-switch-buffer
                          'ivy-rich-switch-buffer-major-mode
                          '(:width 50 ))
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 50))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 50 :face org-level-5))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))))
  ;; (ivy-rich-modify-columns
  ;;  'ivy-switch-buffer
  ;;  '((ivy-rich-switch-buffer-size (:align right))
  ;;    (ivy-rich-switch-buffer-indicators (:width 4 :align right :face default))
  ;;    (ivy-rich-switch-buffer-major-mode (:width 20 :face default))))
  )

(use-package all-the-icons-ivy-rich
  :ensure t
  :after (all-the-icons ivy-rich)
  :init
  ;; The icon size
  ;;(setq all-the-icons-ivy-rich-icon-size 1.0)
  ;; Slow Rendering
  ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
  ;; you can try setting the following variable
  ;;(setq inhibit-compacting-font-caches t)
  :config
  (all-the-icons-ivy-rich-mode 1))
;; counsel
(use-package counsel
  :ensure t
  :diminish "cs"
  :init
  (counsel-mode 1)
  :custom
  (counsel-find-file-at-point t)
  (counsel-yank-pop-separator (propertize "\n→\n" 'face `(:foreground "#990000" :bold t :size 28)))
  :bind (("C-c ;" . #'counsel-M-x)
         ("C-c U" . #'counsel-unicode-char)
         ("C-c i" . #'counsel-imenu)
         ("C-x f" . #'counsel-find-file)
         ("C-c y" . #'counsel-yank-pop)
         ("C-c r" . #'counsel-recentf)
         ("C-c v" . #'counsel-switch-buffer-other-window)
         ("C-h h" . #'counsel-command-history)
         ("C-x C-f" . #'counsel-find-file)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  )
(use-package counsel-projectile
  :ensure projectile
  :defer t
  :bind (("C-c f" . #'counsel-projectile)
         ("C-c F" . #'counsel-projectile-switch-project)))
;;;; avy
(use-package avy
  :ensure t)
;;;; prescient
;;https://github.com/raxod502/prescient.el
(use-package prescient
  :ensure t
  :ensure ivy-prescient
  :ensure company-prescient
  :after (counsel company)
  :init
  (setq-default prescient-history-length 1000)
  :config
  (ivy-prescient-mode)
  (company-prescient-mode)
  (prescient-persist-mode)
  )
;;;; smartscan
;; M-n and M-p move between symbols and type M-' to replace all symbols in the
;; buffer matching the one under point, and C-u M-' to replace symbols in your
;; current defun only (as used by narrow-to-defun.)
;; (bind-key "M-'" #'other-window smartscan-map)
(use-package smartscan
  :ensure t
  :defer 4
  :config (global-smartscan-mode 1))

;;;; bookmarks
;; bm-show
;; bm-show-all
;; bm-remove-all-current-buffer
;; bm-bookmark-annotate
;; bm-bookmark-show-annotation bm-annotate-on-create
(use-package bm
  :ensure t
  :demand t
  :init
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)
  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)
  ;; Restoring bookmarks
  (add-hook 'find-file-hook    #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle)
         ( "s-<f2>" .  bm-bookmark-regexp))
  )


;;; prog-mode
;;;; specific defuns
(require 'prog-defuns)
;;;; documentation
(use-package eldoc
  :diminish "ed"
  :hook (prog-mode . eldoc-mode))
;;;; flycheck
(use-package flycheck
  :ensure t
  ;;:bind
  :init
  (global-set-key (kbd "C-;") 'flymake-display-err-menu-for-current-line)
  (global-set-key (kbd "C-c n") 'flymake-goto-next-error)
  (global-set-key (kbd "C-c p") 'flymake-goto-prev-error)
  :config
  (global-flycheck-mode))
(use-package flycheck-color-mode-line
  :ensure t
  :disabled t
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  )
;; install  Debian devscript
(use-package flycheck-checkbashisms
  :ensure t
  :init
  ;; Check 'echo -n' usage
  (setq flycheck-checkbashisms-newline t)
  ;; Check non-POSIX issues but required to be supported  by Debian Policy 10.4
  ;; Setting this variable to non nil made flycheck-checkbashisms-newline effects
  ;; regardless of its value
  (setq flycheck-checkbashisms-posix t)
  :config
  (flycheck-checkbashisms-setup))
;;;; completion
;; display from which function completion was done
(setq hippie-expand-verbose t)
(use-package company
  ;;  :ensure company-lsp
  :ensure company-box
  :ensure company-shell
  :ensure company-quickhelp
  :diminish "co"
  :init
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.1
        company-echo-delay 0
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        ;;
        company-backends '(company-capf
                           company-keywords
                           company-semantic
                           company-files
                           company-etags
                           company-elisp
                           company-clang
                           company-yasnippet)
        )
  :config
  (global-company-mode t)
  :bind
  (("<menu>" . company-complete)
   ("C-:" . company-complete) )
  )
(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-idle-delay 0.1)
  (company-quickhelp-mode 1))
(use-package company-box
  :after company
  :diminish " company box"
  :init
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  ;; Disable tab-bar in company-box child frames
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))
  (company-box-mode)
  )
;;;; folding
(use-package origami
  :ensure t
  :defer t
  :diminish "or"
  :bind
  ("M-m" . origami-toggle-node)
  :init
  (setq origami-show-fold-header t)
  :config
  (global-origami-mode))
;;; json
(use-package json-mode
  :ensure t
  :mode
  ("\\.json\\'" . json-mode)
  ("\\.js\\'" . json-mode))
(use-package counsel-jq
  :ensure t
  :defer t)
;;;; rest
(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.restclient$" . restclient-mode))
;;;; rust
;; (use-package rust-mode :ensure t :defer t :init)
;; (use-package flycheck-rust :ensure t :defer t :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
;;;; lua
(use-package lua-mode :ensure t :mode ("\\.lua$" . lua-mode))
;;; org-mode
;;;; main
(use-package org
  :hook ((org-mode . prettify-symbols-mode)
         ;;(org-mode . visual-line-mode)
         ;;(org-mode . variable-pitch-mode)
         )
  :init
  (require 'org-defuns)
  (setq org-directory "~/notes/"
        org-roam-directory "~/notes/work/"
        ;; Keep the indentation well structured by setting
        ;; Non-nil means turn on org-indent-mode on startup.
        ;; Local file variables:
        ;; #+STARTUP: indent
        ;; #+STARTUP: noindent
        org-startup-indented t
        org-indent-indentation-per-level 2
        ;; Leave everything at column 0, org-indent will show indentation correctly
        org-adapt-indentation nil
        ;;
        org-src-tab-acts-natively t
        ;; allow some text manipulations w/ cursor
        ;; - on a headline, changing TODO state (left/right) and priority (up/down)
        ;; - on a time stamp, changing the time
        ;; - in a plain list item, changing the bullet type
        ;; - in a property definition line, switching between allowed values
        ;; - in the BEGIN line of a clock table (changing the time block).
        org-support-shift-select t
        ;; show markers
        org-hide-emphasis-markers nil
        ;; display images
        org-startup-with-inline-images t
        ;; TODO check this variable
        org-indent-indentation-per-level 1
        ;; don't confirm babel exec
        org-confirm-babel-evaluate nil
        ;; Show inline images by default
        org-startup-with-inline-images t

        ;; Record time and note when a task is completed
        org-log-done 'time
        ;; Record time and note when the scheduled date of a task is modified
        org-log-reschedule 'time
        ;; Record time and note when the deadline of a task is modified
        org-log-redeadline 'time
        ;; Record time and note when clocking out of a task
        org-log-clock-out 'time
        ;; Record time and note when a task is refiled
        org-log-refile 'time
        ;; Record note when clocking out
        org-log-note-clock-out t
        ;; Log everything into the LOGBOOK drawer
        org-log-into-drawer t

        ;; archiving
        org-archive-location "archive.org::datetree/"

        org-todo-keywords (quote ((sequence "TODO" "NEXT" "WIP_" "HOLD" "PR__" "QA__" "|" "DONE" "REJECTED")
                                  (sequence "MEETING" "PHONE" "EVENT")
                                  (sequence "TODO(t@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "|" "ASSIGNED(.@/!)" "CANCELLED(x@/!)" "DONE(d@/!)" )
                                  ))
        org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
                                             ("WAITING" ("WAITING" . t))
                                             ("HOLD" ("WAITING") ("HOLD" . t))
                                             (done ("WAITING") ("HOLD"))
                                             ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                             ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                             ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
        org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                       ("NEXT" :foreground "royal blue" :weight bold)
                                       ("WIP_" :foreground "blue" :weight bold)
                                       ("DELEGATED" :foreground "blue" :weight bold)
                                       ("ASSIGNED" :foreground "blue" :weight bold)
                                       ("WAITING" :foreground "orange" :weight bold)
                                       ("HOLD" :foreground "orange" :weight bold)
                                       ("PR__" :foreground "orange" :weight bold)
                                       ("QA__" :foreground "orange" :weight bold)
                                       ("DONE" :foreground "forest green" :weight bold)
                                       ("CANCELLED" :foreground "forest green" :weight bold)
                                       ("REJECTED" :foreground "forest green" :weight bold)
                                       ("MEETING" :foreground "forest green" :weight bold)
                                       ("EVENT" :foreground "forest green" :weight bold)
                                       ("PHONE" :foreground "forest green" :weight bold)))

        ;; Priority mapping w/ jira
        ;; A Blocker
        ;; A Critical
        ;; B Major
        ;; C Minor
        ;; D Trivial
        org-priority-highest ?A
        org-priority-lowest ?D
        org-priority-faces '((?A . 'all-the-icons-red)
                             (?B . 'all-the-icons-orange)
                             (?C . 'all-the-icons-yellow)
                             (?D . 'all-the-icons-silver))
        ;; Default tags available in Org files.
        org-tag-alist (quote (("@errand" . ?e)
                              ("@office" . ?o)
                              ("@home" . ?h)
                              ("@school" . ?s)
                              (:newline)
                              ("WAITING" . ?w)
                              ("HOLD" . ?H)
                              ("CANCELLED" . ?c)))
        ;; Tags always available in Org files.
        ;;org-tag-persistent-alist (quote( ))
        ;;
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        ;;org-structure-template-alist
        org-structure-template-alist '(("s" . "src")
                                       ("e" . "example")
                                       ("q" . "quote")
                                       ("v" . "verse")
                                       ("ht" . "export html")
                                       ("b" . "src bash")
                                       ("j" . "src java")
                                       ("y" . "src yaml")
                                       ("l" . "src emacs-lisp")
                                       ("p" . "src python"))
        ;; org-babel-default-header-args
        org-babel-default-header-args '((:session . "none")
                                        (:results . "replace")
                                        (:exports . "both")
                                        (:cache . "no")
                                        (:noweb . "no")
                                        (:hlines . "no")
                                        (:tangle . "no")
                                        (:comments . "link"))

        org-link-abbrev-alist '(("jira"   . "https://jira.talendforge.org/browse/")
                                ("github" . "https://www.github.com/")
                                ("google" . "http://www.google.com/search?q=")
                                )

        )
  ;; Directories
  (defvar org-inbox-dir    (concat org-directory "inbox/")   "Inbox directory.")
  (defvar org-events-dir   (concat org-directory "events/")   "Events directory.")
  (defvar org-work-dir     (concat org-directory "work/")     "Work related directory.")
  (defvar org-personal-dir (concat org-directory "personal/") "Personal stuff directory.")
  ;; Org files
  (defvar org-inbox-file    (concat org-inbox-dir "gtd.org")  "New stuff collects in this file.")

  (defvar org-default-projects-dir  (concat org-directory "projects/")  "Primary GTD directory.")
  (defvar org-default-completed-dir  "~/projects/trophies"            "Directory of completed project files.")

  (defvar org-default-notes-file    (concat org-directory "notes.org") "Personal notes.")

  (defvar org-work-journal-file     (concat org-directory "work/journal.org"))
  (defvar org-work-howto-file       (concat org-directory "work/howto.org"))
  (defvar org-work-tacokit-file     (concat org-directory "work/tacokit.org"))
  (defvar org-diary-file            (concat org-directory "personal/life.org"))
  (defvar org-personal-buy-log-file (concat org-directory "personal/journal_achats.org"))
  (defvar org-personal-stratif-file (concat org-directory "personal/stratif.org"))
  (defvar org-personal-account-file (concat org-directory "personal/accounts/ledger.org"))
  ;; export settings
  (defvar org-export-setup-dir             (concat org-directory "_setup/") "Templates for export.")
  (defvar org-export-output-directory-prefix "export_" "Prefix of directory used for org export.")

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (js . t)
     (clojure . t)
     (java . t)
     (dot . t)
     (plantuml . t)
     (restclient . t)
     ))
  (linum-mode -1)
  (require 'org-habit nil :noerror)
  (require 'org-tempo nil :noerror)
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  ;;  (add-to-list 'org-structure-template-alist '("p" ":PROPERTIES:?:END:"))
  ;; When you save a file, the auto save file is deleted. If pb M-x recover-file
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)))
;;
;; preface todo task with verb
;; give context to task:
;; - priority: How important is it for me to get this done?
;; - location:
;; - effort estimate: If I only have 30 minutes of available time remaining, I’d prefer to work on tasks that take an estimate of no more than 30 minutes, than embark on a large task that I would have to pause on.
;; - project: it helps to group/classify these tasks so I can work on clearing related tasks together.
;; - date : Perhaps this task has a due date, or can only be performed on a date specific.

;;;; org-capture
(use-package org-capture
  :config
  (require 'org-protocol)
  (setq org-capture-templates
        `(
          ("i" "inbox"   entry (file+headline org-inbox-file "Tasks") "** TODO %?")
          ("l" "link"    entry (file+headline org-inbox-file "Links") "** TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "capture" entry (file+headline org-inbox-file "Notes") "** TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
          )
        ;;
        ;;   Here are the available contexts definitions:
        ;;       in-file: command displayed only in matching files
        ;;       in-mode: command displayed only in matching modes
        ;;   not-in-file: command not displayed in matching files
        ;;   not-in-mode: command not displayed in matching modes
        ;;     in-buffer: command displayed only in matching buffers
        ;; not-in-buffer: command not displayed in matching buffers
        ;;    [function]: a custom function taking no argument
        org-capture-templates-contexts
        '(
          ;; For example, if you have a capture template "c" and you want this template to be
          ;; accessible only from message-mode buffers,
          ;; use this:
          ("c"     ((in-mode . "message-mode")))
          ;; You can also bind a key to another capture template depending on contextual rules.
          ;; here d calls template c
          ("c" "d" ((in-mode . "message-mode")))
          )
        )
  )
;;;; org-agenda
(use-package org-agenda
  :config
  (require 'agenda-defuns)
  (setq org-agenda-files (append `(,org-inbox-file
                                   ,(concat org-events-dir "gcal.org")
                                   ,(concat org-events-dir "schedule.org")
                                   )
                                 (directory-files-recursively "~/notes/work/daily" "org$")
                                 (directory-files-recursively "~/notes/work/jira" "org$")
                                 ;;(file-expand-wildcards "~/prj/*/doc")
                                 )
        ;; Switch window when opening org-agenda.
        org-agenda-window-setup 'other-window
        ;; Display indirect buffers in the "current" window.
        org-indirect-buffer-display 'current-window
        ;; week starts on monday
        org-agenda-start-on-weekday 1
        ;; date style
        calendar-date-style 'european
        calendar-week-start-day 1
        calendar-latitude 48.2
        calendar-longitude 3.2833
        calendar-location-name "Sens, France"
        ;; day, week, month, year, or any number of days.
        org-agenda-span 'week
        ;; include diary in agenda
        org-agenda-include-diary t
        ;; set them by myself
        calendar-holidays '((holiday-fixed 1 1 "Jour de l'an")
                            (holiday-fixed 1 6 "Épiphanie")
                            (holiday-fixed 2 2 "Chandeleur")
                            (holiday-fixed 2 14 "Saint Valentin")
                            (holiday-fixed 5 1 "Fête du travail")
                            (holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
                            (holiday-fixed 6 21 "Fête de la musique")
                            (holiday-fixed 7 14 "Fête nationale - Prise de la Bastille")
                            (holiday-fixed 8 15 "Assomption (Religieux)")
                            (holiday-fixed 11 1 "Toussaint")
                            (holiday-fixed 11 11 "Armistice de 1918")
                            (holiday-fixed 12 25 "Noël")
                            (holiday-easter-etc 0 "Pâques")
                            (holiday-easter-etc 1 "Lundi de Pâques")
                            (holiday-easter-etc 39 "Ascension")
                            (holiday-easter-etc 49 "Pentecôte")
                            (holiday-easter-etc -47 "Mardi gras")
                            (holiday-float 5 0 4 "Fête des mères")
                            (holiday-float 6 0 3 "Fête des pères"))

        ;; Agenda Custom Commands :  (key desc type match settings files)
        ;; navigation
        ;; n-p : next/previous line
        ;; N-P : next/previous item
        ;;
        ;; action
        ;; t: Cycle the TODO state of the current item.
        ;; ,: Apply a specific priority (you’ll be prompted in the minibuffer).
        ;; + and -: Increase or decrease priority, respectively.
        ;; S-Left and S-Right: Shift date or time of item at point forward or backward. (DEADLINE and SCHEDULED).
        ;; s: Save all agenda buffers.
        ;; g: Rebuild all agenda views in the current buffer.
        ;; z: add a note to current entry
        ;;
        ;; Drilling Down
        ;; RET: Switch to the current entry in this window.
        ;; TAB: Switch to the current entry in a new split window.
        ;; SPC: Show the current entry in a new split window with highlighting.
        ;; F: Follow mode (persist the effect of SPC as point moves).
        ;;
        ;;UI
        ;; org-agenda-prefix-format
        ;;
        ;; %c the category of the item, "Diary" for entries from the diary, or as given by the CATEGORY keyword or
        ;;    derived from the file name
        ;; %e the effort required by the item
        ;; %l the level of the item (insert X space(s) if item is of level X)
        ;; %i the icon category of the item, see org-agenda-category-icon-alist
        ;; %T the last tag of the item (ignore inherited tags, which come first)
        ;; %t the HH:MM time-of-day specification if one applies to the entry
        ;; %s Scheduling/Deadline information, a short string
        ;; %b show breadcrumbs, i.e., the names of the higher levels
        ;; %(expression) Eval EXPRESSION and replace the control string by the result


        org-agenda-custom-commands
        '(
          ("c" "Simple agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "")
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or  (air-org-skip-subtree-if-habit)
                             (air-org-skip-subtree-if-priority ?A)
                             (org-agenda-skip-if nil '(scheduled deadline))))))))

          ("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                     (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))
          )
        )
  ;;  (define-key org-agenda-mode-map "J" 'air-org-agenda-next-header)
  ;;  (define-key org-agenda-mode-map "K" 'air-org-agenda-previous-header)
  )
;;;; org-roam
(use-package org-roam
  :ensure t
  :after org
  :diminish "roam"
  :init
  (setq org-roam-dailies-capture-templates
        '(
          ("d" "daily" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n\n")

          ("l" "lab" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n"
           :olp ("Lab notes"))

          ("j" "journal" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n"
           :olp ("Journal"))))

  :config
  (org-roam-mode)
  (require 'org-roam-protocol)
  ;;(org-roam-dailies-find-today)
  )
;;;; org-jira
;; (define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
;; (define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
;; (define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
;; (define-key org-jira-map (kbd "C-c ij") 'org-jira-get-issues-from-custom-jql)
;; (define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
;; (define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
;; (define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
;; (define-key org-jira-map (kbd "C-c in") 'org-jira-progress-issue-next)
;; (define-key org-jira-map (kbd "C-c ia") 'org-jira-assign-issue)
;; (define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
;; (define-key org-jira-map (kbd "C-c iR") 'org-jira-refresh-issues-in-buffer)
;; (define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
;; (define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
;; (define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
;; (define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
;; (define-key org-jira-map (kbd "C-c cc") 'org-jira-add-comment)
;; (define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
;; (define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklogs-from-org-clocks)
;; (define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
;; (define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-by-fixversion)
(use-package org-jira
  :ensure t
  :after (org)
  :init
  ;; Priority mappings
  ;; A Blocker
  ;; A Critical
  ;; B Major
  ;; C Minor
  ;; C Trivial
  (setq org-jira-working-dir (concat org-roam-directory "jira")
        jiralib-url "https://jira.talendforge.org/"
        org-jira-priority-to-org-priority-alist '(
                                                  ("Blocker" . ?A)
                                                  ("Critical" . ?A)
                                                  ("Major" . ?A)
                                                  ("Minor" . ?B)
                                                  ("Trivial" . ?C)
                                                  )
        org-jira-jira-status-to-org-keyword-alist '(
                                                    ("New" . "TODO")
                                                    ("Accepted" . "NEXT")
                                                    ("Candidate" . "TODO")
                                                    ("Reopened" . "TODO")
                                                    ("On hold" . "HOLD")
                                                    ("In Progress" . "WIP_")
                                                    ("Code Review" . "PR__")
                                                    ("QA" . "QA__")
                                                    ("Validation" . "QA__")
                                                    ("Done" . "DONE")
                                                    ("Closed" . "DONE")
                                                    ("Rejected" . "REJECTED")
                                                    )

        org-jira-custom-jqls '(
                               (:jql
                                "
status     changed BY currentuser() AFTER startOfDay(-1) BEFORE startOfDay() OR
resolution changed BY currentuser() AFTER startOfDay(-1) BEFORE startOfDay() OR
fixVersion changed BY currentuser() AFTER startOfDay(-1) BEFORE startOfDay() OR
assignee   changed BY currentuser() AFTER startOfDay(-1) BEFORE startOfDay()
 "
                                :limit 50
                                :filename "worked-on-yesterday")
                               (:jql
                                "
status     changed BY currentuser() AFTER startOfDay() BEFORE endOfDay() OR
resolution changed BY currentuser() AFTER startOfDay() BEFORE endOfDay() OR
fixVersion changed BY currentuser() AFTER startOfDay() BEFORE endOfDay() OR
assignee   changed BY currentuser() AFTER startOfDay() BEFORE endOfDay()
 "
                                :limit 50
                                :filename "worked-on-today")
                               (:jql
                                "
status     changed BY currentuser() AFTER startOfWeek() BEFORE endOfWeek() OR
resolution changed BY currentuser() AFTER startOfWeek() BEFORE endOfWeek() OR
fixVersion changed BY currentuser() AFTER startOfWeek() BEFORE endOfWeek() OR
assignee   changed BY currentuser() AFTER startOfWeek() BEFORE endOfWeek()
 "
                                :limit 50
                                :filename "worked-on-week")
                               ))
  :config
  (with-eval-after-load 'org-jira (progn
                                    (message "loaded org-jira")
                                    (require 'jira-defuns))
                        )
  )
;;;; org-gcal
(use-package org-gcal
  :ensure t
  :after (org auth-source-pass)
  :config
  (setq org-gcal-up-days 90
        org-gcal-down-days 90
        org-gcal-auto-archive nil
        org-gcal-client-id (auth-source-pass-get 'secret "Home/Gcal/client-id")
        org-gcal-client-secret (auth-source-pass-get 'secret "Home/Gcal/client-secret")
        org-gcal-file-alist '(("emmanuel.gallois@gmail.com" . "~/notes/events/gcal.org")))

  ;;  (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
  ;;  (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch))
  )
;;;; excorporate
(use-package excorporate
  :ensure t
  :after org
  :init
  (setq excorporate-configuration (quote ("egallois@talend.com" . "https://outlook.office365.com/EWS/Exchange.asmx")))
  )

(defun undx/agenda-updates()
  "Update all external resources for agenda."
  (interactive)
  ;; activate excorporate and request user/password to start connection
  (excorporate)
  ;; enable the diary integration (i.e. write exchange calendar to emacs diary file -> ~/.emacs.d/diary must exist)
  (excorporate-diary-enable)

  (require ‘excorporate)
  (defadvice exco-org-insert-meetings (after exco-org-save-meetings activate)
    (with-current-buffer (get-buffer-create excorporate-org-buffer-name)
      (when (file-exists-p “~/.emacs.d/excorporate.tmp.org”)
        (delete-file “~/.emacs.d/excorporate.tmp.org” nil))
      (write-file “~/.emacs.d/excorporate.tmp.org” nil)
      (rename-buffer excorporate-org-buffer-name)))
  (add-to-list ‘org-agenda-files “~/.emacs.d/excorporate.tmp.org”)
  )

;;;; org-superstar
;; ‘org-superstar-leading’.  Otherwise, this variable has no effect and ‘org-mode’ covers leading stars using
;; ‘org-hide’.  See also ‘org-indent-mode-turns-on-hiding-stars’.
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("✿"  "◉" "○" "✸" "▷"))
  (setq org-superstar-item-bullet-alist
        '((?+ . ?•)
          (?* . ?➤)
          (?- . ?➤)))
  )
;;;; babel, ox-exports and other libs
(use-package ob-restclient :ensure t :defer t)
(use-package plantuml-mode :ensure t :defer t)
(use-package flycheck-plantuml :ensure t :defer t)
;; exporters
(use-package ox-gfm :ensure t :after org)
(use-package ox-slack :ensure t :after org)
(use-package ox-jira :ensure t :after org)
(use-package ox-asciidoc :ensure t :after org)
(use-package ox-json :ensure t :after org)
;; https://github.com/alphapapa/org-ql
(use-package org-ql :ensure t :defer t)
;;; slack
(use-package slack
  :ensure t
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "talend"
   :default t
   :token (auth-source-pass-get 'secret "Work/Slack/token")
   :subscribed-channels '(test-rename rrrrr)
   :full-and-display-names t))

;;; system settings
;;;; GCMH - the Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :diminish " gc"
  :config (gcmh-mode 1))
;;;; helpful
(use-package helpful
  :ensure t
  :init
  :after (counsel)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;;; dired
(use-package dired
  :custom
  (dired-listing-switches "-AFhl --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies (quote always))
  (dired-recursive-deletes (quote top))

  :hook (dired-mode . hl-line-mode))
;;;; sudo edit
(use-package sudo-edit
  :ensure t
  :defer t)
;;;; exec from shell
;; Set the path variable
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :defer t
    :config (exec-path-from-shell-initialize)))
;;;; encryption
(use-package epa
  :init
  (setq epa-file-select-keys 2
        epa-file-cache-passphrase-for-symmetric-encryption t))
;;;; auth-source-pass
(use-package auth-source-pass
  :ensure t
  :after (epa)
  :config
  (auth-source-pass-enable))
;; Ignore errors if work file isn't found.
;; (require 'work-init nil 'noerror)


;;; keybindings
;;  Control (‘C-’),
;;  Shift (‘S-’),
;;  Meta (‘M-’),
;;  Alt (‘A-’),
;;  Super (‘s-’), macOS
;;  Hyper (‘h-’)
;; ----------
;; unuseful keybindings some
;; M-a backward-sentence
;; M-e forward-sentence
;; M-m back-to-indentation
;; M-o specific
;; M-t transpose-word -- very misleading
;; C-o open-line
;;;; undefine some keys
(global-unset-key (kbd "<f1>"))
;;(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f8>"))
(global-unset-key (kbd "<f9>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-z"))
;; get rid of these binding
;;(dolist (key '("\C-c w 0" "\C-c w 1" "\C-c w 2" "\C-c w 3" "\C-c w 4" "\C-c w 5" "\C-c w 6" "\C-c w 7" "\C-c w 8" "\C-c w 9" "\M-o")) (global-unset-key key))

(defun insert-empty-line ()
  "Insert an empty line after the current line and position the cursor on its beginning."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))
(global-set-key [(shift return)] 'insert-empty-line)

(global-set-key (kbd "M-<return>") 'comment-indent-new-line)
(global-set-key [remap move-beginning-of-line] 'undx/move-beginning-of-line)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "M-J") (lambda () (interactive) (join-line +1)))
;; "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)
;; M-k kills to the left
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "M-/") 'hippie-expand)


;;;; ivy keybindings
;;
;;(global-set-key (kbd "C-x x") 'swiper);; TODO redefine
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x b")  'ivy-switch-buffer)
(global-set-key (kbd "C-x B")  'counsel-buffer-or-recentf)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C") 'counsel-compile);; no C-c c it's org-capture !!!
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-h h") 'counsel-command-history)
(global-set-key (kbd "<f11>")   'counsel-bookmark)
(global-set-key (kbd "<C-f11>") 'bookmark-set)
(global-set-key (kbd "M-<f11>") 'bm-next)
(global-set-key (kbd "<S-f11>") 'bm-previous)
;;
;;
;;Ivy-based interface to shell and system tools
(global-set-key (kbd "M-i")     'counsel-imenu)
(global-set-key (kbd "M-I")     'ivy-imenu-anywhere)
(global-set-key (kbd "C-c n") 'counsel-fzf)
;;Use C-M-j (ivy-immediate-done).
;; Helpful if you want to create a new file, but a file already exists that matches the desired name.

;; (global-set-key "\C-s" 'swiper)
;;(define-key swiper-map         [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; see https://oremacs.com/swiper/#minibuffer-key-bindings
;; Key bindings for single selection, action, then exit minibuffer
;;
;; C-m or RET (ivy-done) : Calls the default action and then exits the minibuffer.
;; M-o (ivy-dispatching-done):  Presents valid actions from which to choose. When only one action is available, there is no difference between M-o and C-m.
;; C-j (ivy-alt-done) : When completing file names, selects the current directory candidate and starts a new completion session there. Otherwise, it is the same as ivy-done.
;; TAB (ivy-partial-or-done) : Attempts partial completion, extending current input as much as possible. TAB TAB is the same as C-j (ivy-alt-done).
;; C-M-j (ivy-immediate-done): Exits with the current input instead of the current candidate (like other commands).
;;This is useful e.g. when you call find-file to create a new file, but the desired name matches an existing file. In that case, using C-j would select that existing file, which isn't what you want - use this command instead.
;; C-' (ivy-avy): Uses avy to select one of the candidates on the current candidate page. This can often be faster than multiple C-n or C-p keystrokes followed by C-m;;
;;
(global-set-key (kbd "M-c") 'duplicate-thing)

(global-set-key (kbd "M-<up>")    #'drag-stuff-up)
(global-set-key (kbd "M-<down>")  #'drag-stuff-down)
(global-set-key (kbd "M-<left>")  #'drag-stuff-left)
(global-set-key (kbd "M-<right>") #'drag-stuff-right)


;;;; langs
;;;
;; restore eval and print in emacs-lisp-mode
(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)

;;;; osx
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super)   ; command as super
  (setq mac-option-modifier 'meta)     ; alt as meta
  (setq mac-control-modifier 'control) ; control as... control
  (bind-key "<home>" #'move-beginning-of-line)
  (bind-key "<end>" #'move-end-of-line)
  (bind-key "s-w" #'kill-this-buffer)
  ;; keyboard-quit doesn’t exit the minibuffer, so I give abort-recursive-edit
  (bind-key "s-g" #'abort-recursive-edit)
  (unbind-key "s-t"))
;;; post startup

(provide 'init)
;;; init.el ends here
