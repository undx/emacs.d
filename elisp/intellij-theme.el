;;; intellij-theme.el --- Inspired by IntelliJ's default theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Emmanuel GALLOIS
;; Copyright (C) 2016 Vladimir Polushin
;; Copyright (C) 2015 - 2016  Sam Halliday

;; Author: Vladimir Polushin <vovapolu@gmail.com>
;; Author of base darcula theme: Sam Halliday <Sam.Halliday@gmail.com>
;; Keywords: faces
;; Package-Version: 20171017.1415
;; Package-Commit: 1bbfff8e6742d18e9b77ed796f44da3b7bd10606

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(deftheme intellij
  "Inspired by IntelliJ's default theme")

;; "C-u C-x =" useful for inspecting misbehaving faces.
;; "M-x list-faces-display" useful for listing everything that new major modes introduce.
(require 'color)
(custom-theme-set-variables
 'intellij
 `(ensime-sem-high-faces
   ;; NOTE: Inconsolata doesn't have italics
   ;; FURTHER NOTE: these are overlays, not faces
   `((var . (:foreground "#000000" :underline (:style wave :color "yellow")))
     (val . (:foreground "#000000"))
     (varField . (:foreground "#600e7a" :slant italic))
     (valField . (:foreground "#600e7a" :slant italic))
     (functionCall . (:foreground "#000000" :slant italic))
     (implicitConversion . (:underline (:color "#c0c0c0")))
     (implicitParams . (:underline (:color "#c0c0c0")))
     (operator . (:foreground "#000080"))
     (param . (:foreground "#000000"))
     (class . (:foreground "#20999d"))
     (trait . (:foreground "#20999d" :slant italic))
     (object . (:foreground "#5974ab" :slant italic))
     (package . (:foreground "#000000"))
     (deprecated . (:strike-through "#000000"))
     ))
 )


(let (
      (fg "#000000")
      (bg "#ffffff")
      ;;
      (cursorBg "#5974ab")
      (gutterBg "#f2f2f2")

      (modelineBg "#f2f2f2")
      (statusBarBg "#f2f2f2")
      (statusBarFg "#000000")
      (statusBarBorder "#c0c0c0")

      (selectionFg "#ffffff")
      (selectionActive "#a6d2ff")
      (selectionInactive "#d5d5d5")
      (selectionBackground  "#2675BF")

      (eldocHeaderBg "#f1f1f1")
      (eldocBg "#f6f6f6")
      ;;  menubg      #E6EBF0
      ;; comment     #CBC6BC

      (highlightBg "#fffae3")
      (linenumberFg "#adadad")

      (completionActive "#c5dffc")
      (completionInactive "#f7f7f7")

      (errorColor "#c7222d")
      (warningBg "#f6ebbc")
      (warningStripe "#ebc700")

      (foldedFg "#414d41")
      (foldedBg "#e9f5e6")

      (tabInactiveBg "#d4d4d4")
      (tabInactiveFg "#333333")
      (tabActiveBorder "#c0c0c0")
      (tabBg  "#f2f2f2")
      (tabBorder "#c0c0c0")

      (parenMatch "#93D9D9")
      )

  (custom-theme-set-faces
   'intellij
   ;; default
   `(default ((t (:inherit nil :stipple nil :background ,bg :foreground ,fg :inverse-video nil :box nil :strike-through nil
                           :overline nil :underline nil :slant normal :weight normal :width normal :foundry nil))))
   ;; native UI
   `(cursor ((t (:foreground ,bg :background ,cursorBg ))))
   `(minibuffer-prompt ((t (:weight bold :slant normal :underline nil :inverse-video nil :foreground "#259185"))))
   `(highlight ((t (:background ,highlightBg))))
   `(region ((t (:weight normal :slant normal :underline nil :foreground ,fg :background ,selectionActive))))
   `(mode-line ((t (:weight normal :slant normal :underline nil :box nil :inverse-video t :foreground ,fg :background ,modelineBg))))
   `(mode-line-inactive ((t (:weight normal :slant normal :underline nil :box nil :inverse-video t :foreground "#888888" :background "white smoke" :inherit (mode-line)))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-highlight ((((class color) (min-colors 88))) (t (:inherit (highlight)))))
   `(fringe ((t :background ,gutterBg)))
   `(linum  ((t :foreground ,linenumberFg :background ,gutterBg)))
   `(header-line ((t :inverse-video nil :background ,gutterBg :foreground ,fg :box nil )))
   `(which-func ((t :background nil)))

   `(warning ((t (:inherit 'default :underline (:style wave :color ,warningStripe)))))
   `(error   ((t (:inherit 'default :underline (:style wave :color ,errorColor)))))
   `(compilation-error ((t (:inherit 'default :foreground ,errorColor :underline ,errorColor))))

   `(scala-font-lock:var-face ((t (:foreground "#600e7a" :underline (:style wave :color "yellow") :inherit 'font-lock-variable-name-face))))
   `(sbt:error ((t (:inherit 'default :foreground "red"))))
   `(maker:error ((t (:inherit 'default :foreground "red"))))
   `(ensime-warnline-highlight ((t (:inherit 'font-lock-warning-face))))
   `(ensime-compile-infoline ((t (:foreground "#404040" :inherit 'default))))

   ;; font-lock-warning-face
   ;; for a construct that is peculiar (e.g., an unescaped confusable quote in an Emacs Lisp symbol like ‘‘foo’), or that greatly changes the meaning of other text, like ‘;;;###autoload’ in Emacs Lisp and ‘#error’ in C.

   ;; font-lock-function-name-face
   ;; for the name of a function being defined or declared.

   ;; font-lock-variable-name-face
   ;; for the name of a variable being defined or declared.

   ;; font-lock-keyword-face
   ;; for a keyword with special syntactic significance, like ‘for’ and ‘if’ in C.

   ;; font-lock-comment-face
   ;; for comments.

   ;; font-lock-comment-delimiter-face
   ;; for comments delimiters, like ‘/*’ and ‘*/’ in C. On most terminals, this inherits from font-lock-comment-face.

   ;; font-lock-type-face
   ;; for the names of user-defined data types.

   ;; font-lock-constant-face
   ;; for the names of constants, like ‘NULL’ in C.

   ;; font-lock-builtin-face
   ;; for the names of built-in functions.

   ;; font-lock-preprocessor-face
   ;; for preprocessor commands. This inherits, by default, from font-lock-builtin-face.

   ;; font-lock-string-face
   ;; for string constants.

   ;; font-lock-doc-face
   ;; for documentation strings in the code. This inherits, by default, from font-lock-string-face.

   ;; font-lock-negation-char-face
   ;; for easily-overlooked negation characters.


   ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
   `(font-lock-warning-face ((t (:underline (:style wave :color "orange" :inherit 'default)))))
                                        ;for a construct that is peculiar, or that greatly changes the meaning of other text.
   `(font-lock-function-name-face ((t (:foreground ,fg :inherit 'default :weight bold))))
                                        ;for the name of a function being defined or declared.
   `(font-lock-variable-name-face ((t (:inherit 'default))))
                                        ;for the name of a variable being defined or declared.

   `(font-lock-keyword-face ((t (:foreground "#000090" :weight extra-bold :inherit 'default))))
                                        ;for a keyword with special syntactic significance, like ‘for’ and ‘if’ in C.
   `(font-lock-comment-face ((t (:foreground "#808080" :slant italic :inherit 'default))))
                                        ;for comments.
   `(font-lock-comment-delimiter-face ((t (:foreground "#808080" :inherit 'default))))
                                        ;for comments delimiters, like ‘/*’ and ‘*/’ in C.
   `(font-lock-type-face ((t (:foreground "#20999d" :inherit 'default))))
                                        ;for the names of user-defined data types.
   `(font-lock-constant-face ((t (:foreground "#5974ab" :weight bold :inherit 'font-lock-variable-name-face))))
                                        ;for the names of constants, like ‘NULL’ in C.
   `(font-lock-builtin-face ((t (:inherit 'font-lock-keyword-face :slant italic ))))
                                        ;for the names of built-in functions.
   `(font-lock-preprocessor-face ((t (:inherit 'font-lock-builtin-face :foreground "#a57705"))))
                                        ;for preprocessor commands.
   `(font-lock-string-face ((t (:foreground "#008000" :weight bold :inherit 'default))))
                                        ;for string constants.
   `(font-lock-doc-face ((t (:foreground "#808080" :inherit 'font-lock-comment-face))))
                                        ;for documentation strings in the code.
   `(font-lock-negation-char-face ((t (:underline (:color foreground-color :style line) :inherit 'default))))
                                        ;for easily-overlooked negation characters.

   `(show-paren-match ((t :background ,parenMatch :weight extra-bold)))
   `(show-paren-mismatch (( t :background ,errorColor :weight extra-bold)))

   `(flymake-errline ((t (:inherit 'error))))
   `(flymake-warnline ((t (:inherit 'warning))))
   `(flycheck-fringe-error ((t :background ,errorColor)))
   `(flycheck-fringe-warning ((t :background ,warningBg)))

   `(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
   `(shadow ((t (:foreground "#465a61"))))
   `(secondary-selection ((t (:background "#0a2832"))))
   `(trailing-whitespace ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#c60007" :background "red1"))))
   `(button ((t (:inherit (link)))))
   `(link ((t (:weight normal :slant normal :underline (:color foreground-color :style line) :inverse-video nil :foreground "#5859b7"))))
   `(link-visited ((t (:weight normal :slant normal :underline (:color foreground-color :style line) :inverse-video nil :foreground "#c61b6e" :inherit (link)))))
   `(header-line ((t (:weight normal :slant normal :underline nil :box nil :inverse-video t :foreground "#708183" :background "#0a2832" :inherit (mode-line)))))

   `(tooltip ((((class color)) (:foreground ,fg :background ,eldocBg))))

   `(popup-menu-face ((t (:inherit 'mode-line))))
   `(popup-menu-selection-face ((t (:inherit 'highlight))))
   `(popup-face ((t (:inherit 'mode-line))))
   `(popup-menu-summary-face ((t (:inherit 'mode-line :weight bold))))
   `(popup-summary-face ((t (:inherit 'mode-line :weight bold))))
   `(ac-candidate-face ((t (:inherit 'mode-line))))
   `(ac-selection-face ((t (:inherit 'highlight))))

   `(company-tooltip ((t :background ,modelineBg  :foreground ,fg )))
   `(company-tooltip-common ((t :inherit font-lock-keyword-face :background ,completionActive :underline t)))
   ;; current selected item
   `(company-tooltip-selection ((t :foreground ,fg :weight extra-bold)))
   ;; matching search
   `(company-tooltip-common-selection ((t :foreground ,fg :background ,completionActive :underline t)))
   `(company-scrollbar-fg      ((t :inherit company-tooltip)))
   `(company-scrollbar-bg      ((t :inherit company-tooltip)))
   ;;   `(company-template-field ((t :inherit company-tooltip :foreground "orange")))
   `(company-tooltip-annotation ((t :inherit company-tooltip :foreground "yellow")))

   `(org-code ((t (:inherit 'default))))
   `(org-block ((t (:inherit 'org-code))))
   `(org-verbatim ((t (:foreground "#444444"))))

   ;; WORKAROUND https://github.com/jrblevin/markdown-mode/issues/273
   `(markdown-code-face ((t (:inherit 'org-code :background "#EEEEEE"))))
   `(markdown-pre-face ((t (:inherit 'org-verbatim))))
   ;; http://www.gnu.org/software/emacs/manual/html_node/ediff/Highlighting-Difference-Regions.html
   `(ediff-current-diff-A ((t (:background "#CBBBBB"))))
   `(ediff-current-diff-B ((t (:background "#BBCBBB"))))
   `(ediff-current-diff-C ((t (:background "#BBBBCB"))))
   `(ediff-fine-diff-A ((t (:weight ultra-bold :background "#DBBBBB"))))
   `(ediff-fine-diff-B ((t (:weight ultra-bold :background "#BBDBBB"))))
   `(ediff-fine-diff-C ((t (:weight ultra-bold :background "#BBBBDB"))))
   `(ediff-odd-diff-A ((t nil)))
   `(ediff-odd-diff-B ((t (:inherit 'ediff-odd-diff-A))))
   `(ediff-odd-diff-C ((t (:inherit 'ediff-odd-diff-A))))
   `(ediff-even-diff-A ((t nil)))
   `(ediff-even-diff-B ((t (:inherit 'ediff-even-diff-A))))
   `(ediff-even-diff-C ((t (:inherit 'ediff-even-diff-A))))
   `(smerge-mine ((t (:inherit 'ediff-current-diff-A))))
   `(smerge-other ((t (:inherit 'ediff-current-diff-B))))
   `(smerge-refined-removed ((t (:inherit 'ediff-fine-diff-A))))
   `(smerge-refined-added ((t (:inherit 'ediff-fine-diff-B))))
   `(smerge-markers ((t (:inherit 'font-lock-comment-face))))
   `(git-gutter:modified ((t (:foreground "#9876aa"))))
   `(git-gutter:added ((t (:foreground "#629755"))))
   `(git-gutter:deleted ((t (:foreground "#cc7832"))))
   `(ido-subdir ((t (:inherit 'font-lock-string-face))))
   `(isearch ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#bd3612" :background "#042028"))))
   `(isearch-fail ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#bd3612" :background "#042028"))))
   `(lazy-highlight ((t (:weight normal :slant normal :underline nil :inverse-video t :foreground "#a57705" :background "#042028"))))
   `(compilation-info ((t (:weight bold :foreground "#a6c25c" :underline nil))))
   `(match ((((class color) (min-colors 88) (background light)) (:background "yellow1"))
            (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3"))
            (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow"))
            (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue"))
            (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))

   ;; outlines
   `(outline-1 ((t :weight extra-bold :height 1.2)))
   `(outline-2 ((t :weight bold :height 1.15)))
   `(outline-3 ((t :weight bold :height 1.10)))
   `(outline-4 ((t :weight semi-bold :height 1.09)))
   `(outline-5 ((t :weight semi-bold :height 1.06)))
   `(outline-6 ((t :weight semi-bold :height 1.03)))
   `(outline-8 ((t :weight semi-bold)))
   `(outline-9 ((t :weight semi-bold)))
   ;; org-mode
   `(org-meta-line ((t (:inherit font-lock-comment-face :extend t :background "white smoke" :slant normal))))
   `(org-document-title ((t ( :inherit outline-1))))
   `(org-document-info ((t (:foreground "dark orange"))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-level-1 ((t :inherit outline-1)))
   `(org-level-2 ((t :inherit outline-2)))
   `(org-level-3 ((t :inherit outline-3)))
   `(org-level-4 ((t :inherit outline-4)))
   `(org-level-5 ((t :inherit outline-5)))
   `(org-level-6 ((t :inherit outline-6)))
   `(org-level-7 ((t :inherit outline-7)))
   `(org-level-8 ((t :inherit outline-8)))
   `(org-level-9 ((t :inherit outline-9)))
   `(org-code     ((t :inherit fixed-pitch)))
   `(org-verbatim ((t :inherit fixed-pitch :foreground "dark gray")))
   ;; `(org-block ((t (:inherit fixed-pitch))))
   ;; `(org-code ((t (:inherit (shadow fixed-pitch)))))
   ;; `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   ;; `(org-link ((t (:foreground "royal blue" :underline t))))
   ;; `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   ;; `(org-property-value ((t (:inherit fixed-pitch))) t)
   ;; `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   ;; `(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   ;; `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   ;; `(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
   ;; ivy-current-match : Highlights the currently selected candidate.
   ;; ivy-minibuffer-match-face-1 : Highlights the background of the match.
   ;; ivy-minibuffer-match-face-2 : Highlights the first (modulo 3) matched group.
   ;; ivy-minibuffer-match-face-3 : Highlights the second (modulo 3) matched group.
   ;; ivy-minibuffer-match-face-4 : Highlights the third (modulo 3) matched group.
   ;; ivy-confirm-face : Highlights the "(confirm)" part of the prompt.
   `(swiper-minibuffer-match-face-1 ((t :background "#dddddd")))
   `(swiper-minibuffer-match-face-2 ((t :background "#bbbbbb" :weight bold)))
   `(swiper-minibuffer-match-face-3 ((t :background "#bbbbff" :weight bold)))
   `(swiper-minibuffer-match-face-4 ((t :background "#ffbbff" :weight bold)))
   ;; eldoc
   `(eldoc-box-border ((t  :background "yellow" :foreground "black")))
   `(eldoc-box-body   ((t  :family "Ubuntu" :background nil :foreground "black")))
   ;; helpful
   `(helpful-heading ((t :inherit outline-1)))

   ;; origami
   `(origami-fold-fringe-face ((t :background ,warningBg)))
   ;; the ... part
   `(origami-fold-replacement-face ((t :background ,warningBg)))
   ;; bm : bookmrks
   `(bm-face ((t                    :background "#c0c0c0")))
   `(bm-fringe-face ((t             :background "#c0c0c0")))
   `(bm-persistent-face ((t         :background "#c0c0c0")))
   `(bm-fringe-persistent-face ((t  :background "#c0c0c0")))
   `(bm-face ((t                    :background "#c0c0c0")))
   `(bm-fringe-face ((t             :background "#c0c0c0")))
   `(bm-persistent-face ((t         :background "#c0c0c0")))
   `(bm-fringe-persistent-face ((t  :background "#c0c0c0")))
   ))
;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'intellij)

;;; intellij-theme.el ends here
