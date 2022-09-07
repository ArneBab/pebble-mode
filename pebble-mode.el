;;; pebble-mode.el --- A major mode for pebble

;; Copyright (C) 2011-2022 Florian Mounier aka paradoxxxzero
;; Copyright (C) 2022-- Arne Babenhauserheide for Disy Informationssysteme GmbH

;; Author: Florian Mounier aka paradoxxxzero
;; Author: Arne Babenhauserheide
;; Version: 0.3

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;   This is an Emacs major mode for pebble with:
;;        syntax highlighting
;;        sgml/html integration
;;        indentation (working with sgml)
;;        more to come

;; It follows pebbles syntax reference: https://pebbletemplates.io/wiki/guide/basic-usage/

;; pebble-mode is based on jinja2-mode by Florian Mounier

;;; Code:

(require 'sgml-mode)

(defgroup pebble nil
  "Major mode for editing pebble code."
  :prefix "pebble-"
  :group 'languages)

(defcustom pebble-user-keywords nil
  "Custom keyword names."
  :type '(repeat string)
  :group 'pebble)

(defcustom pebble-user-functions nil
  "Custom function names."
  :type '(repeat string)
  :group 'pebble)

;; (defcustom pebble-debug nil
;;   "Log indentation logic"
;;   :type 'boolean
;;   :group 'pebble)

(defun pebble-closing-keywords ()
  "Keywords that open a tag which gets closed with an end tag."
  (append
   pebble-user-keywords
   '("if" "for" "block" "filter" "with"
     "raw" "macro" "autoescape" "trans" "call")))

(defun pebble-indenting-keywords ()
  "Keywords that close and re-open indentation."
  (append
   (pebble-closing-keywords)
   '("else" "elseif")))

(defun pebble-builtin-keywords ()
  "Core keywords."
  '("as" "autoescape" "debug" "extends"
    "firstof" "in" "include" "load"
    "now" "regroup" "ssi" "templatetag"
    "url" "widthratio" "elseif" "true"
    "false" "none" "False" "True" "None"
    "loop" "super" "caller" "varargs"
    "kwargs" "break" "continue" "is"
    "not" "or" "and"
    "do" "pluralize" "set" "from" "import"
    "context" "with" "without" "ignore"
    "missing" "scoped"))

(defun pebble-filters-keywords ()
  "Keywords that can be used as filters."
  (append
   pebble-user-functions
   '("abs" "attr" "batch" "capitalize"
     "center" "default" "dictsort"
     "escape" "filesizeformat" "first"
     "float" "forceescape" "format"
     "groupby" "indent" "int" "join"
     "last" "length" "list" "lower"
     "pprint" "random" "replace"
     "reverse" "round" "safe" "slice"
     "sort" "string" "striptags" "sum"
     "title" "trim" "truncate" "upper"
     "urlize" "wordcount" "wordwrap" "xmlattr")))

(defun pebble-find-open-tag ()
  "Find the innermost open tag."
  (if (search-backward-regexp
       (rx-to-string
        `(and "{%"
              (? "-")
              (* whitespace)
              (? (group
                  "end"))
              (group
               ,(append '(or)
                        (pebble-closing-keywords)
                        ))
              (group
               (*? anything))
              (* whitespace)
              (? "-")
              "%}")) nil t)
      (if (match-string 1) ;; End tag, going on
          (let ((matches (pebble-find-open-tag)))
            (if (string= (car matches) (match-string 2))
                (pebble-find-open-tag)
              (list (match-string 2) (match-string 3))))
        (list (match-string 2) (match-string 3)))
    nil))

(defun pebble-close-tag ()
  "Close the previously opened template tag."
  (interactive)
  (let ((open-tag (save-excursion (pebble-find-open-tag))))
    (if open-tag
        (insert
         (if (string= (car open-tag) "block")
             (format "{%% end%s%s %%}"
                     (car open-tag)(nth 1 open-tag))
           (format "{%% end%s %%}"
                   (match-string 2))))
      (error "Nothing to close")))
  (save-excursion (pebble-indent-line)))

(defun pebble-insert-tag ()
  "Insert an empty tag."
  (interactive)
  (insert "{% ")
  (save-excursion
    (insert " %}")
    (pebble-indent-line)))

(defun pebble-insert-var ()
  "Insert an empty tag."
  (interactive)
  (insert "{{ ")
  (save-excursion
    (insert " }}")
    (pebble-indent-line)))

(defun pebble-insert-comment ()
  "Insert an empty tag."
  (interactive)
  (insert "{# ")
  (save-excursion
    (insert " #}")
    (pebble-indent-line)))

(defconst pebble-font-lock-comments
  `(
    (,(rx "{#"
          (* whitespace)
          (group
           (*? anything)
           )
          (* whitespace)
          "#}")
     . (1 font-lock-comment-face t))))

(defconst pebble-font-lock-keywords-1
  (append
   pebble-font-lock-comments
   sgml-font-lock-keywords-1))

(defconst pebble-font-lock-keywords-2
  (append
   pebble-font-lock-keywords-1
   sgml-font-lock-keywords-2))

(defconst pebble-font-lock-keywords-3
  (append
   pebble-font-lock-keywords-1
   pebble-font-lock-keywords-2
   `(
     (,(rx "{{"
           (* whitespace)
           (group
            (*? anything)
            )
           (*
            "|" (* whitespace) (*? anything))
           (* whitespace)
           "}}") (1 font-lock-variable-name-face t))
     (,(rx  (group "|" (* whitespace))
            (group (+ word))
            )
      (1 font-lock-keyword-face t)
      (2 font-lock-warning-face t))
     (,(rx-to-string `(and (group "|" (* whitespace))
                           (group
                            ,(append '(or)
                                     (pebble-filters-keywords)
                                     ))))
      (1 font-lock-keyword-face t)
      (2 font-lock-function-name-face t)
      )
     (,(rx-to-string `(and word-start
                           (? "end")
                           ,(append '(or)
                                    (pebble-indenting-keywords)
                                    )
                           word-end)) (0 font-lock-keyword-face))
     (,(rx-to-string `(and word-start
                           ,(append '(or)
                                    (pebble-builtin-keywords)
                                    )
                           word-end)) (0 font-lock-builtin-face))

     (,(rx (or "{%" "%}" "{%-" "-%}")) (0 font-lock-function-name-face t))
     (,(rx (or "{{" "}}")) (0 font-lock-type-face t))
     (,(rx "{#"
           (* whitespace)
           (group
            (*? anything)
            )
           (* whitespace)
           "#}")
      (1 font-lock-comment-face t))
     (,(rx (or "{#" "#}")) (0 font-lock-comment-delimiter-face t))
     )))

(defvar pebble-font-lock-keywords
  pebble-font-lock-keywords-1)

(defvar pebble-enable-indent-on-save nil)

(defun sgml-indent-line-num ()
  "Indent the current line as SGML."
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (if (>= (point) savep) (setq savep nil))
            (sgml-calculate-indent))))
    (if (null indent-col)
        0
      (if savep
          (save-excursion indent-col)
        indent-col))))

(defun pebble-calculate-indent-backward (default)
  "Return indent column based on previous lines; DEFAULT is indentation from sgml."
  (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
    (forward-line -1)
    (if (looking-at "^[ \t]*{%-? *end") ; Don't indent after end
        (current-indentation)
      (if (looking-at (concat "^[ \t]*{%-? *.*?{%-? *end" (regexp-opt (pebble-indenting-keywords))))
          (current-indentation)
        (if (looking-at (concat "^[ \t]*{%-? *" (regexp-opt (pebble-indenting-keywords)))) ; Check start tag
            (+ (current-indentation) indent-width)
          (if (looking-at "^[ \t]*<") ; Assume sgml block trust sgml
              default
            (if (bobp)
                0
              (pebble-calculate-indent-backward default))))))))


(defun pebble-calculate-indent ()
  "Return indent column."
  (if (bobp)  ; Check beginning of buffer
      0
    (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
      (if (looking-at "^[ \t]*{%-? *e\\(nd\\|lse\\|lif\\)") ; Check close tag
          (save-excursion
            (forward-line -1)
            (if
                (and
                 (looking-at (concat "^[ \t]*{%-? *" (regexp-opt (pebble-indenting-keywords))))
                 (not (looking-at (concat "^[ \t]*{%-? *.*?{% *end" (regexp-opt (pebble-indenting-keywords))))))
                (current-indentation)
              (- (current-indentation) indent-width)))
        (if (looking-at "^[ \t]*</") ; Assume sgml end block trust sgml
            default
          (save-excursion
            (pebble-calculate-indent-backward default)))))))

(defun pebble-indent-line ()
  "Indent current line as pebble code."
  (interactive)
  (let ((old_indent (current-indentation)) (old_point (point)))
    (move-beginning-of-line nil)
    (let ((indent (max 0 (pebble-calculate-indent))))
      (indent-line-to indent)
      (if (< old_indent (- old_point (line-beginning-position)))
          (goto-char (+ (- indent old_indent) old_point)))
      indent)))

(defun pebble-indent-buffer()
  "Re-indent the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

;;;###autoload
(define-derived-mode pebble-mode html-mode  "Pebble"
  "Major mode for editing pebble files."
  :group 'pebble
  ;; Disabling this because of this emacs bug:
  ;;  http://lists.gnu.org/archive/html/bug-gnu-emacs/2002-09/msg00041.html
  ;; (modify-syntax-entry ?\'  "\"" sgml-mode-syntax-table)
  (set (make-local-variable 'comment-start) "{#")
  (set (make-local-variable 'comment-start-skip) "{#")
  (set (make-local-variable 'comment-end) "#}")
  (set (make-local-variable 'comment-end-skip) "#}")
  ;; it mainly from sgml-mode font lock setting
  (set (make-local-variable 'font-lock-defaults)
       '((
          pebble-font-lock-keywords
          pebble-font-lock-keywords-1
          pebble-font-lock-keywords-2
          pebble-font-lock-keywords-3)
         nil t nil nil
         (font-lock-syntactic-keywords
          . sgml-font-lock-syntactic-keywords)))
  (set (make-local-variable 'indent-line-function) 'pebble-indent-line))

(define-key pebble-mode-map (kbd "C-c c") 'pebble-close-tag)
(define-key pebble-mode-map (kbd "C-c t") 'pebble-insert-tag)
(define-key pebble-mode-map (kbd "C-c v") 'pebble-insert-var)
(define-key pebble-mode-map (kbd "C-c #") 'pebble-insert-comment)

(when pebble-enable-indent-on-save
  (add-hook 'pebble-mode-hook
    (lambda ()
      (add-hook 'after-save-hook 'pebble-indent-buffer nil 'make-it-local))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pebble\\'" . pebble-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.j2\\'" . pebble-mode))

(provide 'pebble-mode)

;;; pebble-mode.el ends here
