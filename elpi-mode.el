;;; elpi-mode.el --- Major mode for editing elpi files           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Vojtěch Štěpančík

;; Author: Vojtěch Štěpančík
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
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

;;; Code:
(eval-when-compile
  (require 'rx))

(defvar font-lock-beg)
(defvar font-lock-end)

(defconst elpi--declaration-regex
  (rx-to-string
   '(: line-start
       (* white)
       (group (? "external"))
       (* white)
       (group (| "pred" "func"))
       (+ white)
       (group (: symbol-start (+? anychar) symbol-end))))
  "Regex identifying the head of a `pred' or `fun' declaration.")

;; TODO: better highlighting of types
;; TODO: type declarations
;; TODO: typeabbrev
;; TODO: macro
;; TODO: shorten
;; TODO: namespace
(defconst elpi--font-lock-defaults
  (let ((directives
         '("type" "constraint" "rule" "accumulate"
           "typeabbrev" "macro" "shorten" "namespace"))
        (builtins
         '("pi" "sigma" "is" "div" "mod"))
        (sigils
         '("=>" "==>" ":-" "?-" "[" "]" "::" "<=>" "=" "==" "|")))
    `((,(rx-to-string
         `(: line-start (* white) (| ,@directives)))
       . 'font-lock-keyword-face)
      (,(rx-to-string
         '(: symbol-start (? "do") "!" symbol-end))
       . 'font-lock-warning-face)
      (,(rx-to-string `(| ,@sigils)) . 'font-lock-builtin-face)
      (,(rx-to-string
         `(: symbol-start
             (| ,@builtins)
             symbol-end))
       . 'font-lock-builtin-face)
      (,(rx-to-string
         '(: line-start (* white) ":" (+ alpha)))
       . 'font-lock-preprocessor-face)
      (,elpi--declaration-regex
       (1 'font-lock-keyword-face)
       (2 'font-lock-keyword-face)
       (3 'font-lock-function-name-face)
       (,(rx-to-string
          '(:
            (group (| "i" "o"))
            (* white)
            ":"
            (group (+ (not (| "," "."))))))
        (save-excursion
          (goto-char (match-end 0))
          (search-forward "." nil t)
          (point))
        (goto-char (match-end 0))
        (1 'font-lock-keyword-face)
        (2 'font-lock-type-face)))
      (,(rx-to-string
         '(: symbol-start
             (| "pi" "sigma")
             symbol-end))
       (0 'font-lock-builtin-face)
       (,(rx-to-string
          '(:
            (group (+? (not "\\")))
            (group "\\")))
        (save-excursion
          (goto-char (match-end 0))
          (search-forward "\\" (line-end-position) t)
          (point))
        (goto-char (match-end 0))
        (1 'font-lock-variable-name-face)
        (2 'font-lock-builtin-face)))
      (,(rx-to-string
         '(: symbol-start
             (| upper "_")
             (*? anychar)
             symbol-end))
       (0 'font-lock-variable-name-face))
      (,(rx-to-string
         '(: line-start
             symbol-start
             lower
             (*? anychar)
             symbol-end))
       (0 'font-lock-function-name-face))
      (,(rx-to-string
         '(: symbol-start
             "@"
             (+? anychar)
             symbol-end))
       (0 'font-lock-function-name-face))
      (,(rx-to-string
         '(: symbol-start
             (+? (: (+? anychar) "."))
             (group (+? (not ".")))
             symbol-end))
       (1 'font-lock-function-name-face))
      (,(rx-to-string
         '(: symbol-start
             (+ digit)
             symbol-end))
       (0 'font-lock-number-face)))))

(defconst elpi--syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?/ ". 14n" st)
    (modify-syntax-entry ?* ". 23n" st)
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?< "_" st)
    (modify-syntax-entry ?> "_" st)
    (modify-syntax-entry ?! "_" st)
    (modify-syntax-entry '(?0 . ?9) "_" st)
    (modify-syntax-entry ?. "_" st)
    ;; only valid at the start of macros, not sure how I want to reflect that
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry ?| "." st)
    st))

(defun elpi--font-lock-extend-region-function ()
  "Extend the font-locking region to properly cover all pred definitions."
  (let ((changed nil))
    (save-excursion
      (goto-char font-lock-end)
      (condition-case nil
          (progn
            (re-search-backward elpi--declaration-regex font-lock-beg nil 1)
            (goto-char (match-end 0))
            (let ((done nil))
              (while (not done)
                (let* ((pos (search-forward "." nil nil 1))
                       (next-stx (syntax-after pos)))
                  (unless (and next-stx (eq ?w (syntax-class-to-char (car next-stx))))
                    (setq done t)))))
            (let ((decl-end (match-end 0)))
              (when (< font-lock-end decl-end)
                (setq
                 font-lock-end decl-end
                 changed t))))
        (search-failed))
      )
    changed))

;;;###autoload
(define-derived-mode elpi-mode prog-mode "elpi"
  "Major mode for editing elpi source code."
  (setq font-lock-defaults '(elpi--font-lock-defaults))
  (add-to-list
   'font-lock-extend-region-functions
   #'elpi--font-lock-extend-region-function t)
  (set-syntax-table elpi--syntax-table)
  (setq comment-start "%")
  (setq comment-start-skip "%+[\t ]*"))
;;;###

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elpi\\'" . elpi-mode))

(provide 'elpi-mode)
;;; elpi-mode.el ends here

