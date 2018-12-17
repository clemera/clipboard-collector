;;; clipboard-collector.el --- Collect clipboard entries according to regex rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  clemera

;; Author: clemera <clemera@clemera>
;; Keywords: convenience

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

;; Provides a macro `clipboard-collector-create' which creates a command to
;; collects clipboard items according to specific rules and afterwards act on
;; the collected items.
;;

;;; Code:



(defvar clipboard-collector--last-clip nil
  "Save last clipboard entry.")


(defun clipboard-collector--apply-rule (clip &optional rules)
  "Apply first rule of `clipboard-collector--rules' for CLIP.

Returns cons of matching regex of used rule and clipboard
contents transformed according to matched rule."
  (cl-dolist (rules (or rules clipboard-collector--rules))
    (when (string-match (car rules) clip)
      (let ((main (cond ((functionp (car (cddr rules)))
                         (funcall (car (cddr rules)) clip))
                        (t clip)))
            (format (cond ((functionp (cadr rules))
                           (funcall (cadr rules) (match-string 1 clip)))
                          ((stringp (cadr rules))
                           (cadr rules))
                          (t "%s"))))
        (cl-return (cons (car rules)
                         (format format main)))))))

(defun clipboard-collector--try-collect ()
  "If Clibboard changed and matches rule collect it."
  (let ((clip (gui-get-selection 'CLIPBOARD))
        (item nil))
    (when (and (not (string-empty-p clip))
               (not (string= clip
                             clipboard-collector--last-clip))
               (setq item (clipboard-collector--apply-rule clip)))
      (setq clipboard-collector--last-clip clip)
      (clipboard-collector--collect item))))


(defvar clipboard-collector--items nil
  "Saves collected items.")

(defun clipboard-collector--collect (item)
  "Collect ITEM.

ITEM is added to `clipboard-collector--items'."
  ;; replace if new match for same rule
  (cl-delete item clipboard-collector--items
             :test (lambda (i1 i2)
                     (string= (car i1) (car i1))))
  (push item clipboard-collector--items)
  (funcall clipboard-collector-display-function (cdr item)))


;; configure those for collecting
(defvar clipboard-collector--rules nil
  "Clipboard collection rules.

Uses the following list format:

    (match-regex [transform-format-string] [transform-clipboard-func])

MATCH-REGEX is the triggering regex, if clipboard contents match
this regex the clipboard entry will be collected.

Optional TRANSFORM-FORMAT-STRING should be a format string where
the placeholder is replaced by the clipboard contents.

If you want to transform the clipboard contents using a function
specify TRANSFORM-CLIPBOARD-FUNC. This is applied before contents
are applied to TRANSFORM-FORMAT-STRING.")

(defvar clipboard-collector--finish-function nil
  "Default function used by `clipboard-collector-finish'.")

(defvar clipboard-collector--timer nil)

(define-minor-mode clipboard-collector-mode
  "This mode is for internal use only.

Used by `clipboard-collector-start-watch' to setup an exit
binding.")

(defun clipboard-collector-start-watch ()
  "Run a timer to watch for clipboard changes.

If one of regexes of `clipboard-collector--rules' matches call
`clipboard-collector--collect' with first matching group and
clipboard contents."
  (interactive)
  (when clipboard-collector--timer
    (clipboard-collector-stop-watch))
  (setq clipboard-collector--last-clip "")
  (funcall interprogram-cut-function "")
  (setq clipboard-collector--items nil)
  (setq clipboard-collector--timer
        (run-at-time 0 0.2 #'clipboard-collector--try-collect))
  (clipboard-collector-mode 1)
  (message "Start collecting, finish with %s."
           (substitute-command-keys "\\[clipboard-collector-finish]")))

(defvar clipboard-collector-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "\r" 'clipboard-collector-finish))))


(defun clipboard-collector-stop-watch ()
  "Run a timer to watch for clipboard changes.

If one of regexes of `clipboard-collector--rules' matches call
`clipboard-collector--collect' with first matching group and
clipboard contents."
  (interactive)
  (when clipboard-collector--timer
    (cancel-timer clipboard-collector--timer)
    (setq clipboard-collector--timer nil)))

(defun clipboard-collector-finish ()
  "Run a timer to watch for clipboard changes.

If one of regexes of `clipboard-collector--rules' matches call
`clipboard-collector--collect' with first matching group and
clipboard contents."
  (interactive)
  (clipboard-collector-stop-watch)
  (unwind-protect
      (funcall clipboard-collector--finish-function
               (mapcar #'cdr clipboard-collector--items))
    (clipboard-collector-mode -1)))

(defvar clipboard-collector-display-function
  #'clipboard-collector-display
  "Function to display collected item.

Called with collected item.")

(defun clipboard-collector-display (item)
  "Display message for ITEM."
  (message "%s" item))

(defun clipboard-collector-finish-default (items)
  "Insert ITEMS separated by newlines."
  (save-excursion
    (while items
      (insert (pop items)
              (if items "\n" "")))))


(defmacro clipboard-collector-create (name rules &optional finishf)
  "Create clipboard collector command named NAME.

Calling the command will start a timer which checks the clipboard
for changes. If the content of a clipboard change match a rule of
RULES with format of `clipboard-collector--rules', it is
collected according to the rule.

The command will enable `clipboard-collector-mode' which will
bind `clipboard-collector-finish' to finish collecting items
using FINISHF which defaults to
`clipboard-collector-finish-default'."
  `(defun ,name ()
     ,(format "Start timer to collect clipboard items according
to the following rules (see `clipboard-collector--rules'):

%s

This command enables `clipboard-collector-mode' which binds
`clipboard-collector-finish' to apply function

`%s'

on the collected items. "
              (pp rules) (pp finishf))
     (interactive)
     (setq clipboard-collector--finish-function
           (or ',finishf #'clipboard-collector-finish-default))
     (setq clipboard-collector--rules ',rules)
     (clipboard-collector-start-watch)))


(provide 'clipboard-collector)
;;; clipboard-collector.el ends here
