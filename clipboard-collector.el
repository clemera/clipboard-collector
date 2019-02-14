;;; clipboard-collector.el --- Collect clipboard entries according to regex rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Clemens Radermacher <clemera@posteo.net>
;; URL: https://github.com/clemera/clipboard-collector
;; Version: 0.3
;; Package-Requires: ((emacs "25"))
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

;; When collecting information using copy/paste, it would be useful if one could
;; stay at one place to copy things and later insert them all at once at another
;; place. Emacs has append-next-kill but it only works inside Emacs and it only
;; applies to the very next command. Further it would be great if Emacs could
;; detect specific clipboard entries and transform them to a different format
;; automatically. clipboard-collector provides you with those features (tested
;; only for Linux).

;; Use `clipboard-collector-mode' or `clipboard-collector-create' to create a
;; command which collects clipboard items according to specific rules.
;;

;;; Code:

(eval-when-compile (require 'subr-x))   ;string-empty-p

(defvar clipboard-collector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'clipboard-collector-finish)
    map)
  "Keymap active during clipboard collection commands.")


(defvar clipboard-collector--last-clip ""
  ;; FIXME: the docstring should say what it holds, rather than what the code
  ;; using it does (IOW it's the code which "save"s, not the variable).
  ;; Same comment applies to clipboard-collector--items.
  "Save last clipboard entry.")


(defvar clipboard-collector--items nil
  "Saves collected items.")


;; configure those for collecting
(defvar clipboard-collector--rules '((".*" "%s"))
  "Clipboard collection rules.

Uses the following list format:

    (MATCH-REGEX [TRANSFORM-FORMAT-STRING] [TRANSFORM-CLIPBOARD])

MATCH-REGEX is the triggering regex, if clipboard contents match
this regex the clipboard entry will be collected.

Optional TRANSFORM-FORMAT-STRING should be a format string where
the '%s' placeholder is replaced by the clipboard contents.

Additionally the matched candidated can be transformed by
specifying TRANSFORM-CLIPBOARD. If it's a function it gets called
with the matched candidated and its return value will be applied
to TRANSFORM-FORMAT-STRING. The function can use match-data of
MATCH-REGEX. In case TRANSFORM-CLIPBOARD is a number the match
string of that number will be applied to
TRANSFORM-FORMAT-STRING.")

(defvar clipboard-collector--finish-function
  #'clipboard-collector-finish-default
  "Default function used by `clipboard-collector-finish'.")

(defvar clipboard-collector--timer nil)

(defvar clipboard-collector--transient-exit nil)

(defvar clipboard-collector-mode nil)

;;;###autoload
(define-minor-mode clipboard-collector-mode
  "Start collecting clipboard items.

Rules used are defined in `clipboard-collector--rules'. Because
this mode is only for temporary use and you want its bindings to
have precedence over all other ones when activated,
`clipboard-collector-mode-map' is made transient while this mode
is active."
  :lighter " cc"
  :global t
  :variable clipboard-collector-mode
  (if clipboard-collector-mode
      (progn
        ;; Set defaults.
        (setq clipboard-collector--finish-function
              #'clipboard-collector-finish-default)
        (setq clipboard-collector--rules '((".*" "%s")))

        ;; Init clip data.
        (setq clipboard-collector--items nil)
        (setq clipboard-collector--last-clip
              (or (ignore-errors (gui-get-selection 'CLIPBOARD))
                  ""))

        ;; Intercept kills inside Emacs.
        (add-function :after interprogram-cut-function
                      #'clipboard-collector--try-collect)

        ;; Outside Emacs use gpastel if available or poll the clipboard.
        (if (bound-and-true-p gpastel-mode)
            (add-hook 'gpastel-update-hook
                      #'clipboard-collector--try-collect-last-kill)
          (setq clipboard-collector--timer
                (run-at-time 0 0.2 #'clipboard-collector--try-collect)))

        ;; Make keymap highest priority.
        ;; FIXME: Say why it isn't sufficient to have the map in
        ;; minor-mode-map-alist?
        (setq clipboard-collector--transient-exit
              (set-transient-map clipboard-collector-mode-map t))
        (message "Start collecting, finish with %s."
                 (substitute-command-keys "\\[clipboard-collector-finish]")))

    (remove-function interprogram-cut-function #'clipboard-collector--try-collect)
    (when clipboard-collector--transient-exit
      (funcall clipboard-collector--transient-exit))
    (remove-hook 'gpastel-update-hook
                 #'clipboard-collector--try-collect-last-kill)
    (when clipboard-collector--timer
      (cancel-timer clipboard-collector--timer)
      (setq clipboard-collector--timer nil))))


(defun clipboard-collector--apply-rule (clip &optional rules)
  "Apply first rule of `clipboard-collector--rules' for CLIP.

Returns cons of matching regex of used rule and clipboard
contents transformed according to matched rule."
  (cl-dolist (rule (or rules clipboard-collector--rules))
    (when (string-match (car rule) clip)
      (let* ((converter (car (cddr rule)))
             (transformed (cond ((functionp converter)
                                 (funcall converter clip))
                                ((numberp converter)
                                 (match-string converter clip))
                                (t clip)))
             (format (cond ((functionp (cadr rule))
                            (funcall (cadr rule) (match-string 1 clip)))
                           ((stringp (cadr rule))
                            (cadr rule))
                           (t "%s"))))
        (cl-return (cons (car rule)
                         (format format transformed)))))))


(defun clipboard-collector--try-collect-last-kill ()
  (clipboard-collector--try-collect (or (car kill-ring) "")))

(defun clipboard-collector--try-collect (&optional clip)
  "If Clibboard changed and matches rule collect it.

If CLIP is not given `gui-get-selection' is used to check for the
clipboard entry."
    (condition-case nil
      (let ((clip (or clip (gui-get-selection 'CLIPBOARD)))
            (item nil))
        (when (and (not (string-empty-p clip))
                   (not (string= clip
                                 clipboard-collector--last-clip))
                   (setq item (clipboard-collector--apply-rule clip)))
          (setq clipboard-collector--last-clip clip)
          (clipboard-collector--collect item)))
    (error (progn (message "Error during clipboard collection, exited `clipboard-collector-mode'")
                  (clipboard-collector-mode -1)))))

(defvar clipboard-collector-display-function
  #'clipboard-collector-display
  "Function to display collected item.

Called with collected item.")

(defun clipboard-collector--collect (item)
  "Collect ITEM.

ITEM is added to `clipboard-collector--items'."
  (push item clipboard-collector--items)
  (funcall clipboard-collector-display-function (cdr item)))

(defun clipboard-collector-finish ()
  "Finish collecting clipboard items.

Uses `clipboard-collector--finish-function' ."
  (interactive)
  (clipboard-collector-mode -1)
  (funcall clipboard-collector--finish-function
           (nreverse (mapcar #'cdr clipboard-collector--items))))

(defun clipboard-collector-display (item)
  "Display message for ITEM."
  (message "Collected: '%s'" item))

(defun clipboard-collector-finish-default (items)
  "Insert ITEMS separated by newlines."
  (save-excursion
    (while items
      (insert (pop items)
              (if items "\n" "")))))

;;;###autoload
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
     (clipboard-collector-mode 1)
     (setq clipboard-collector--finish-function
           (or ',finishf #'clipboard-collector-finish-default))
     (setq clipboard-collector--rules ',rules)))


(provide 'clipboard-collector)
;;; clipboard-collector.el ends here
