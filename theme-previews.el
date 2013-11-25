;;; theme-previews.el --- Make previews of installed themes

;; Copyright (C) 2013 Rikard Glans

;; Author: Rikard Glans <rikard@ecx.se>
;; URL: https://github.com/darrik/theme-previews
;; Version: 0.0.2
;; Keywords: colorthemes, themes
;; Created: 22nd Nov 2013

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Open a file you want to use for your previews (i.e a code snippet) and then
;; run M-x theme-previews

;;; Code:
(when (< emacs-major-version 24)
  (error "Theme Previews only works with Emacs 24 or greater."))

;; Customization

(defgroup theme-previews nil
  "Create previews of themes."
  :group 'applications
  :prefix "tp-")

;; Configuration variables

(defcustom tp-outdir "."
  "Output directory."
  :type 'string
  :tag "Output directory"
  :group 'theme-previews)

(defcustom tp-badthemes
  '(
    tronesque   ;; Utterly messes up your mode line.
    tronesqued
    sw          ;; Base for a Star Wars™ based theme
    color       ;; Emacs <24 color-theme package
    vmalloc     ;; Makes emacs crap out on a missing "quote" face
    assemblage  ;; Breaks with "max-lisp-eval-depth" error
    dark-laptop ;; -^^-
    mesa        ;; -^^-
    )
  "Themes known to mess things up."
  :type 'sexp
  :tag "Bad themes"
  :group 'theme-previews)

;; Functions

;; TODO: Make more generic
(defun tp--has-suffix? (theme)
  "Check if theme is using suffix versions."
  (let* ((thm    (replace-regexp-in-string "-theme" "" (format "%s" theme)))
         (dark   (intern (format "%s-dark" thm)))
         (light  (intern (format "%s-light" thm)))
         (themes (custom-available-themes)))
    (if (or (memq dark themes) (memq light themes))
        t
      nil)))

(defun tp--isbad? (theme)
  "Look up THEME in tp-badthemes."
  (member theme tp-badthemes))

;; TODO: Error checking. Did we actually get a html buffer? did we actually switch? did we actually write a file?
(defun tp--make-preview (theme)
  "Create preview file for THEME using current buffer."
  (let* ((bufname (buffer-name))
         (outname (format "%s/%s-%s.html" tp-outdir theme bufname))
         (cbuf    (current-buffer)))
    (let ((hbuf (htmlfontify-buffer)))
      (switch-to-buffer hbuf)
      ;;; TODO: This might not be desireable on non-code texts.
      ;; copy "body" into "pre"
      (goto-char (point-min))
      (kill-ring-save
       (goto-char (- (search-forward-regexp "^body {") 1))
       (search-forward-regexp "}$"))
      (insert "\npre ")
      (yank)
      ;; write and kill
      (write-file outname)
      (kill-buffer)
      (switch-to-buffer cbuf))))

(defun tp--make-previews (themes)
  "Call make-preview on each available theme."
  (let ((thm (car themes)))
    (if (or (tp--isbad? thm) (tp--has-suffix? thm))
        (tp--make-previews (remq thm themes)) ; TODO: nil check this
      ;; else
      (load-theme thm t) ; t UNSAFE

      (let* ((current (tp--current-theme))
             (rest    (remq current themes)))
        (tp--make-preview current)
        (tp--reset-theme)
        (unless (eq rest nil)
          (tp--make-previews rest))))))

(defun tp--reset-theme ()
  "Disable all loaded themes, effectively resetting to default colors."
  (mapc 'disable-theme custom-enabled-themes))

(defun tp--current-theme ()
  "Return currently loaded theme."
  (car custom-enabled-themes))

(defun tp--run ()
  "Do it!"
  (let ((ctheme (tp--current-theme)))
    (tp--make-previews (custom-available-themes))

    (if (eq ctheme nil)
        (tp--reset-theme)
      (load-theme ctheme t)))) ; We can probably assume that the currently
                               ; loaded theme is safe.

(defun theme-previews ()
  "Make previews of all installed themes using current buffer."
  (interactive)
  (message "[TP]: Making previews in five seconds (cancel with CTRL-g.)\nThis can take a very long time depending on how many themes you have installed!")
  (sit-for 5) ; TODO: Replace with "Run: y/n" ?
  (tp--run)
  (message "[TP]: Done!"))

(provide 'theme-previews)

;;; theme-previews.el ends here
