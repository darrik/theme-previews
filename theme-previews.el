;;; theme-previews.el --- Make previews of installed themes

;; Copyright (C) 2013 Rikard Glans

;; Author: Rikard Glans <rikard@ecx.se>
;; URL: https://github.com/darrik/theme-previews
;; Version: 0.0.1
;; Keywords: colorthemes, themes
;; Created: 22nd Nov 2013
;; Package-Requires: ((htmlize "1.47"))

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

;; TODO: Error checking. Did we actually get a htmlized buffer? did we actually switch? did we actually write a file?
(defun tp--make-preview (theme)
  "Create preview file for THEME using current buffer."
  (let* ((bufname (buffer-name))
         (outname (format "%s-%s.html" theme bufname))
         (cbuf    (current-buffer)))
    (let ((hbuf (htmlize-buffer)))
      (switch-to-buffer hbuf)
      (write-file outname)
      (kill-buffer)
      (switch-to-buffer cbuf))))

(defun tp--make-previews (themes)
  "Call make-preview on each available theme."
  (load-theme (car themes))

  (let* ((current (car custom-enabled-themes))
        (rest    (remq current themes)))
    (tp--make-preview current)
    (tp--reset-theme)
    (unless (eq rest nil)
      (tp--make-previews rest))))

(defun tp--reset-theme ()
  "Disable all loaded themes, effectively resetting to default colors."
  (mapc 'disable-theme custom-enabled-themes))

(defun tp--run ()
  "Do it!"
  (let ((ctheme (car custom-enabled-themes)))
    (tp--make-previews (custom-available-themes))

    (if (eq ctheme nil)
        (tp--reset-theme)
      (load-theme ctheme t)))) ; We can probably safely assume that the currently loaded theme is safe.

(defun theme-previews ()
  "Make previews of all installed themes using current buffer."
  (interactive)
  (message "Making previews in five seconds (cancel with CTRL-g.)")
  (sit-for 5)
  (tp--run)
  (message "Done!"))

(provide 'theme-previews)

;;; theme-previews.el ends here
