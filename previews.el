;;; previews.el --- Make a single HTML page for all new comics
;; Copyright (C) 2015 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; previews.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; previews.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar previews-mail-address nil
  "The address to notify when we got a new month's data.")

(defun previews-check ()
  "Check whether this month's previews exists, and if so, download it."
  (interactive)
  (let ((month (format-time-string "%B"))
	(year (format-time-string "%Y")))
    (unless (file-exists-p (previews-file year month))
      (when (previews-fetch-month year month)
	(when previews-mail-address
	  (message-mail previews-mail-address
			(format "Previews for %s %s has been downloaded"
				month year))
	  (insert "Indeed.")
	  (message-send-and-exit))))))

(defun previews-insert (&rest format)
  (let ((url (apply 'format format))
	result)
    (message "%s" url)
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (when (re-search-forward "\r?\n\r?\n" nil t)
	(setq result (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer)))
    (when result
      (insert result)
      (goto-char (point-min)))
    (sleep-for 1)))

(defun previews-fetch-month (year month)
  (let ((base
	 (format "http://www.milehighcomics.com/comicindex/nice/%s-%s/"
		 month year))
	publishers titles html)
    (with-temp-buffer
      (insert "<body>\n")
      (with-temp-buffer
	(previews-insert "%sPublisherIndex.html" base)
	(while (re-search-forward "href=\"\\(Publisher-[^\"]+\\)\">\\([^<]+\\)"
				  nil t)
	  (push (list (match-string 1) (match-string 2)) publishers)))
      (dolist (elem (nreverse publishers))
	(setq titles nil)
	(insert (format "<h1>%s</h1><p>" (cadr elem)))
	(message "%s" (cadr elem))
	(with-temp-buffer
	  (previews-insert "%s%s" base (car elem))
	  (while (re-search-forward "href=\"\\(Title-[^\"]+\\)\">\\([^<]+\\)"
				    nil t)
	    (push (list (match-string 1) (match-string 2) (cadr elem))
		  titles)))
	(dolist (elem (nreverse titles))
	  (destructuring-bind (url title publisher) elem
	    (with-temp-buffer
	      (previews-insert "%s%s" base url)
	      (cond
	       ((re-search-forward "^<b>" nil t)
		(push (buffer-substring (point)
					(progn (re-search-forward "^</td>")
					       (match-beginning 0)))
		      html))
	       ((re-search-forward "\\(/cgi-bin/nice.cgi\\?action=list&title=[^\"]+\\)\">\\([^<]+\\)"
				   nil t)
		(setq url (match-string 1))
		(with-temp-buffer
		  (previews-insert "http://www.milehighcomics.com%s" url)
		  (when (re-search-forward "^<b>" nil t)
		    (push (buffer-substring
			   (point)
			   (progn (re-search-forward ".*You Pay Only Or Less")
				  (match-beginning 0)))
			  html))))))
	    (when html
	      (insert (car html))
	      (insert "<p>")))))
      (if publishers
	  (let ((coding-system-for-write 'utf-8))
	    (write-region (point-min) (point-max) (previews-file year month))
	    t)
	nil))))

(defun previews-file (year month)
  (format "~/tmp/nice-%s-%s.html" year month))

(provide 'previews)

;;; csid.el ends here
