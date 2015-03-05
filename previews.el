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

(defun previews-fetch-month (month year)
  (let ((base
	 (format "http://www.milehighcomics.com/comicindex/nice/%s-%s/"
		 month year))
	do publishers titles html)
    (with-temp-buffer
      (insert "<body>\n")
      (with-temp-buffer
	(url-insert-file-contents 
	 (format "%sPublisherIndex.html" base))
	(goto-char (point-min))
	(while (re-search-forward "href=\"\\(Publisher-[^\"]+\\)\">\\([^<]+\\)"
				  nil t)
	  (push (list (match-string 1) (match-string 2)) publishers)))
      (setq publishers (nreverse publishers))
      (dolist (elem publishers)
	(setq titles nil)
	(insert (format "<h1>%s</h1><p>" (cadr elem)))
	(message "%s" (cadr elem))
	(with-temp-buffer
	  (ignore-errors
	    (url-insert-file-contents (format "%s%s" base (car elem))))
	  (sleep-for 1)
	  (goto-char (point-min))
	  (while (re-search-forward "href=\"\\(Title-[^\"]+\\)\">\\([^<]+\\)"
				    nil t)
	    (push (list (match-string 1) (match-string 2) (cadr elem))
		  titles)))
	(setq titles (nreverse titles))
	(dolist (elem titles)
	  (destructuring-bind (url title publisher) elem
	    (with-temp-buffer
	      (ignore-errors
		(mm-url-insert-file-contents (format "%s%s" base url)))
	      (sleep-for 1)
	      (goto-char (point-min))
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
		  (ignore-errors
		    (mm-url-insert-file-contents
		     (format "http://www.milehighcomics.com%s" url)))
		  (sleep-for 1)
		  (goto-char (point-min))
		  (when (re-search-forward "^<b>" nil t)
		    (push (buffer-substring
			   (point)
			   (progn (re-search-forward ".*You Pay Only Or Less")
				  (match-beginning 0)))
			  html)))))
	      (when html
		(insert (car html))
		(insert "<p>"))))))
      (let ((coding-system-for-write 'utf-8))
	(write-region (point-min) (point-max)
		      (format "~/tmp/nice-%s-%s.html"
			      year month))))))

(provide 'previews)

;;; csid.el ends here
