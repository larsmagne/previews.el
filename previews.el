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

(defun nice-mm-url-insert-file-contents (url)
  (if (not (ignore-errors (mm-url-insert-file-contents url)))
      (progn
	(sleep-for 2)
	(ignore-errors (mm-url-insert-file-contents url)))))

(defun fetch-all-data ()
  (let ((base "http://www.milehighcomics.com/comicindex/nice/February-2015/")
	(output (find-file "~/nice3.html"))
	do
	publishers titles html)
    (save-excursion
      (set-buffer output)
      (erase-buffer)
      (insert "<body>\n"))
    (with-temp-buffer
      (mm-url-insert-file-contents 
       (format "%sPublisherIndex.html" base))
      (goto-char (point-min))
      (while (re-search-forward "href=\"\\(Publisher-[^\"]+\\)\">\\([^<]+\\)"
				nil t)
	(push (list (match-string 1) (match-string 2)) publishers)))
    (setq publishers (nreverse publishers))
    ;;(while (not (string-match "Image" (cadar publishers))) (pop publishers))
    (dolist (elem publishers)
      (setq titles nil)
      (save-excursion
	(set-buffer output)
	(goto-char (point-max))
	(insert (format "<h1>%s</h1><p>" (cadr elem))))
      (message "%s" (cadr elem))
      (with-temp-buffer
	(unless (ignore-errors
		  (mm-url-insert-file-contents
		   (format "%s%s" base (car elem)))
		  t)
	  (sleep-for 1))
	(goto-char (point-min))
	(while (re-search-forward "href=\"\\(Title-[^\"]+\\)\">\\([^<]+\\)"
				  nil t)
	  (push (list (match-string 1) (match-string 2) (cadr elem))
		titles)))
      (setq titles (nreverse titles))
      (dolist (elem titles)
	(destructuring-bind (url title publisher) elem
	  (with-temp-buffer
	    (sleep-for 1)
	    (ignore-errors
	      (mm-url-insert-file-contents
	       (format "%s%s" base url)))
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
		(unless (ignore-errors
			  (mm-url-insert-file-contents
			   (format "http://www.milehighcomics.com%s" url))
			  t)
		  (sleep-for 1))
		(goto-char (point-min))
		(when (re-search-forward "^<b>" nil t)
		  (push (buffer-substring (point)
					  (progn (re-search-forward ".*You Pay Only Or Less")
						 (match-beginning 0)))
			html)))))
	    (when html
	      (save-excursion
		(set-buffer output)
		(goto-char (point-max))
		(insert (car html))
		(insert "<p>")))))))))
	
(defun fetch-genre ()
  (let ((url "http://www.milehighcomics.com/cgi-bin/genresearch.cgi?title=OV2010")
	(mm-url-use-external nil)
	urls)
    (save-excursion
      (set-buffer (get-buffer-create "*genre*"))
      (erase-buffer)
      (insert "<body bgcolor=black text=white>\n"))
    (with-temp-buffer
      (mm-url-insert-file-contents url)
      (goto-char (point-min))
      (while (re-search-forward "/cgi-bin/genresearch.cgi[^\"]+" nil t)
	(push (match-string 0) urls)))
    (dolist (url (reverse urls))
      (let ((issues nil)
	    contents)
	(with-temp-buffer
	  (mm-url-insert-file-contents
	   (concat "http://www.milehighcomics.com" url))
	  (goto-char (point-min))
	  (while (re-search-forward "/cgi-bin/genresearch.cgi\\?action=issue[^\"]+" nil t)
	    (push (match-string 0) issues)))
	(dolist (issue (reverse issues))
	  (message "%s" issue)
	  (with-temp-buffer
	    (mm-url-insert-file-contents
	     (concat "http://www.milehighcomics.com" issue))
	    (re-search-forward "\n<IMG SRC\\|\n<TABLE BORDER>")
	    (let ((start (match-beginning 0)))
	      (search-forward "View Your Shopping")
	      (beginning-of-line)
	      (setq contents (buffer-substring start (point)))
	      (save-excursion
		(set-buffer (get-buffer-create "*genre*"))
		(goto-char (point-max))
		(insert contents)))))))
    (set-buffer "*genre*")
    (goto-char (point-min))
    (replace-regexp "/cgi-bin/ebasket.cgi" "http://www.milehighcomics.com/cgi-bin/ebasket.cgi")
    ))

(provide 'previews)

;;; csid.el ends here
