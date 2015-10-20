;;; previews.el --- Make a single HTML page for all new comics
;; Copyright (C) 2015 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: comics

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

(defun previews-index ()
  (with-current-buffer (url-retrieve-synchronously
			(format "http://www.previewsworld.com/support/previews_docs/orderforms/archive/%s/%s%s_COF.txt"
				(format-time-string "%Y")
				(upcase (format-time-string "%h"))
				(format-time-string "%y"))
			t t)
    (previews-fetch
     (previews-parse-index))))

(defun previews-parse-index ()
  (goto-char (point-min))
  (while (re-search-forward "\r" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "^PAGE" nil t)
    (delete-region (match-beginning 0) (line-beginning-position 2)))
  (goto-char (point-min))
  (let ((publisher nil)
	(comics nil))
    (while (not (eobp))
      (cond
       ((looking-at "[^\t\n]*\t\\([^\t\n]+\\)\t\\([^\t\n]+\\)")
	(push (list publisher (match-string 1) (match-string 2))
	      comics))
       ((looking-at ".+")
	(setq publisher (match-string 0))))
      (forward-line 1))
    (nreverse comics)))

(defun previews-fetch (index)
  (with-temp-buffer
    (loop with prev-publisher
	  for (publisher id title) in index
	  if (or (and (not (string-match "\\bCVR\\b" title))
		      (not (string-match "\\bVAR\\b" title)))
		 (string-match "\\bREG\\b" title)
		 (string-match "\\bMAIN\\b" title)
		 (string-match "\\bA\\b" title))
	  do (unless (equal prev-publisher publisher)
	       (insert (format "<h1>%s</h1><p>\n" (capitalize publisher)))
	       (setq prev-publisher publisher))
	  (message "%s (%s)"
		   title
		   publisher)
	  (insert "<p>")
	  (insert (replace-regexp-in-string
		   "#1\\b"
		   "<b style=\"color: red;\">\\&</b>"
		   (replace-regexp-in-string " +(C: [-0-9]+) *" "" title))
		  "\n")
	  (let ((data (previews-fetch-id id))
		(shr-base (shr-parse-base "http://www.previewsworld.com/")))
	    (insert (format "<p>%s (%s)<p>%s<p>\n"
			    (nth 1 data)
			    (nth 3 data)
			    (nth 2 data)))
	    (when (nth 0 data)
	      (insert (format "<img src=\"%s\">"
			      (shr-expand-url (nth 0 data))))))
	  else
	  do (message "Skipping %s" title))
    (let ((coding-system-for-write 'utf-8))
      (write-region (point-min) (point-max) (previews-file)))))

(defun previews-file ()
  (format-time-string "~/tmp/previews-%Y-%h.html"))

(defun previews-fetch-id (id)
  (with-current-buffer
    (url-retrieve-synchronously
     (format "http://www.previewsworld.com/Catalog/%s"
	     (replace-regexp-in-string " +" "" id))
     t t)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r\n]+" nil t)
      (replace-match " " t t))
    (prog1
	(previews-parse-dom
	 (libxml-parse-html-region (point-min) (point-max)))
      (kill-buffer (current-buffer)))))

(defun previews-parse-dom (dom)
  (list (dom-attr (dom-by-tag (dom-by-class dom "FancyPopupImage") 'img)
		  'src)
	(dom-texts (dom-by-class dom "StockCodeCreators"))
	(dom-texts (dom-by-class dom "PreviewsHtml"))
	(car (last (split-string
		    (dom-texts (dom-by-class dom "StockCodeSrp")))))))

(provide 'previews)

;;; previews.el ends here
