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

(require 'json)
(require 'dom)
(require 'shr)

(defvar previews-mail-address nil
  "The address to notify when we got a new month's data.")

(defvar previews-data-directory "~/src/emerald/data/")

(defun previews-check ()
  "Check whether this month's previews exists, and if so, download it."
  (interactive)
  (let ((time (float-time (current-time)))
	did)
    (unless (file-exists-p (previews-file time))
      (setq did (previews-index time)))
    (incf time (* 60 60 24 12))
    (unless (file-exists-p (previews-file time))
      (setq did (previews-index time)))
    (when (and did
	       previews-mail-address)
      (message-mail previews-mail-address
		    (format "Previews for %s %s has been downloaded"
			    month year))
      (insert "Indeed.")
      (message-send-and-exit))))

(defun previews-index-by-date (date)
  (previews-index (float-time (apply 'encode-time
				     (mapcar (lambda (e)
					       (or e 0))
					     (parse-time-string date))))))

(defun previews-index (time)
  (set-locale-environment "C")
  (with-current-buffer (url-retrieve-synchronously
			(format "http://www.previewsworld.com/Catalog/CustomerOrderForm/TXT/%s%s"
				(upcase (format-time-string "%h" time))
				(format-time-string "%y" time))
			t t)
    (goto-char (point-min))
    (when (and (search-forward "\n\n" nil )
	       (= (following-char) 255))
      (decode-coding-region (point) (point-max) 'utf-16))
    (if (not (search-forward "PREVIEWS" nil t))
	nil
      (previews-fetch-and-write
       (previews-interpret-index
	(previews-parse-index))
       time))))

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
       ((looking-at ".*\t")
	(push (cons publisher
		    (split-string (buffer-substring (point) (line-end-position))
				  "\t"))
	      comics))
       ((looking-at ".+")
	(setq publisher (match-string 0))))
      (forward-line 1))
    (nreverse comics)))

(defun previews-interpret-index (index)
  (loop with prev-name
	for (publisher class code title date price) in index
	;; Sometimes there's an extra TAB before the title.  In that
	;; case, shift values down.
	do (when (zerop (length title))
	     (setq title date
		   date price
		   price ""))
	collect (let ((data `((:publisher . ,publisher)
			      (:code . ,(replace-regexp-in-string " " "" code))
			      (:price . ,(replace-regexp-in-string "SRP: " "" price))
			      (:date . ,date))))
		  (when (plusp (length class))
		    (nconc data (list (cons :class class))))
		  (with-temp-buffer
		    (insert title)
		    (goto-char (point-min))
		    (when (re-search-forward " +(OF \\([0-9]+\\))" nil t)
		      (nconc data (list (cons :duration (match-string 1))))
		      (replace-match ""))
		    (goto-char (point-min))
		    (when (re-search-forward " +(C: \\([-0-9]+\\))" nil t)
		      (nconc data (list (cons :comething (match-string 1))))
		      (replace-match ""))
		    (goto-char (point-min))
		    (when (re-search-forward " +(\\([A-Z][A-Z][A-Z][0-9][0-9][0-9][0-9][0-9][0-9]\\))" nil t)
		      (nconc data (list (cons :original (match-string 1))))
		      (replace-match ""))
		    (goto-char (point-min))
		    (when (re-search-forward " +(RES)" nil t)
		      (nconc data (list (cons :resolicited t)))
		      (replace-match ""))
		    (goto-char (point-min))
		    (when (re-search-forward " +(MR)" nil t)
		      (nconc data (list (cons :mature t)))
		      (replace-match ""))
		    (goto-char (point-min))
		    (when (re-search-forward " +(O/A)" nil t)
		      (replace-match ""))
		    (goto-char (point-min))
		    (when (re-search-forward "\\b\\(GN\\|TP\\|HC\\|SC\\)\\b"
					     nil t)
		      (nconc data (list (cons :binding (match-string 1)))))
		    (goto-char (point-min))
		    (cond
		     ;; Variants.
		     ((looking-at "\\(.*\\) +\\(\\(#[^ ]+\\)\\|\\(.*VOL [^ ]+\\)\\)")
		      (nconc data (list (cons :issue (match-string 2))
					(cons :title (match-string 1))))
		      (setq name (match-string 0))
		      (when (equal name prev-name)
			(nconc data (list (cons :variant t))))
		      (setq prev-name name))
		     ;; T-shirts.
		     ((looking-at "\\(.* +T/S\\)")
		      (nconc data (list (cons :title (match-string 1))))
		      (setq name (match-string 1))
		      (when (equal name prev-name)
			(nconc data (list (cons :variant t))))
		      (setq prev-name name))
		     ;; Hardcovers/softcovers
		     ((looking-at "\\(.*\\) \\(HC\\|SC\\)\\b\\(.*\\)")
		      (setq name (concat (match-string 1)
					 (or (match-string 3) "")))
		      (nconc data (list (cons :title name)))
		      (when (equal name prev-name)
			(nconc data (list (cons :variant t))))
		      (setq prev-name name)))		     
		    (nconc data (list (cons :name (buffer-string)))))
		  data)))

(defun previews-fetch-and-write (index time)
  (with-temp-buffer
    (insert (json-encode (previews-fetch index)))
    (let ((coding-system-for-write 'utf-8))
      (write-region (point-min) (point-max) (previews-file time))))
  (with-temp-buffer
    (insert (format-time-string "emeraldDate = '%Y-%m';\n" time))
    (insert "emeraldDates = ")
    (insert
     (json-encode
      (loop for file in (directory-files previews-data-directory
					 nil "previews.*json")
	    when (string-match "-\\([-0-9]+\\)" file)
	    collect (match-string 1 file))))
    (insert ";\n")
    (write-region (point-min) (point-max)
		  (expand-file-name "timestamp.js" previews-data-directory))))

(defun previews-fetch (index)
  (loop for elem in index
	collect (let ((data (previews-fetch-id (cdr (assq :code elem))))
		      (shr-base (shr-parse-base "http://www.previewsworld.com/")))
		  (message "%s" (cdr (assq :name elem)))
		  (append elem
			  `((:img . ,(and (plusp (length (nth 0 data)))
					  (shr-expand-url (nth 0 data))))
			    (:creators . ,(nth 1 data))
			    (:text . ,(nth 2 data)))))))

(defun previews-file (time)
  (expand-file-name
   (format-time-string "previews-%Y-%m.json" time)
   previews-data-directory))

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
  ;; Image.
  (list (dom-attr (dom-by-tag (dom-by-class dom "^mainContentImage$") 'img)
		  'src)
	;; Creators.
	(dom-texts (dom-by-class dom "^Creators$"))
	;; Text.
	(dom-text (dom-by-class dom "^Text$"))
	;; Price.
	(car (last (split-string
		    (dom-texts (dom-by-class dom "^SRP$")))))))

(defun previews-cache-images (month)
  (let ((dir (expand-file-name (format "img/%s" month)
			       previews-data-directory))
	(json
	 (with-temp-buffer
	   (insert-file-contents (expand-file-name (format "previews-%s.json" month)
						   previews-data-directory))
	   (json-read))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (loop for comic across json
	  do (let ((src (cdr (assq 'img comic)))
		   (code (cdr (assq 'code comic))))
	       (when (and src code)
		 (let ((output (expand-file-name (format "%s-full.jpg" code) dir)))
		   (unless (file-exists-p output)
		     (message "%s" src)
		     (call-process "curl" nil nil nil
				   "-o" output
				   "-L"
				   "-q" src)
		     (when (file-exists-p output)
		       (if (zerop (file-attribute-size (file-attributes output)))
			   (delete-file output)
			 (call-process "convert" nil nil nil
				       "-resize" "600x" output
				       (replace-regexp-in-string "-full.jpg" "-scale.jpg" output))))
		     (sleep-for 5))))))))

(defun previews-make-cache ()
  (dolist (file (directory-files previews-data-directory nil "previews.*json"))
    (when (string-match "previews-\\([-0-9]+\\).json" file)
      (let ((month (match-string 1 file)))
	(unless (file-exists-p (expand-file-name
				(format "img/%s" month) previews-data-directory))
	  (previews-cache-images month))))))

(provide 'previews)

;;; previews.el ends here
