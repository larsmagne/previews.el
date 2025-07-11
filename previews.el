;;; previews.el --- Make a single HTML page for all new comics -*- lexical-binding: t -*-
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
(require 'time-date)
(require 'find-func)

(defvar previews-mail-address nil
  "The address to notify when we got a new month's data.")

(defvar previews-data-directory "~/src/emerald/data/")

(defun previews-check ()
  "Check whether this month's previews exists, and if so, download it."
  (interactive)
  (let ((time (float-time (current-time)))
	did)
    (cl-incf time (* 60 60 24 12))
    (unless (file-exists-p (previews-file time))
      (setq did (previews-index time)))
    (when (and did
	       previews-mail-address)
      (message-mail previews-mail-address
		    (format "Previews for %s has been downloaded"
			    (format-time-string "%Y-%m" time)))
      (insert "Indeed.")
      (message-send-and-exit))))

(defun previews-index-by-date (date)
  (previews-index (float-time (apply 'encode-time
				     (mapcar (lambda (e)
					       (or e 0))
					     (parse-time-string date))))))

(defun previews-index (time)
  (when-let ((diamond (previews--index-diamond time))
	     (lunar (seq-uniq (previews--index-lunar time)
			      (lambda (e1 e2)
				(equal (cdr (assq :code e1))
				       (cdr (assq :code e2))))))
	     (prh (previews--index-prh (format-time-string "%Y-%m" time))))
    ;; Sort the publishers alphabetically.
    (setq diamond
	  (sort (append lunar prh diamond)
		(lambda (e1 e2)
		  (cond
		   ;; Search merch to the end.
		   ((and (assq :merch e1)
			 (not (assq :merch e2)))
		    nil)
		   ((and (not (assq :merch e1))
			 (assq :merch e2))
		    t)
		   ;; Sort by publisher within non-merch/merch.
		   (t
		    (if (equal (downcase (cdr (assq :publisher e1)))
			       (downcase (cdr (assq :publisher e2))))
			(string< (cdr (assq :publisher e1))
				 (cdr (assq :publisher e2)))
		      (string< (downcase (cdr (assq :publisher e1)))
			       (downcase (cdr (assq :publisher e2))))))))))
    (previews-fetch-and-write diamond time)
    t))

(defun previews--index-diamond (time)
  (set-locale-environment "C")
  (with-current-buffer (url-retrieve-synchronously
			(format "https://www.previewsworld.com/Catalog/CustomerOrderForm/TXT/%s%s"
				(upcase (format-time-string "%h" time))
				(format-time-string "%y" time))
			t t)
    (goto-char (point-min))
    (when (and (search-forward "\n\n" nil )
	       (= (following-char) 255))
      (decode-coding-region (point) (point-max) 'utf-16))
    (if (not (search-forward "PREVIEWS" nil t))
	nil
      (previews-interpret-index (previews-parse-index)))))

(defun previews-parse-index ()
  (goto-char (point-min))
  (while (re-search-forward "\r" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "^PAGE" nil t)
    (delete-region (match-beginning 0) (line-beginning-position 2)))
  (goto-char (point-min))
  (let ((publisher nil)
	(merchandise nil)
	(comics nil))
    (while (not (eobp))
      (cond
       ((looking-at ".*\t")
	(push (list publisher merchandise
		    (split-string (buffer-substring (point) (line-end-position))
				  "\t"))
	      comics))
       ((looking-at "MERCHANDISE *$")
	(setq merchandise t))
       ((looking-at ".+")
	(setq publisher (match-string 0))))
      (forward-line 1))
    (nreverse comics)))

(defun previews-interpret-index (index)
  (cl-loop with prev-name
	   for (publisher merchandise (class code title date price)) in index
	   ;; Sometimes there's an extra TAB before the title.  In that
	   ;; case, shift values down.
	   do (when (zerop (length title))
		(setq title date
		      date price
		      price ""))
	   collect (let ((data
			  `((:publisher . ,publisher)
			    (:code . ,(replace-regexp-in-string " " "" code))
			    (:price . ,(replace-regexp-in-string "SRP: " ""
								 price))
			    (:date . ,date)
			    (:distributor . "Diamond")))
			 name)
		     (when (plusp (length class))
		       (nconc data (list (cons :class class))))
		     (when merchandise
		       (nconc data (list (cons :merch "t"))))
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
		       (when (re-search-forward
			      "\\b\\(HC GN\\|GN\\|TP\\|HC\\|SC\\)\\b"
			      nil t)
			 (nconc data (list (cons :binding (match-string 1)))))
		       (goto-char (point-min))
		       (cond
			;; Variants.
			((looking-at "\\(.*\\) +\\(\\(#[^ ]+\\)\\|\\(.*VOL [^ ]+\\)\\|ONE SHOT\\)")
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
			((or
			  (looking-at "\\(.*\\) \\(LTD ED HC\\|HC GN\\|SGN ED\\)\\b\\(.*?\\)")
			  (looking-at "\\(.*\\) \\(HC\\|SC\\|GN\\)\\b\\(.*?\\)"))
			 (setq title (concat (match-string 1)
					     (or (match-string 3) "")))
			 (nconc data (list (cons :title title)))
			 (when (equal title prev-name)
			   (nconc data (list (cons :variant t))))
			 (setq prev-name title)))		     
		       (nconc data (list (cons :name (buffer-string)))))
		     data)))

(defun previews-fetch-and-write (index time)
  (with-temp-buffer
    (insert (json-encode (previews-fetch index)))
    (let ((coding-system-for-write 'utf-8))
      (write-region (point-min) (point-max) (previews-file time))))
  (previews-make-cache)
  (previews-cache-images (format-time-string "%Y-%m" time))
  (with-temp-buffer
    (insert (format-time-string "emeraldDate = '%Y-%m';\n" time))
    (insert "emeraldDates = ")
    (insert
     (json-encode
      (cl-loop for file in (directory-files previews-data-directory
					    nil "previews.*json")
	       when (string-match "-\\([-0-9]+\\)" file)
	       collect (match-string 1 file))))
    (insert ";\n")
    (write-region (point-min) (point-max)
		  (expand-file-name "timestamp.js" previews-data-directory))))

(defun previews-fetch (index)
  (cl-loop for elem in index
	   collect (if (assq :text elem)
		       ;; If we already have the text, then we don't have
		       ;; to fetch anything.
		       elem
		     ;; Fetch the text from Diamond.
		     (let ((data (previews-fetch-id (cdr (assq :code elem))))
			   (shr-base (shr-parse-base
				      "https://www.previewsworld.com/")))
		       (message "%s" (cdr (assq :name elem)))
		       (append elem
			       `((:img . ,(and (plusp (length (nth 0 data)))
					       (shr-expand-url (nth 0 data))))
				 (:creators . ,(nth 1 data))
				 (:text . ,(string-trim (nth 2 data)))))))))

(defun previews-file (time)
  (expand-file-name
   (format-time-string "previews-%Y-%m.json" time)
   previews-data-directory))

(defun previews-fetch-id (id)
  (with-current-buffer
      (url-retrieve-synchronously
       (format "https://www.previewsworld.com/Catalog/%s"
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
  (list (dom-attr (dom-by-id dom "mainContentImage") 'src)
	;; Creators.
	(dom-texts (dom-by-class dom "^Creators$"))
	;; Text.
	(string-join
	 (cl-loop for child in (dom-children (dom-by-class dom "^Text$"))
		  when (or (stringp child)
			   (eq (dom-tag child) 'i))
		  collect (if (stringp child)
			      child
			    (dom-text child))))
	;; Price.
	(car (last (split-string
		    (dom-texts (dom-by-class dom "^SRP$")))))))

(defun previews-cache-images (month &optional refresh)
  (let ((dir (expand-file-name (format "img/%s" month)
			       previews-data-directory))
	(json
	 (with-temp-buffer
	   (insert-file-contents
	    (expand-file-name (format "previews-%s.json" month)
			      previews-data-directory))
	   (json-read))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (cl-loop for comic across json
	     do (let ((src (cdr (assq 'img comic)))
		      (code (cdr (assq 'code comic))))
		  (when (and src code)
		    (let ((output (expand-file-name
				   (format "%s-full.jpg" code) dir)))
		      (when (or (not (file-exists-p output))
				(and refresh
				     (previews--placeholder-image-p output)))
			(message "%s" src)
			(if (not (string-match "/tif/" src))
			    (call-process "curl" nil nil nil
					  "-o" output
					  "-L"
					  "-q" src)
			  (call-process "curl" nil nil nil
					"-o" "/tmp/pfile.tiff"
					"-L"
					"-q" src)
			  (when (file-exists-p "/tmp/pfile.tiff")
			    (call-process "convert" nil nil nil
					  "/tmp/pfile.tiff" output)
			    (delete-file "/tmp/pfile.tiff")))
			(when (file-exists-p output)
			  (if (zerop (nth 7 (file-attributes output)))
			      (delete-file output)
			    (call-process "convert" nil nil nil
					  "-resize" "600x" output
					  (replace-regexp-in-string
					   "-full.jpg" "-scale.jpg" output))))
			(sleep-for 5))))))))

(defun previews--placeholder-image-p (file)
  (with-temp-buffer
    (call-process "file" nil t nil file)
    (goto-char (point-min))
    (and (search-forward ": " nil t)
	 (re-search-forward "\\([0-9]+\\) x \\([0-9]+\\)," nil t)
	 (< (string-to-number (match-string 1)) 200))))

(defun previews-refresh-placeholders ()
  (interactive)
  (let* ((regexp "previews-\\([-0-9]+\\)[.]json\\'")
	 (latest (car (last (directory-files
			     previews-data-directory t regexp)))))
    (and (string-match regexp latest)
	 (previews-cache-images (match-string 1 latest) t))))

(defun previews-make-cache ()
  (dolist (file (directory-files previews-data-directory nil "previews.*json"))
    (when (string-match "previews-\\([-0-9]+\\).json" file)
      (let ((month (match-string 1 file)))
	(unless (file-exists-p (expand-file-name
				(format "img/%s" month) previews-data-directory))
	  (previews-cache-images month))))))

(defun previews--index-lunar (time)
  (let ((decoded (decode-time time)))
    (setq decoded (decoded-time-add decoded (make-decoded-time :month 2)))
    (setf (decoded-time-day decoded) 1)
    (cl-loop with month = (decoded-time-month decoded)
	     while (= month (decoded-time-month decoded))
	     when (= (decoded-time-weekday (decode-time (encode-time decoded)))
		     2)
	     append (previews--index-lunar-1 (encode-time decoded))
	     do (setq decoded (decoded-time-add decoded
						(make-decoded-time :day 1))))))
	     
(defun previews--index-lunar-1 (time)
  (let* ((date (format-time-string "%m/%d/%Y" time))
	 (url (format "https://www.lunardistribution.com//?foc=&release=%s"
		      date)))
    (with-current-buffer
	(url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
	 (previews--parse-lunar
	  (libxml-parse-html-region (point) (point-max)))))))

(defun previews--parse-lunar (dom)
  (cl-loop with prev-name
	   for elem in (dom-by-class dom "productdetail")
	   collect (let* ((name (dom-attr elem 'data-title))
			  (code (dom-attr elem 'data-code))
			  (data
			   `((:publisher . ,(previews--lunar-publisher
					     (substring code 4 6)))
			     (:code . ,code)
			     (:price . ,(dom-attr elem 'data-retail))
			     (:date . ,(dom-attr elem 'data-instore))
			     (:creators . ,(dom-attr elem 'data-creators))
			     (:text  . ,(dom-attr elem 'data-desc))
			     (:distributor . "Lunar")
			     (:img . ,(dom-attr elem 'data-img)))))
		     (with-temp-buffer
		       (insert name)
		       (goto-char (point-min))
		       (when (re-search-forward " +(MR)" nil t)
			 (nconc data (list (cons :mature t)))
			 (replace-match ""))
		       (goto-char (point-min))
		       (when (re-search-forward
			      "\\b\\(HC GN\\|GN\\|TP\\|HC\\|SC\\)\\b"
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
			 (setq prev-name name)))		     
		       (nconc data (list (cons :name (buffer-string)))))
		     data)))

(defvar previews--lunar-publishers
  '((DC "DC Comics")
    (TM "Twomorrows")
    (IM "Image")
    (DE "Dynamite")
    (RE "Rebellion")
    (ON "Oni")
    (MA "Mad Cave")
    (BM "Black Mask")
    (CS "Comic Shop News")
    (RE "Rocketship Entertainment")
    (AH "Ahoy Comics")
    (MP "Panick Entertainment")
    (RC "Rekcah Comics")
    (IP "IPI Comics")
    (BD "Bad Idea")
    (AW "Awa")
    (UB "U B")
    (VL "V L")
    (PZ "Papercutz")
    (CP "Clover Press")
    (PG "PUGW")
    (FB "Fantagraphics Books")
    (DQ "Drawn & Quarterly")
    (AC "Archie Comics")))

(defun previews--lunar-publisher (code)
  (or (cadr (assq (intern code) previews--lunar-publishers))
      code))

(defun previews--fill-comic (comic)
  (let ((dom
	 (with-current-buffer
	     (url-retrieve-synchronously
	      (concat "https://prhcomics.com/book/?isbn="
		      (cdr (assq :code comic))))
	   (goto-char (point-min))
	   (prog1
	       (and (search-forward "\n\n" nil t)
		    (libxml-parse-html-region (point) (point-max)))
	     (kill-buffer (current-buffer))))))
    (setcdr (assq :text comic)
	    (string-trim
	     (string-clean-whitespace
	      (dom-texts (dom-by-class dom "book-detail-about")))))
    (setcdr (assq :creators comic)
	    (string-join
	     (seq-uniq
	      (cl-loop for author in (dom-by-class dom "book-detail-author")
		       collect (dom-text (dom-by-tag author 'a))))
	     ", "))
    comic))

(defun previews--index-prh (date &optional inhibit-fetch)
  (unless inhibit-fetch
    (call-process (expand-file-name "prhget.py"
				    (file-name-directory
				     (find-library-name "previews")))
		  nil nil nil date))
  (let ((xlsx (car (sort (directory-files "/tmp/prh/" t "[.]xlsx\\'")
			 #'file-newer-than-file-p))))
    (call-process "ssconvert" nil nil nil
		  "-O" "separator=\"\x1e\" format=raw"
		  xlsx "/tmp/prh/prh.txt")
    (with-temp-buffer
      (insert-file-contents "/tmp/prh/prh.txt")
      (re-search-forward "^ISBN")
      (forward-line 1)
      (let ((comics nil))
	(while (not (eobp))
	  (let* ((c (mapcar
		     (lambda (elem)
		       (replace-regexp-in-string "\\`[\"]\\|[\"]\\'" "" elem))
		     (split-string (buffer-substring (point) (pos-eol))
				   "\x1e")))
		 (name (nth 2 c))
		 (title name)
		 (issue ""))
	    (when (string-match " \\(#[0-9]+\\|Vol\\(ume\\|[.]\\)? [0-9]+\\)"
				title)
	      (setq issue (match-string 1 title)
		    title (substring title 0 (match-beginning 0))))
	    (push
	     `((:publisher . ,(nth 5 c))
	       (:code . ,(nth 0 c))
	       (:price . ,(string-replace " US" "" (nth 6 c)))
	       (:date . ,(nth 10 c))
	       (:creators . ,"")
	       (:text  . ,"")
	       (:issue . ,issue)
	       (:title . ,title)
	       (:name . ,name)
	       (:img . ,(concat
			 "https://images.penguinrandomhouse.com/cover/tif/"
			 (nth 0 c)))
	       (:distributor . "PRH"))
	     comics))
	  (forward-line 1))
	(setq comics (nreverse comics))
	(unless inhibit-fetch
	  (cl-loop for comic in comics
		   do
		   (previews--fill-comic comic)
		   (sleep-for 5)))
	comics))))

(defun previews--data (month)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (format "previews-%s.json" month)
		       previews-data-directory))
    (json-parse-buffer :object-type 'alist)))

(defun previews--update-prh (month)
  (let ((json (previews--data month))
	(comics (previews--index-prh month t)))
    (cl-loop for comic in comics
	     for code = (cdr (assq :code comic))
	     for old-comic = (cl-loop for old across json
				      when (equal code (cdr (assq 'code old)))
				      return old)
	     when old-comic
	     do (cl-loop
		 for slot in '(publisher price date title issue name)
		 do (if (assq slot old-comic)
			(setcdr (assq slot old-comic)
				(cdr (assq (intern (format ":%s" slot))
					   comic)))
		      (nconc old-comic
			     (list (cons slot
					 (cdr (assq (intern (format ":%s" slot))
						    comic))))))))
    json))

(defun previews--update-json (json month)
  (with-temp-buffer
    (json-insert json)
    (write-region (point-min) (point-max)
		  (expand-file-name (format "previews-%s.json" month)
				    previews-data-directory))))

(provide 'previews)

;;; previews.el ends here
