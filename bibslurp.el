;;; bibslurp.el --- retrieve BibTeX entries from NASA ADS

;; Copyright (C) 2013 Mike McCourt
;;
;; Authors: Mike McCourt <mkmcc@astro.berkeley.edu>
;; URL: https://github.com/mkmcc/bibslurp
;; Version: 0.0.2
;; Keywords: bibliography, nasa ads
;; Package-Requires: ((s "1.6.0") (dash "1.5.0"))

;; This file is not part of GNU Emacs.

;; bibslurp is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; bibslurp is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with bibslurp.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Provides a function `bibslurp-query-ads', which reads a search
;; string from the minibuffer, sends the query to NASA ADS
;; (http://adswww.harvard.edu/), and displays the results in a new
;; buffer called "ADS Search Results".

;; The "ADS Search Results" buffer opens in `bibslurp-mode', which
;; provides a few handy functions.  Typing the number preceding an
;; abstract and hitting RET calls `bibslurp-slurp-bibtex', which
;; fetches the bibtex entry corresponding to the abstract and saves it
;; to the kill ring.  Typing 'a' instead pulls up the abstract page.
;; At anytime, you can hit 'q' to quit bibslurp-mode and restore the
;; previous window configuration.

;;; Example usage:

;; add an entry to a bibtex buffer:
;;   M-x bibslurp-query-ads RET ^Quataert 2008
;;   1 RET
;;   q
;;   C-y

;; For more examples and information see the project page at
;; http://astro.berkeley.edu/~mkmcc/software/bibslurp.html

;;; Notes about the implementation:

;; 1. As far as I know, ADS doesn't have an API for searching its
;;   database, and emacs doesn't have functionality to parse html.
;;   Since I don't want to implement a browser in emacs lisp, that
;;   leaves me parsing the html pages with regexps.  While that would
;;   be a terrible idea under ordinary circumstances, the ADS pages
;;   are all automatically generated, so they should conform to a
;;   pretty regular format.  That gives me some hope...

;; 2. There are many ways you can customize the behaviour of biblurp.
;;    I define font-lock faces at the beginning of the file so you can
;;    add these to your color theme.  I also run a mode hook at the
;;    end of `bibslurp-mode', so you can inject your own code at that
;;    point.

;;; Installation:

;; Use package.el. You'll need to add MELPA to your archives:

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Alternatively, you can just save this file and do the standard
;; (add-to-list 'load-path "/path/to/bibslurp.el")
;; (require 'bibslurp)

;;; TODO:
;;  1. add hooks for show-abstract?
;;  2. finish documentation
;;  3. clean up code
;;  4. rethink regexps
;;  5. add a function to download pdfs?


;;; Code:
(require 's)
(require 'dash)



;;; start by making a rudimentary web browser
;; define font-lock faces
(make-face 'bibslurp-number-face)
(make-face 'bibslurp-name-face)
(make-face 'bibslurp-score-face)
(make-face 'bibslurp-date-face)
(make-face 'bibslurp-author-face)
(make-face 'bibslurp-title-face)

(set-face-attribute 'bibslurp-number-face nil
                    :inherit 'font-lock-string-face)
(set-face-attribute 'bibslurp-score-face nil
                    :inherit 'font-lock-comment-face)
(set-face-attribute 'bibslurp-author-face nil
                    :inherit 'font-lock-builtin-face)
(set-face-attribute 'bibslurp-title-face nil
                    :inherit 'font-lock-string-face)
(set-face-attribute 'bibslurp-name-face nil
                    :slant 'italic)
(set-face-attribute 'bibslurp-date-face nil
                    :inherit 'font-lock-variable-name-face)

;; key bindings
(defvar bibslurp-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") 'bibslurp-slurp-bibtex)
    (define-key map (kbd "z")   'bibslurp-slurp-bibtex)
    (define-key map (kbd "SPC")   'scroll-up)
    (define-key map (kbd "S-SPC") 'scroll-down)
    (define-key map ">" 'end-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map "r" 'isearch-backward)
    (define-key map "s" 'isearch-forward)
    (define-key map "q" 'bibslurp-quit)
    (define-key map "a" 'bibslurp-show-abstract)
    (define-key map "n" 'bibslurp-next-entry)
    (define-key map "p" 'bibslurp-previous-entry)
    map)
  "Keymap for bibslurp mode.")

(define-derived-mode bibslurp-mode fundamental-mode "BibSlurp"
  "Major mode for perusing ADS search results and slurping bibtex
entries to the kill-ring.  This is pretty specific, so you should
only enter the mode via `bibslurp-query-ads'.

\\<bibslurp-mode-map>"
  (use-local-map bibslurp-mode-map))

(defun bibslurp/follow-link (number)
  "take a link number and return the corresponding url as a
string.

argument may be either an integer or a string.  returns nil if
the link number is invalid.  links are stored in the list
`bibslurp/link-list', which is populated by `bibslurp-query-ads'
once the search results are returned."
  (interactive "P")
  (let* ((link-number
          (if (stringp number)
              (string-to-number number)
            number)))
    (nth (- link-number 1) bibslurp/link-list)))

(defun bibslurp-quit ()
  "Close the bibslurp buffer and restore the previous window
configuration."
  (interactive)
  (when (eq major-mode 'bibslurp-mode)
    (kill-buffer)
    (when (get-register :bibslurp-window)
      (jump-to-register :bibslurp-window))))

(defun bibslurp/build-ads-url (search-string)
  "Helper function which turns a search string (e.g. \"^Quataert
2008\") into an ads search url.  Used by `bibslurp-query-ads'."
  (let ((base-url
         "http://adsabs.harvard.edu/cgi-bin/nph-basic_connect?qsearch=")
        (url-sep "+")
        (url-end "&version=1"))
    (concat base-url
            (replace-regexp-in-string " " url-sep search-string)
            url-end)))


;; functions to parse and display the search results page.
(defvar bibslurp/link-list nil
  "list of abstract URLs for the current search.")

(defvar bibslurp-query-history nil
  "History for `bibslurp-query-ads'.")

;;;###autoload
(defun bibslurp-query-ads (search-string)
  "Interactive function which asks for a search string and sends
the query to NASA ADS.  Displays results in a new buffer called
\"ADS Search Results\" and enters `bibslurp-mode'.  You can
retrieve a bibtex entry by typing the number in front of the
abstract link and hitting enter.  Hit 'a' instead to pull up the
abstract.  You can exit the mode at any time by hitting 'q'."
  (interactive (list (read-string "Search string: " nil 'bibslurp-query-history)))
  (let ((search-url (bibslurp/build-ads-url search-string))
        (buf (get-buffer-create "ADS Search Results"))
        (inhibit-read-only t)
        (clean-list))
    (with-temp-buffer
      (url-insert-file-contents search-url)
      (setq clean-list (-map 'bibslurp/clean-entry (bibslurp/read-table)))
      (setq bibslurp/link-list '())
      (--map (add-to-list 'bibslurp/link-list (car (last it)) t) clean-list))
    (with-current-buffer buf
      (erase-buffer)
      (insert "ADS Search Results for \""
              (propertize search-string 'face 'font-lock-string-face)
              "\"\n\n")
      (insert
       (propertize
        (concat
         "Scroll with SPC and SHIFT-SPC, or search using 's' and 'r'."
         "\n\n"
         "* To slurp a bibtex entry, type the number of the abstract and hit RET."
         "\n\n"
         "* To view an abstract, type the number of the abstract and hit 'a'."
         "\n\n"
         "* To quit and restore the previous window configuration, hit 'q'."
         "\n\n\n\n") 'face 'font-lock-comment-face))
      (save-excursion
        (insert
         (mapconcat 'identity
		    (--map (apply 'bibslurp/print-entry it)
			   clean-list) ""))
	;; Shave off the last newlines
	(delete-char -4))
      (bibslurp-mode))
    (window-configuration-to-register :bibslurp-window)
    (switch-to-buffer buf)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (delete-other-windows)))

(defun bibslurp/read-table ()
  "Parse the HTML from a search results page.

TODO: describe in more detail.  also rethink this."
  (goto-char (point-min))
  ;; search results are printed in a <table> element.  annoyingly, one
  ;; result actually spans *two* adjacent table rows, so we keep a
  ;; temp variable to store and combine them.
  (re-search-forward "<table>")
  (let ((rows '())
        (temp '()))
    ;; find the next <tr>...</tr> block
    (while (re-search-forward "<tr>" nil t)
      (let ((end (save-excursion
                   (re-search-forward "</tr>")
                   (point)))
            (data '()))
        ;; populate data with the <td>...</td> entries
        (while (re-search-forward "<td[^>]*>\\(.*?\\)</td>" end t)
          (add-to-list 'data (match-string-no-properties 1) t))
        ;; search results start with a number.  if this is a new
        ;; search result, store it in the temp variable.  otherwise,
        ;; if temp is non-nil, this is the continuation of a search
        ;; result.  append them and add to the rows list.
        (cond
         ((and (car data) (s-numeric? (car data)))
          (setq temp data))
         (temp
          (add-to-list 'rows (append temp data) t)
          (setq temp '())))))
    rows))

(defun bibslurp/clean-entry (entry)
  "Process the data returned by `bibslurp/read-table' into
something human readable.

Note that this function depends on the *order* of <td> elements
not changing in the ADS pages.  I pretty much have to hope that
that's the case..."
  (let ((num       (nth 0 entry))
        (link-data (nth 1 entry))
        (score     (nth 3 entry))
        (date      (nth 4 entry))
        (authors   (nth 7 entry))
        (title     (nth 9 entry)))
    (when (string-match "<a href=\"\\([^\"]+?\\)\">\\([^<]+\\)</a>" link-data)
      (let ((abs-url (match-string-no-properties 1 link-data))
            (abs-name (match-string-no-properties 2 link-data)))
        (list num score abs-name date authors title abs-url)))))

(defun bibslurp/print-entry (num score abs-name date authors title abs-url)
  "Format a single search result for printing.

TODO: this is really messy code.  cleanup."
  (let* ((fmt-num (concat
                   (make-string (- 3 (length num)) ? )
                   (format "[%s].  %s"
                           (propertize num 'face 'bibslurp-number-face)
                           (propertize abs-name 'face 'bibslurp-name-face))))
         (fmt-score (propertize (format "(%s)" score) 'face 'bibslurp-score-face))
         (pad (make-string (- 80 (length fmt-num) (length fmt-score)) ? ))
         (meta (concat fmt-num pad fmt-score)))
    (propertize
     (concat meta "\n"
	     (s-truncate 80
			 (concat (make-string 8 ? )
				 (propertize (s-right 4 date) 'face 'bibslurp-date-face) " "
				 (propertize authors 'face 'bibslurp-author-face)))
	     "\n\n"
	     (when title (s-word-wrap 80 title))
	     "\n\n\n\n") 'number num)))

;; functions to find and retrieve bibtex entries
(defun bibslurp/absurl-to-bibdata (abs-url)
  "Take the URL of an ADS abstract page and return data about the
corresponding bibtex entry.

This list has the form (bib-url new-label), where bib-url is the
bib-url of the ADS bibtex page and new-label is the suggested
label.

new-label may be nil if a bibtex url is found, but it can't
suggest a new label.  If the bibtex url is not found, this
function simply returns nil."
  (let ((buf (url-retrieve-synchronously abs-url)))
    ;; define a url string as anything in double quotes, that doesn't
    ;; contain a double quote.  I think this is valid...
    ;;
    ;; I'm not sure if this regexp should be more permissive about
    ;; matching whitespace in different parts of the tag.  this seems
    ;; to work for ADS at least.
    (let ((bib-link-regex
           "<a\\s-*href=\\\"\\([^\\\"]+?\\)\\\"\\s-*/?>\\s-*Bibtex"))
      (with-current-buffer buf
        (goto-char (point-min))
        (when (re-search-forward bib-link-regex nil t)
          (let ((bib-url (match-string-no-properties 1))
                (new-label (bibslurp/suggest-label)))
            (list bib-url new-label)))))))

(defun bibslurp/biburl-to-bib (bib-url &optional new-label)
  "Take the URL for an ADS bibtex entry and return the entry as a
string.  Optionally, replace the default (and useless) ADS label
with the argument NEW-LABEL."
  (let ((buf (url-retrieve-synchronously bib-url)))
    (with-current-buffer buf
      (goto-char (point-min))
      ;; first, look for a bibtex definition and replace the label if
      ;; appropriate.
      (when (re-search-forward "@\\sw+{\\([^,]+\\)," nil t)
        (when (and new-label (not (string-equal new-label "")))
          (replace-match new-label t t nil 1))
        ;; next, find the definition and return it.  use the nifty
        ;; function `forward-sexp' to navigate to the end.
        (goto-char (point-min))
        (re-search-forward "@\\sw+")
        (let ((bpoint (point)))
          (forward-sexp)
          (concat (match-string-no-properties 0)
                  (buffer-substring bpoint (point))))))))

(defun bibslurp-slurp-bibtex (&optional link-number)
  "Automatically find the bibtex entry for an abstract in the
NASA ADS database.

This function is rather specific -- it presumes you've used
`bibslurp-query-nasa-ads' to search ADS for an abstract.  Then, you
can call this function from the *Bibslurp* buffer.  It will prompt
for the number in front of the abstract you want, then will find
the bibtex entry and save it to the kill ring.

The functions `bibslurp/absurl-to-bibdata' and `bibslurp/biburl-to-bib' are
more general."
  (interactive)
  (setq link-number
	(or link-number
	    current-prefix-arg
	    (get-text-property (point) 'number)
	    (read-string "Link number: ")))
  (let ((bib-data (bibslurp/absurl-to-bibdata
		   (bibslurp/follow-link link-number))))
    (cond
     ((eq bib-data nil)
      (message "Couldn't find link to bibtex entry."))
     (t
      (let ((bib-url (car bib-data))
	    (new-label (cadr bib-data)))
	(kill-new (bibslurp/biburl-to-bib bib-url new-label)))
      (message "Saved bibtex entry to kill-ring.")))))

(defun bibslurp/suggest-label ()
  "Parse an abstract page and suggest a bibtex label.  Returns an
empty string if no suggestion is found."
  (save-excursion
    (goto-char (point-min))
    (let ((author-regexp
           "<meta\\s-+name=\"citation_authors\"\\s-+content=\"\\(\\sw+\\)")
          (date-regexp
           "<meta\\s-+name=\"citation_date\"\\s-+content=\"\\([0-9/-]+\\)"))
      (when (re-search-forward author-regexp nil t)
        (let ((author (match-string-no-properties 1)))
          (when (re-search-forward date-regexp nil t)
            (let ((date (match-string-no-properties 1)))
              (concat author (s-right 4 date)))))))))



;;; functions to display abstracts

(defun bibslurp/format-abs-meta ()
  "copy title, authors, and source from the header metadata."
  (goto-char (point-min))
  (when (re-search-forward
         "<meta\\s-+name=\"citation_title\"\\s-+content=\"\\(.*?\\)\""
         nil t)
    (let ((title (match-string 1)))
      (goto-char (point-min))
      (when (re-search-forward
             "<meta\\s-+name=\"citation_authors\"\\s-+content=\"\\(.*?\\)\""
             nil t)
        (let ((authors (match-string 1)))
          (goto-char (point-min))
          (when (re-search-forward
                 "<meta\\s-+name=\"dc\\.source\"\\s-+content=\"\\(.*?\\)\""
                 nil t)
            (let ((source (match-string 1)))
              (concat title "\n" authors "\n" source))))))))

(defun bibslurp/format-abs-text ()
  "return the abstract text"
  ;; abstracts are displayed as
  ;;   <h3 align="center">Abstract</h3>
  ;;   abstract text....
  ;;   <hr>
  ;; the <hr> isn't required by html, but it's there in ADS.  so I can
  ;; quasi-safely use it to mark the end of the abstract
  (when (re-search-forward
         "<h3[^>]+>\\s-*Abstract\\s-*</h3>\\(\\(.*\n?\\)+?\\)<hr>" nil t)
    (s-word-wrap 80 (match-string 1))))

(defun bibslurp/format-abs ()
  "take a buffer containing the HTML for an abstract page and
turn it into something human readable."
  (let ((meta (bibslurp/format-abs-meta))
        (abs (bibslurp/format-abs-text))
        (inhibit-read-only t))
    (when (and meta abs)
      (let ((buf (get-buffer-create "ADS Abstract")))
        (with-current-buffer buf
          (erase-buffer)
          (insert meta "\n\n\n" abs)
          (view-mode)
          (local-set-key (kbd "q") 'kill-buffer))
        (switch-to-buffer buf)))))

(defun bibslurp-show-abstract (&optional link-number)
  "Display the abstract page for a specified link number."
  (interactive)
  (setq link-number
	(or link-number
	    current-prefix-arg
	    (get-text-property (point) 'number)
	    (read-string "Link number: ")))
  (let* ((abs-url (bibslurp/follow-link link-number)))
    (when abs-url
      (with-temp-buffer
        (url-insert-file-contents (bibslurp/follow-link link-number))
        (bibslurp/format-abs)))))

;;; Navigation

(defun bibslurp-next-entry ()
  "Move to the next entry."
  (interactive)
  (let ((pos (next-single-property-change (point) 'number)))
    (if (integerp pos)
	(goto-char pos))))

(defun bibslurp-previous-entry ()
  "Move to the previous entry."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'number)))
    (if (integerp pos)
	(goto-char pos))))

(provide 'bibslurp)

;;; bibslurp.el ends here
