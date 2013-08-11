;;; bibslurp.el --- retrieve BibTeX entries from NASA ADS

;; Copyright (C) 2013 Mike McCourt
;;
;; Authors: Mike McCourt <mkmcc@astro.berkeley.edu>
;; URL: https://github.com/mkmcc/bibslurp
;; Version: 0.0.1
;; Keywords: bibliography, nasa ads

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a function `bibslurp-query-ads', which reads a search
;; string from the minibuffer, sends the query to NASA ADS
;; (http://adswww.harvard.edu/), and displays the results in a new
;; buffer called "ADS Search Results".

;; The "ADS Search Results" buffer opens in `bibslurp-mode', which
;; provides a few handy functions.  Typing the number preceding an
;; abstract and hitting RET calls `bibslurp-slurp-bibtex', which
;; fetches the bibtex entry corresponding to the abstract and saves it
;; to the kill ring.  Or hit 'a' instead to pull up the abstract.
;; Typing 'q' quits bibslurp-mode and restores the previous window
;; configuration.

;; For more information and examples see the project website here:
;; http://astro.berkeley.edu/~mkmcc/software/bibslurp.html

;; Note that this functionality requires the lynx browser
;; (http://lynx.isc.org/) -- I make pretty heavy use of its system of
;; numbered links.  I do have a long-term goal of replacing the lynx
;; calls with internal emacs functions.

;;; Example usage:

;; add an entry to a bibtex buffer:
;;   M-x bibslurp-query-ads RET ^Quataert 2008
;;   7 RET
;;   q
;;   C-y

;;; Installation:

;; Use package.el. You'll need to add MELPA to your archives:

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Alternatively, you can just save this file and do the standard
;; (add-to-list 'load-path "/path/to/bibslurp.el")
;; (require 'bibslurp)

;;; TODO:
;; 1. long-term goal: replace lynx calls with internal emacs functions

;;; Code:

;;; utility functions

(defun bibslurp/s-right (len s)
  "Returns up to the LEN last chars of S.
copied from s.el (https://github.com/magnars/s.el)"
  (let ((l (length s)))
    (if (> l len)
        (substring s (- l len) l)
      s)))


;;; start by making a rudimentary web browser

(defvar bibslurp-link-regexp "\\[\\([0-9]+\\)\\]"
  "Regular expression that tells bibslurp what links look like.
The parenthesized subexpression is the unique
string (representing a base 10 number) denoting a link")

(defvar bibslurp-font-lock-keywords
  `((,bibslurp-link-regexp . font-lock-keyword-face))
  "Font lock for `bibslurp-mode'.
Only numbered links are fontified.")

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
    map)
  "Keymap for bibslurp mode.")

(define-derived-mode bibslurp-mode fundamental-mode "Bib Slurp"
  "Major mode for perusing ADS search results and slurping bibtex
entries to the kill-ring.  This is pretty specific, so you should
only enter the mode via `bibslurp-query-ads'.  Requires the lynx
browser to the installed.

\\<bibslurp-mode-map>"
  (use-local-map bibslurp-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(bibslurp-font-lock-keywords)))

(defun bibslurp/follow-link (number)
  "take a link number and return the corresponding url as a
string.  argument may be either an integer or a string.  returns
nil if the link number is invalid, throws an error if the current
buffer doesn't conform to the expected \"lynx --dump\" format."
  (interactive "P")
  (let* ((link-number
          (if (stringp number)
              number
            (number-to-string number)))
         (link-regex
          (concat "\\s-*"
                  link-number
                  "\\." "\\s-*"
                  "\\(http://.*\\)$")))
    (save-excursion
      (if (and (re-search-forward "^References$")
               (re-search-forward link-regex nil t))
          (match-string-no-properties 1)))))


;;; stuff specific to ads

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

;;;###autoload
(defun bibslurp-query-ads (&optional search-string)
  "Interactive function which asks for a search string and sends
the query to NASA ADS.  Displays results in a new buffer called
\"ADS Search Results\" and enters `bibslurp-mode'.  You can
retrieve a bibtex entry by typing the number in front of the
abstract link and hitting enter.  You can exit the mode at any
time by hitting 'q'."
  (interactive (list (read-string "Search string: ")))
  (let ((search-url (bibslurp/build-ads-url search-string))
        (buf (get-buffer-create "ADS Search Results")))
    (with-current-buffer buf
      (call-process "lynx" nil t nil "-dump" search-url)
      (goto-char (point-min))
      (bibslurp-mode))
    (window-configuration-to-register :bibslurp-window)
    (switch-to-buffer buf)
    (delete-other-windows)))

(defun bibslurp-quit ()
  "Close the bibslurp buffer and restore the previous window
configuration."
  (interactive)
  (when (eq major-mode 'bibslurp-mode)
   (kill-buffer)
   (when (get-register :bibslurp-window)
       (jump-to-register :bibslurp-window))))

(defun bibslurp/absurl-to-bibdata (abs-url)
  "Take the URL of an ADS abstract page and return data about the
corresponding bibtex entry.

This list has the form (bib-url new-label), where bib-url is the
bib-url of the ADS bibtex page and new-label is the suggested
label.

new-label may be nil if a bibtex url is found, but it can't
suggest a new label.  If the bibtex url is not found, this
function simply returns nil."
  (with-temp-buffer
    ; use lynx -dump to parse the html, find links, etc.
    (call-process "lynx" nil t nil "-dump" abs-url)
    (goto-char (point-min))
    ; look for, e.g. "[25]Bibtex entry for this abstract"
    (when (re-search-forward (concat bibslurp-link-regexp "Bibtex") nil t)
      ; look for, e.g. " 25. http://..."
      (let ((bib-link-regexp
             (concat "^\\s-*" (match-string-no-properties 1)
                     "\\.\\s-*\\(.+\\)$")))
        (re-search-forward "^References$")
        (re-search-forward bib-link-regexp)
        (let ((bib-url (match-string-no-properties 1))
              (new-label (bibslurp/suggest-label)))
          (list bib-url new-label))))))

(defun bibslurp/biburl-to-bib (bib-url &optional new-label)
  "Take the URL for an ADS bibtex entry and return the entry as a
string.  Optionally, replace the default (and useless) ADS label
with the argument NEW-LABEL."
  (with-temp-buffer
    ; lynx -source doesn't process the text at all
    (call-process "lynx" nil t nil "-source" bib-url)
    ; first, look for a bibtex definition and replace the label if
    ; appropriate.
    (goto-char (point-min))
    (when (re-search-forward "@\\sw+{\\([^,]+\\)," nil t)
      (when (and new-label (not (string-equal new-label "")))
        (replace-match new-label t t nil 1))
      ; next, find the definition and return it.  use the nifty
      ; function `forward-sexp' to navigate to the end.
      (goto-char (point-min))
      (re-search-forward "@\\sw+")
      (let ((bpoint (point)))
        (forward-sexp)
        (concat (match-string-no-properties 0)
                (buffer-substring bpoint (point)))))))

(defun bibslurp-slurp-bibtex (link-number)
  "Automatically find the bibtex entry for an abstract in the
NASA ADS database.

This function is rather specific -- it presumes you've used
`bibslurp-query-nasa-ads' to search ADS for an abstract.  Then, you
can call this function from the *Bibslurp* buffer.  It will prompt
for the number in front of the abstract you want, then will find
the bibtex entry and save it to the kill ring.

The functions `bibslurp/absurl-to-bibdata' and `bibslurp/biburl-to-bib' are
more general."
  (interactive (list (or current-prefix-arg
                         (read-string "Link number: "))))
  (let* ((abs-url (bibslurp/follow-link link-number))
         (bib-data (bibslurp/absurl-to-bibdata abs-url)))
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
  (let ((author
         (save-excursion
           (goto-char (point-min))
           (if (re-search-forward "Authors:\\s-*\\[\\sw+\\]\\(\\sw+\\)[,;]" nil t)
               (match-string-no-properties 1))))
        (date
         (save-excursion
           (goto-char (point-min))
           (if (re-search-forward "Date:\\s-*\\([0-9/]+\\)" nil t)
               (match-string-no-properties 1)))))
    (concat author (bibslurp/s-right 4 date))))



;;; function to display abstracts

(defun bibslurp-show-abstract (link-number)
  "Display the abstract page for a specified link number."
  (interactive (list (or current-prefix-arg
                         (read-string "Link number: "))))
  (let* ((abs-url (bibslurp/follow-link link-number))
         (inhibit-read-only t)
         (buf (get-buffer-create "ADS Abstract")))
    (if (eq abs-url nil)
        (progn
          (message "Couldn't find abstract %s." link-number)
          (kill-buffer buf))
      (with-current-buffer buf
        (erase-buffer)
        (call-process "lynx" nil t nil "-dump" abs-url)
        (goto-char (point-min))
        (re-search-forward "Abstract$" nil t)
        (view-mode)
        (local-set-key (kbd "q") 'kill-buffer))
      (switch-to-buffer buf))))

(provide 'bibslurp)

;;; bibslurp.el ends here
