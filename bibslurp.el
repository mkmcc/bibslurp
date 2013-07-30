;;; bibslurp.el --- retrieve BibTeX entries from NASA ADS

;; I think this basically works!

;; TODO:
;; 1. see how this responds to bad input.  test for errors, etc.
;; 2. submit to MELPA!
;; 3. long-term goal: replace lynx calls with internal emacs functions

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
    (define-key map "q" 'bibslurp/quit)
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

(defun bibslurp/quit ()
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

Return nil if not found."
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

(provide 'bibslurp)

;;; bibslurp.el ends here
