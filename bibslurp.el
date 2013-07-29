;;; bibslurp.el --- retrieve BibTeX entries from NASA ADS

;; I think this basically works!

;; TODO:
;; 1. see how this responds to bad input.  test for errors, etc.
;; 2. make space and S-space page down and up in bibslurp-mode
;; 3. make sure the autoloading is right...
;; 4. submit to MELPA!
;; 5. long-term goal: replace lynx calls with internal emacs functions

;;; start by making a rudimentary web browser
;;; -- use lynx for now, but eventually should replace this with
;;;    internal emacs functions

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
    map)
  "Keymap for bibslurp mode.")

(define-derived-mode bibslurp-mode fundamental-mode "bibslurp"
  "Major mode for perusing ADS search results and slurping bibtex
entries to the kill-ring.  This is pretty specific, so you should
only enter the mode via `bibslurp-query-ads'.  Requires the lynx
browser to the installed."
  (kill-all-local-variables)
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
  "Helper function which turns a search string (e.g. '^Quataert
2008') into an ads search url.  Used by `bibslurp-query-ads'."
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
      (bibslurp-mode)
      (window-configuration-to-register :bibslurp-window)
      (switch-to-buffer buf)
      (local-set-key (kbd "q")
                     (lambda () (interactive)
                       (kill-buffer)
                       (jump-to-register :bibslurp-window))))))

(defun bibslurp/absurl-to-biburl (abs-url)
  "Take the URL of an ADS abstract page and return a URL for the
corresponding bibtex entry.  Return nil if not found."
  (with-temp-buffer
    ; use lynx -dump to parse the html, find links, etc.
    (call-process "lynx" nil t nil "-dump" abs-url)
    (goto-char (point-min))
    ; look for, e.g. "[25]Bibtex entry for this abstract"
    (when (re-search-forward "\\[\\([0-9]+\\)\\]Bibtex" nil t)
      ; look for, e.g. " 25. http://..."
      (let ((bib-link-regexp
             (concat "^\\s-*" (match-string-no-properties 1)
                     "\\.\\s-*\\(.+\\)$")))
        (re-search-forward "^References$")
        (re-search-forward bib-link-regexp)
        (match-string-no-properties 1)))))

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

(defun bibslurp-slurp-bibtex (link-number &optional new-label)
  "Automatically find the bibtex entry for an abstract in the
NASA ADS database.

This function is rather specific -- it presumes you've used
`bibslurp-query-nasa-ads' to search ADS for an abstract.  Then, you
can call this function from the *Bibslurp* buffer.  It will prompt
for the number in front of the abstract you want, then will find
the bibtex entry and save it to the kill ring.

The functions `bibslurp/absurl-to-biburl' and `bibslurp/biburl-to-bib' are
more general."
  (interactive (list (or current-prefix-arg
                         (read-string "Link number: "))
                     (read-string "New label: ")))
  (let* ((abs-url (bibslurp/follow-link link-number))
         (bib-url (bibslurp/absurl-to-biburl abs-url)))
    (cond
     ((eq bib-url nil)
      (message "Couldn't find link to bibtex entry."))
     (t
      (kill-new (bibslurp/biburl-to-bib bib-url new-label))
      (message "Saved bibtex entry to kill-ring.")))))


(provide 'bibslurp)

;;; bibslurp.el ends here
