(defcustom wikidata-wbsearchentities-languages-codes
  '(("English" . "en"))
  "Languages to consider when prompting for a language")

(defcustom wikidata-wbsearchentities-custom-params
  '(("language" . "en")
    ("uselang" . "en")
    ("limit" . "5"))
  "Custom parameters that are passed to the API endpoint
wbsearchentities.")

(defun wikidata-wbsearchentities-change-language ()
  "Change the language used for \"uselang\" and \"language\" in
wbsearchentities"
  (let* ((candidates
          (mapcar (lambda (x)
                    (propertize (car x) 'code (cdr x)))
                  wikidata-languages-codes))
         (candidate (completing-read "Language: " candidates nil :require-match))
         (code (get-text-property 0 'code candidate)))
    (setf (alist-get "language" wikidata-wbsearchentities-custom-params nil nil 'equal) code
          (alist-get "uselang" wikidata-wbsearchentities-custom-params nil nil 'equal) code)))

(provide 'wikidata-wbsearchentities)
