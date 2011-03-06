;; In Emacs 19, the regexps in compilation-error-regexp-alist do not
;; match the error messages when the language is not English.
;; Hence we add a regexp.
(require 'compile)

(defconst html-error-regexp
  "^[A-\377]+ \"\\([^\"\n]+\\)\", [A-\377]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by htlex.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc html-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list html-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info

(defconst html-error-chars-regexp
  ".*, .*, [A-\377]+ \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by htlex.")

;; Wrapper around next-error.

(defvar html-error-overlay nil)

;;itz 04-21-96 somebody didn't get the documetation for next-error
;;right. When the optional argument is a number n, it should move
;;forward n errors, not reparse.

;itz 04-21-96 instead of defining a new function, use defadvice
;that way we get our effect even when we do \C-x` in compilation buffer  

(defadvice next-error (after html-next-error activate)
 "Reads the extra positional information provided by Htlex.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."

 (if (eq major-mode 'html-mode)
     (let ((beg nil) (end nil))
       (save-excursion
	 (set-buffer
	  (if (boundp 'compilation-last-buffer) 
	      compilation-last-buffer	;Emacs 19
	    "*compilation*"))		;Emacs 18
	 (save-excursion
	   (goto-char (window-point (get-buffer-window (current-buffer))))
	   (if (looking-at html-error-chars-regexp)
	       (setq beg
		     (string-to-int
		      (buffer-substring (match-beginning 1) (match-end 1)))
		     end
		     (string-to-int
		      (buffer-substring (match-beginning 2) (match-end 2)))))))
       (cond (beg
	      (setq beg (+ (point) beg)
		    end (+ (point) end))
	      (goto-char beg)
	      (push-mark end t)
	      (cond ((fboundp 'make-overlay)
		     (if html-error-overlay ()
		       (setq html-error-overlay (make-overlay 1 1))
		       (overlay-put html-error-overlay 'face 'region))
		     (unwind-protect
			 (progn
			   (move-overlay html-error-overlay
					 beg end (current-buffer))
			   (sit-for 60))
		       (delete-overlay html-error-overlay)))))))))

