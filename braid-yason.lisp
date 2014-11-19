;;;; braid-yason.lisp

(in-package #:braid-yason)

;;; Unfortunately we see UTF-8 byte order marks being used, despite
;;; the spec explicitly not recommending it. See
;;; http://www.unicode.org/versions/Unicode5.0.0/ch02.pdf

(define-constant +utf8-bom+ (vector #xEF #xBB #xBF)
	:documentation "Byte Order Mark for UTF-8")

(defun my-utf8-bytes-to-string (bytes)
  "Convert a byte array to a UTF-8 string, skipping the byte order
mark if necessary"
  (if (equalp (subseq bytes 0 3) +utf8-bom+)
      (babel:octets-to-string bytes :start 3 :encoding :utf-8)
      (babel:octets-to-string bytes :encoding :utf-8)))

;;;

(defun convert-string-to-keyword (string)
	"Converts STRING to a keyword."
	(intern (string-upcase string) :keyword))

(defun encode-json (object)
	"Encodes OBJECT as a JSON string."
	(with-output-to-string (stream)
		(yason:encode object stream)))

(defun decode-json (object)
	"Takes a string or byte array containing a JSON description returning a
corresponding lisp data structure."
	(let ((string (typecase object
									(string object)
									((simple-array (unsigned-byte 8)) (my-utf8-bytes-to-string object)))))
		(yason:parse string
								 ;; Use options that favour readability.
								 :object-as :plist :object-key-fn #'convert-string-to-keyword)))

;;;

(defun wrap-json-response (handler)
	"Braid middleware that converts a response body to JSON and sets the
content-type header if not already present."
	(lambda (request)
		(let* ((response (funcall handler request))
					 (json (encode-json (braid:body response))))
			(setf (braid:body response) json)
			(unless (braid:header response :content-type)
				(setf (braid:header response :content-type) "application/json; charset=utf-8"))
			response)))


(defun wrap-json-params (handler)
	"Braid middleware that parses a JSON request body and adds the
resulting data structure into the request with key :json-params"
	;; TODO - Needs to pull out a proper parms list
	(lambda (request)
		(let ((response (funcall handler request)))
			(cons :json-params
						(cons (decode-json (braid:body response)) response)))))


(defun wrap-json-body (handler)
	"Braid middleware that parses a JSON request body replacing it with
a corresponding Lisp data structure."
	(lambda (request)
		(let ((response (funcall handler request)))
			(setf (braid:body response)
						(decode-json (braid:body response)))
			response)))






