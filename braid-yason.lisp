;;;; braid-yason.lisp

(in-package #:braid-yason)

;;;

(defun string-to-keyword (string)
	"Converts STRING to a keyword."
	(intern (string-upcase string) :keyword))

(defun encode-json (object)
	"Encodes OBJECT as a JSON string."
	(with-output-to-string (stream)
		(yason:encode object stream)))

(defun parse-json (string)
	"Takes a STRING containing a JSON description returning a
corresponding lisp data structure."
	(yason:parse string
							 ;; Use options that favour readability.
							 :object-as :plist :object-key-fn #'string-to-keyword))

;;;

(defun encode-json-response (response)
	"Converts a response body to JSON and sets the content-type header
if not already present."
	(setf (braid:body response) (encode-json (braid:body response)))
	(unless (braid:header response :content-type)
		(setf (braid:header response :content-type) "application/json; charset=utf-8"))
	response)

(defun wrap-encode-json-response (handler)
	"Braid middleware that converts a response body to JSON and sets the
content-type header if not already present."
	(lambda (request)
		(json-response (funcall handler request))))

(defun parse-json-params (request)
	"Parses the request body (a JSON string) and adds a new
key :json-params with the correponding Lisp data structure."
	;; TODO
	(cons :json-params
						(cons (parse-json (braid:body request)) request)))

(defun wrap-parse-json-params (handler)
	"Braid middleware that parses the request body (a JSON string) and
adds a new key :json-params with the correponding Lisp data
structure."
	(lambda (request)
		(parse-json-params (funcall handler request))))

(defun parse-json-body (response)
	"Parses RESPONSE body as a JSON string replacing body with the
corresponding Lisp data structure."
	(setf (braid:body response)
				(parse-json (braid:body response)))
	response)
	
(defun wrap-parse-json-body (handler)
	"Braid middleware that parses a JSON string response body replacing
it with a corresponding Lisp data structure."
	(lambda (response)
		(parse-json-body (funcall handler request))))

;;; End
