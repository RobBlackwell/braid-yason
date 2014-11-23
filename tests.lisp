(ql:quickload :braid-yason) 

(defun my-request-handler (request)
	""
	(braid:make-response :body '(1 2 3)))

(defun my-response-handler (response)
	""
	(braid:make-response :body "[1,2,3]"))

(defun test1 ()
	(let ((response
				 (funcall 
					(braid-yason:wrap-encode-json-body 'my-request-handler) nil)))
		(string= (braid:body response) "[1,2,3]")))

(defun test2 ()
	(let ((request (funcall (braid-yason:wrap-parse-json-body 'my-response-handler) nil)))
		(equal (braid:body request) '(1 2 3))))

;;(funcall (braid-yason:wrap-json-response #'my-handler) t)

;; (funcall (braid-yason:wrap-json-params (braid-yason:wrap-json-response #'my-handler)) t)




