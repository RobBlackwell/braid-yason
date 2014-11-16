(ql:quickload :braid-yason) 

(defun my-handler (request)
	""
	(braid:make-response :body '( 1 2 3)))

;;(funcall (braid-yason:wrap-json-response #'my-handler) t)




