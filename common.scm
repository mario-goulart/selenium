(use tcp)

(define (can-connect? host port)
  (handle-exceptions exn
    #f
    (receive (in out)
        (tcp-connect host port)
      (close-input-port in)
      (close-output-port out)
      #t)))

(define (wait-for-connection host port)
  (let loop ()
    (unless (can-connect? host port)
      (print "==== Waiting for the webdriver server...")
      (sleep 1)
      (loop))))
