(use tcp)

(define (can-connect? host port)
  (handle-exceptions exn
    #f
    (receive (in out)
        (tcp-connect host port)
      (close-input-port in)
      (close-output-port out)
      #t)))
