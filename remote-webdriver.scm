(include "common.scm")

(define (run-remote command jar-file)
  (let ((profile-path (make-pathname (current-directory) "profile")))
    (open-input-pipe (sprintf "~A ~A" command jar-file))))

(define (with-remote-webdriver jar-file thunk
                               #!key (scheme 'http)
                                     (host "127.0.0.1")
                                     (port 4444)
                                     (path "/wd/hub")
                                     (command "java -jar")
                                     (capabilities '()))
  (parameterize
    ((command-executor-scheme scheme)
     (command-executor-host host)
     (command-executor-port port)
     (command-executor-path path)
     (desired-capabilities capabilities))
    (run-remote command jar-file)

    ;; Wait until the webdriver starts accepting requests
    (wait-for-connection host port)

    (parameterize ((session-identifier (start-session)))
      (thunk))))
