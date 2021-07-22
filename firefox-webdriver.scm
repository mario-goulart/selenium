(include "common.scm")

(define (with-firefox-webdriver thunk
                                #!key (scheme 'http)
                                      (host "127.0.0.1")
                                      (port 4444)
                                      (path "/wd/hub")
                                      (capabilities
                                          '()))
  (parameterize
    ((command-executor-scheme scheme)
     (command-executor-host host)
     (command-executor-port port)
     (command-executor-path path)
     (desired-capabilities
      (alist-update 'browserName "firefox" capabilities)))

    ;; Wait until the webdriver starts accepting requests
    (wait-for-connection host port)

    (parameterize ((session-identifier (start-session)))
      (thunk))))
