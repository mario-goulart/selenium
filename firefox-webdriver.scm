(include "common.scm")

(define (with-firefox-webdriver thunk
                                #!key (scheme 'http)
                                      (host "127.0.0.1")
                                      (port 4444)
                                      (path "/wd/hub")
                                      (command "firefox")
                                      (capabilities
                                          '((browserName . "firefox")
                                            (javascriptEnabled . #t)
                                            (platform . "ANY"))))
  (parameterize
    ((command-executor-scheme scheme)
     (command-executor-host host)
     (command-executor-port port)
     (command-executor-path path)
     (desired-capabilities capabilities))

    ;; Wait until the webdriver starts accepting requests
    (wait-for-connection host port)

    (parameterize ((session-identifier (start-session)))
      (thunk))))
