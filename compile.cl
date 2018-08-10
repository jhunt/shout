(sb-ext:save-lisp-and-die
  "app"
  :compression t
  :executable  t
  :toplevel (lambda ()
              (format t " ######  ##     ##  #######  ##     ## ######## #### ~%")
              (format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
              (format t "##       ##     ## ##     ## ##     ##    ##    #### ~%")
              (format t " ######  ######### ##     ## ##     ##    ##     ##  ~%")
              (format t "      ## ##     ## ##     ## ##     ##    ##         ~%")
              (format t "##    ## ##     ## ##     ## ##     ##    ##    #### ~%")
              (format t " ######  ##     ##  #######   #######     ##    #### ~%")
              (format t "starting up...~%")
              (api:run :port 8199)
              (loop (sleep 5))))
