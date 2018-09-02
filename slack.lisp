(in-package :slack)

(defvar *default-name* "shout!bot")
(defvar *default-icon* "http://cl.ly/image/3e1h0H3H2s0P/concourse-logo.png")

(defun env (name default)
  (or (sb-unix::posix-getenv name) default))

(defun attach (text &key title color)
  (remove-if #'null
    (list
      (if (null title) nil `(title . ,title))
      (if (null color) nil `(color . ,color))
      `(text . ,text))))

(defun send (text &key (icon     (env "SHOUT_BOTICON" *default-icon*))
                       (username (env "SHOUT_BOTNAME" *default-name*))
                       (webhook  (env "SHOUT_WEBHOOK" ""))
                       (attachments nil))
  (if (equal webhook "")
    (error "no webhook supplied to slack:send!"))
  (drakma:http-request webhook
                       :method :post
                       :content (json:encode-json-to-string
                                  `((text . ,text)
                                    (username . ,username)
                                    (icon_url . ,icon)
                                    (attachments . ,attachments)))))

