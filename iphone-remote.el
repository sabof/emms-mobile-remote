(require 'eldoc)
(require 'xmlgen)

(defun my-test-handler (httpcon)
  "Demonstration function"
  (let (( met (elnode-http-method httpcon))
        ( params (elnode-http-params httpcon))
        ( action-param (elnode-http-param httpcon "action")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (elnode-http-return httpcon (xmlgen `(html (ul (li ,(format "%s" params))
                                                   (li ,action-param))))
                        )))
(provide 'iphone-remote)
;; iphone-remote.el ends here
