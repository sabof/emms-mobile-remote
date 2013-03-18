(require 'eldoc)
(require 'emms)
(require 'xmlgen)

(defvar ir-front-html
  (concat
   (file-name-directory
    (or load-file-name
        buffer-file-name))
   "front.html"))

(defun ir-eval-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon "eval"))

(defmacro ir-with-playlist-window (&rest body)
  `(let* (( emms-buffers (emms-playlist-buffer-list))
          ( all-windows (reduce 'append (mapcar 'window-list (frame-list))))
          ( win (find-if (lambda (win)
                           (memq (window-buffer win) emms-buffers))
                         all-windows)))
     (with-selected-window win
       (with-current-buffer (window-buffer win)
         ,@body
         ))))

(defun ir-emms-process-action (action)
  (case action
    ( pause
      (emms-pause))
    ( next
      (emms-queue-goto-next-track))
    ( prev
      (emms-previous))
    ( vol-up
      (shell-command "volume_up.sh"))
    ( vol-down
      (shell-command "volume_down.sh"))
    ( mute
      (shell-command "pa-vol.sh mute"))
    ( stop
      (emms-stop))
    ( seek-fwd
      (emms-seek-forward))
    ( seek-bkwd
      (emms-seek-backward))
    ( next-playlist
      (ir-with-playlist-window
       (let* (( emms-buffers
                (cl-sort (emms-playlist-buffer-list)
                         'string<
                         :key 'buffer-name
                         ))
              ( cur-buf-index
                (cl-position (current-buffer) emms-buffers))
              ( new-buf
                (nth
                 (mod (1+ cur-buf-index)
                      (length emms-buffers))
                 emms-buffers)))
         (set-window-buffer (selected-window) new-buf)
         (setq emms-playlist-buffer new-buf)
         )))))

(defun ir-index-handler (httpcon)
  (let (( action-param (elnode-http-param httpcon "action")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (when (stringp action-param)
      (ir-emms-process-action (intern action-param)))
    (elnode-http-return
     httpcon
     (with-temp-buffer
       (insert-file-contents ir-front-html)
       (buffer-string)))))

(defun ir-root-handler (httpcon)
  (let (( map '(("action" . ir-action-handler)
                ("index" . ir-index-handler))))
    (elnode-hostpath-dispatcher httpcon map)))

(provide 'iphone-remote)
;; iphone-remote.el ends here
