(require 'elnode)
(require 'emms)
(require 'xmlgen)

(defvar emr-front-html
  (concat
   (file-name-directory
    (or load-file-name
        buffer-file-name))
   "front.html"))

(defmacro emr-with-playlist-window (&rest body)
  `(let* (( emms-buffers (emms-playlist-buffer-list))
          ( all-windows (reduce 'append (mapcar 'window-list (frame-list))))
          ( win (find-if (lambda (win)
                           (memq (window-buffer win) emms-buffers))
                         all-windows)))
     (with-selected-window win
       (with-current-buffer (window-buffer win)
         ,@body
         ))))

(defun emr-focus ()
  (emr-with-playlist-window
   (emms-playlist-mode-center-current)
   (hl-line-highlight)))

(defun emr-switch-playlist (offset)
  (emr-with-playlist-window
   (let* (( emms-buffers
            (cl-sort (emms-playlist-buffer-list)
                     'string<
                     :key 'buffer-name))
          ( cur-buf-index
            (cl-position (current-buffer) emms-buffers))
          ( new-buf
            (nth (mod (+ cur-buf-index
                         offset
                         (length emms-buffers))
                      (length emms-buffers))
                 emms-buffers)))
     (set-window-buffer (selected-window) new-buf)
     (setq emms-playlist-buffer new-buf)
     (emr-focus))))

(defun emr-emms-process-action (action)
  (case action
    ( pause
      (emms-pause))
    ( next
      (if emms-player-playing-p
          (if (fboundp 'emms-queue-goto-next-track)
              (emms-queue-goto-next-track)
              (emms-next))
          (emms-playlist-current-select-next))
      (emr-focus))
    ( prev
      (if emms-player-playing-p
          (emms-previous)
          (emms-playlist-current-select-previous))
      (emr-focus))
    ( vol-up
      (if (executable-find "volume_up.sh")
          (es-silence-messages
           (shell-command "volume_up.sh"))
          (emms-volume-raise)))
    ( vol-down
      (if (executable-find "volume_down.sh")
          (es-silence-messages
           (shell-command "volume_down.sh"))
          (emms-volume-lower)))
    ( mute
      (when (executable-find "pa-vol.sh")
        (es-silence-messages
         (shell-command "pa-vol.sh mute"))))
    ( stop
      (emms-stop))
    ( seek-fwd
      (emms-seek-forward))
    ( seek-bkwd
      (emms-seek-backward))
    ( next-playlist
      (emr-switch-playlist 1))
    ( prev-playlist
      (emr-switch-playlist -1)))
  (when (executable-find "xdotool")
    (es-silence-messages
     (shell-command "xdotool key Alt"))))

(defun emr-index-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return
   httpcon
   (with-temp-buffer
     (insert-file-contents emr-front-html)
     (buffer-string))))

(defun emr-action-handler (httpcon)
  (let (( action-param (elnode-http-param httpcon "a")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (when (stringp action-param)
      (condition-case error
          (emr-emms-process-action (intern action-param))
        (error (message "%s: %s: %s" action-param (car error) (cdr error)))))
    (elnode-http-return httpcon)))

(defun emr-root-handler (httpcon)
  (let (( map '(("action" . emr-action-handler)
                ("index" . emr-index-handler))))
    (elnode-hostpath-dispatcher httpcon map)))

(defun emms-mobile-remote-start ()
  (interactive)
  (let* (( host (shell-command-to-string "ip addr show eth0"))
         ( host (progn (string-match "inet \\(.+?\\)/" host)
                       (match-string 1 host))))
    (elnode-start 'emr-root-handler
                  :port 8000
                  :host host)))

(provide 'emms-mobile-remote)
;; emms-mobile-remote.el ends here