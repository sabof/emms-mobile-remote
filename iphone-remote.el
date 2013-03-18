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

(defun ir-focus ()
  (ir-with-playlist-window
   (emms-playlist-mode-center-current)
   (hl-line-highlight)))

(defun ir-emms-process-action (action)
  (case action
    ( pause
      (emms-pause))
    ( next
      (if emms-player-playing-p
          (if (fboundp 'emms-queue-goto-next-track)
              (emms-queue-goto-next-track)
              (emms-next))
          (emms-playlist-current-select-next))
      (ir-focus))
    ( prev
      (if emms-player-playing-p
          (emms-previous)
          (emms-playlist-current-select-previous))
      (ir-focus))
    ( vol-up
      (es-silence-messages
       (shell-command "volume_up.sh")))
    ( vol-down
      (es-silence-messages
       (shell-command "volume_down.sh")))
    ( mute
      (es-silence-messages
       (shell-command "pa-vol.sh mute")))
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
         (ir-focus)))))
  (when (executable-find "xdotool")
    (es-silence-messages
     (shell-command "xdotool key Alt"))))

(defun ir-index-handler (httpcon)
  (let (( action-param (elnode-http-param httpcon "action")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (when (stringp action-param)
      (condition-case error
          (ir-emms-process-action (intern action-param))
        (error (message "%s" error))))
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
