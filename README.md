# emms-mobile-remote
Control EMMS using your mobile

![screenshot](https://github.com/sabof/emms-mobile-remote/raw/master/screenshot.png)

This package requires elnode.

## Installation

- Create a folder
- Put emms-mobile-remote.el and front.html in it
- Add it to your load path
- Add to your .emacs:

```lisp
(require 'emms-mobile-remote)
(setq emr-pass "sesame")
```

- M-x emms-mobile-remote-start
- Open shell and run ifconfig, and take note of the "inet addr" shown under eth0
- In your mobile browser enter http://{the above value}/index/?pass={the value of emr-pass}
- Open EMMS
- In order to get a big a display, do M-: (set-face-attribute 'default (selected-frame) :height 300)

## Caveats

- By default "mute" simply sets the volume to 0 - you might want to bind it to some shell script instead.
