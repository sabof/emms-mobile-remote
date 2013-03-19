# emms-mobile-remote
Control emms using your mobile

![screenshot](https://github.com/sabof/emms-mobile-remote/raw/master/screenshot.png)

This package is requires elnode.

## Installation

- create a folder,
- put emms-mobile-remote.el and front.html to it
- add it to your load path
- Add to your .emacs

(require 'emms-mobile-remote)
(setq emr-pass "sesame")

- do M-x emms-mobile-remote-start
- open shell and run ifconfig, and take not of the "inet addr" shown under eth0
- in your mobile browser enter http://{the above value}/index/?pass={the value of emr-pass}
- open emms
- in order to get a big a display, do M-: (set-face-attribute 'default (selected-frame) :height 300)

## Caveats

- By default "mute" simply sets the volume to 0 - you might want to bind it to some shell script instead.
