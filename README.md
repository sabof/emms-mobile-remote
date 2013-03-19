# emms-mobile-remote
Control emms using your mobile

![screenshot](https://github.com/sabof/emms-mobile-remote/raw/master/screenshot.png)

This package is requires elnode.

## Installation

- create a folder,
- put emms-mobile-remote.el and front.html to it
- add it to your load path
- (require 'emms-mobile-remote)
- do M-x emms-mobile-remote-start
- open shell and run ifconfig
- enter the "inet addr" shown under eth0, with port 8000 on your mobile browser
- open emms
- in order to get a big a display, do M-: (set-face-attribute 'default (selected-frame) :height 300)

## Caveats

- By default "mute" simply sets the volume to 0 - you might want to bind it to some shell script instead.
