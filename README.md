# Plant
A simple daemonless wayland notification utility with support for font gamma correction.

![Plant](https://github.com/gitRaiku/plant/blob/master/Plant.gif?raw=true)

# Configuration
Like all suckless software all configuration is done by editing the `src/config.h` file

# Installation
Just a simple
```
sudo make install
```
should build it and copy it to `/usr/local/bin`

This package is also available as an AUR package `https://aur.archlinux.org/packages/wl-plant`.

# Usage
```
Usage: plant [--debug] [--] <first line to display> [line2] [line3] ...
    --debug: Enables debug logging (mostly font rendering information)
    --: Enables literal parsing of every argument after it
```
To display the notification

# Improvements
Need make it die after cliking on it

# Inspirations
This project was inspired by [herbe](https://freetype.org/freetype2/docs/tutorial/step2.html) which was made by a based guy. This is supposed to be the same thing but for Wayland.
