# Theme Previews

Create preview files for installed themes.

## Installation

Put `theme-previews.el` somewhere in your `load-path` and `(require 'theme-previews)`.

You also need to install htmlize 1.47 which can be found in [MELPA](http://melpa.milkbox.net/) or [here](http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi).

## Status

Currently very *beta* and stuff will change. I want to make the output directory configurable among other things.

Use with caution.

## Usage

Load a file you want to use for the previews (i.e a code snippet) and run
`M-x theme-previews`. It will then go through all your installed themes and create
a file called \<theme\>-\<buffer-name\>.html in your (at the moment) current directory.

