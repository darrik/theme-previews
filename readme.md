# Theme Previews

Create preview files for installed themes.

## Installation

Put `theme-previews.el` somewhere in your `load-path` and `(require
'theme-previews)`.

## Status

Currently **beta** and needs more testing.

## Usage

Load a file you want to use for the previews (i.e., a code snippet) and run
`M-x theme-previews`. It will then go through all your installed themes and
create a file called \<theme\>-\<buffer-name\>.html in the directory specified
in `M-x customize-group RET theme-previews` (defaults to current directory.)

## Notes

Creating previews can take a very long time depending on how many themes you
have installed.

You can cancel at any time with `CTRL-g`.

## License

Copyright (C) 2013 Rikard Glans

Author: Rikard Glans <rikard@ecx.se>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

