gitconfig-mode
==============

A GNU Emacs major mode for editing `.gitconfig` files.

Features
--------

- Syntax highlighting
- Indentation

Installation
------------

Install the ELPA package from [MELPA][] or [Marmalade][] with `M-x
package-install gitconfig-mode`, or [download][] the latest release and install
`gitconfig-mode.el` with `M-x package-install-file`.

`gitconfig-mode` needs at least GNU Emacs 24.

Usage
-----

`gitconfig-mode` is automatically enabled for ``.gitconfig`` and ``.git/config``
files.  The mode is derived from `conf-unix-mode`, and all commands provided by
`conf-mode` will work as expected.

Customization
-------------

`M-x customize-group gitconfig`

Further help
------------

`C-h f gitconfig-mode`

License
-------

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.

See `COPYING` for details.

[melpa]: http://melpa.milkbox.net/
[marmalade]: http://marmalade-repo.org/packages/gitconfig-mode/
[download]: https://github.com/lunaryorn/gitconfig-mode/tags
