gitconfig-mode
==============

A GNU Emacs major mode for editing `.gitconfig` files.

Features
--------

- Syntax highlighting
- Indentation
- Section movements with `C-M-a` and `C-M-b`

Installation
------------

[Download][] the latest release and install `gitconfig-mode.el` with `M-x
package-install-file`.

Or just drop `gitconfig-mode.el` somewhere into your `load-path`.

`gitconfig-mode` needs at least GNU Emacs 24.

Usage
-----

``.gitconfig`` and ``.git/config`` files will automatically use
`gitconfig-mode`.

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

[download]: https://github.com/lunaryorn/gitconfig-mode/tags
