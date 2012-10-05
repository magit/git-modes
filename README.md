git-commit-mode
===============

A GNU Emacs major mode for editing Git commit messages according to the
[guidelines for commit messages][guidelines].

Features
--------

- Syntax highlighting for commit messages
- Correct filling
- Insertion of common headers like `Signed-of-by`
- Optional [Magit][] integration

Installation
------------

Install the bleeding-edge package from [MELPA][] with `M-x package-install
git-config-mode`` or [download][] the latest release and install
`git-commit-mode.el` with `M-x package-install-file`.

Or just drop `git-commit-mode.el` somewhere into your `load-path`.

Usage
-----

Just configure `emacs` or `emacsclient` as editor for Git. `git-commit-mode`
will automatically be enabled for Git message buffers.

Customization
-------------

`M-x customize-group git-commit`

Further help
------------

`C-h f git-commit-mode`

Credits
-------

This project is forked of the
[original work](https://github.com/rafl/git-commit-mode) by
[Florian Ragwitz](https://github.com/rafl).  Thanks for his work.

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

[guidelines]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[melpa]: http://melpa.milkbox.net/
[download]: https://github.com/lunaryorn/git-commit-mode/tags
[magit]: http://magit.github.com/magit/
