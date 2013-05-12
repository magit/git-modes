Git modes
=========

GNU Emacs modes for Git-related files.


Features
--------

- `git-commit-mode` – A GNU Emacs major mode for editing Git commit messages
  according to the [guidelines by Tim Pope][1].  Integrates into [Magit][2].
- `gitconfig-mode` – A `conf-mode`-derived major mode for editing
  `.gitconfig` files.
- `gitignore-mode` – A `conf-mode`-derived major mode for editing `.gitignore`
  files.


Installation
------------

Install the ELPA packages from [MELPA][3] (bleeding edge snapshots) or
[Marmalade][4] (stable releases):

- `git-commit-mode`: `M-x package-install git-commit-mode`
- `gitconfig-mode`: `M-x package-install gitconfig-mode`
- `gitignore-mode`: `M-x package-install gitignore-mode`

Or [download][5] the latest release and install the desired modes with `M-x
package-install-file`, e.g. `M-x package-install-file git-commit-mode`.

The modes are written and tested against GNU Emacs 24 and may or may not work in
earlier versions of GNU Emacs.


Usage
-----


### `git-commit-mode`

Just configure `emacs` or `emacsclient` as editor for Git. `git-commit-mode`
will automatically be enabled for Git message buffers.

`git-commit-mode` integrates into with [Magit][2] by redefining
`magit-log-edit-mode` to support all `git-commit-mode` features.

### `gitconfig-mode`

`gitconfig-mode` is automatically enabled for `.gitconfig` and `.git/config`
files.  The mode is derived from `conf-unix-mode`, so all commands provided by
`conf-mode` (e.g. `conf-align-assignments`) will work as expected.

### `gitignore-mode`

`gitignore-mode` is automatically enabled for `.gitignore` and
`.git/info/exclude` files.


Customization
-------------

- `git-commit-mode`: `M-x customize-group git-commit`
- `gitconfig-mode`: No customization provided.
- `gitignore-mode`: No customization provided.


Further help
------------

- `C-h f git-commit-mode`
- `C-h f gitconfig-mode`
- `C-h f gitignore-mode`


Credits
-------

`git-commit-mode` is forked of the [original work][6] done by
[Florian Ragwitz][7] and improved by [John Wiegley][8].

The following people contributed to these modes:

- [Bradley Wright](https://github.com/bradleywright)
- [Peter Eisentraut](https://github.com/petere)
- [Ramkumar Ramachandra](https://github.com/artagnon)
- [Tim Wraight](https://github.com/timwraight)

Great thanks also goes to [Bozhidar Batsov](https://github.com/bbatsov) for
adding these modes to his awesome [Prelude](https://github.com/bbatsov/prelude)
project, thus making them available to a larger user base.


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

See [`COPYING`][9] for details.


[1]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[2]: http://magit.github.com/magit/
[3]: http://melpa.milkbox.net
[4]: http://marmalade-repo.org/
[5]: https://github.com/lunaryorn/git-modes/tags
[6]: https://github.com/rafl/git-commit-mode
[7]: https://github.com/rafl
[8]: https://github.com/jwiegley
[9]: https://github.com/lunaryorn/git-modes/blob/master/COPYING
