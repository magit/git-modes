Git Config Modes
================

Emacs major modes for various Git configuration files.

An list of contributors can be found [here][1].

`gitconfig-mode`
----------------

`gitconfig-mode` is automatically enabled for `.gitconfig`,
`.git/config` and `git/config` files.

Derives from `conf-unix-mode`, so commands provided by `conf-mode`
work here too.

`gitconfig-mode` was created by [Sebastian Wiesner][sw].

`gitignore-mode`
----------------

`gitignore-mode` is automatically enabled for `.gitignore`,
`.git/info/exclude` and `git/ignore` files.

Derives from `conf-unix-mode`, so commands provided by `conf-mode`
work here too.

`gitignore-mode` was created by [Sebastian Wiesner][sw].

`gitattributes-mode`
--------------------

`gitattributes-mode` is automatically enabled for `.gitattributes`,
`.git/info/attributes`, and `git/attributes`.

`gitattributes-mode` was created by [Rüdiger Sonderfeld][rs].


[1]:  https://github.com/magit/git-modes/graphs/contributors
[sw]: https://github.com/lunaryorn
[rs]: https://github.com/ruediger
