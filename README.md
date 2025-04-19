## DanSchoppe dotfiles

Source-controlled system configuration files

### Usage

This repo relies on [GNU Stow](https://www.gnu.org/software/stow/) to
symlink dotfiles from this repository to the home directory.

```bash
$ git clone https://github.com/DanSchoppe/dotfiles.git
$ cd dotfiles
$ stow */
```

Alternatively, stow packages one-at-a-time, like:

```bash
$ stow emacs
```
