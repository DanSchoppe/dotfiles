## DanSchoppe dotfiles

Source-controlled system configuration files

### Usage

This repo relies on [GNU Stow](https://www.gnu.org/software/stow/) to
symlink dotfiles from this repository to the home directory.

```bash
$ git clone https://github.com/DanSchoppe/dotfiles.git
$ cd dotfiles
$ stow --target=$HOME */
```

Alternatively, stow packages one-at-a-time, like:

```bash
$ stow --target=$HOME emacs
```

After adding, moving, or deleting files, re-run stow by:

```bash
$ stow --target=$HOME --restow */
```
