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

### Notes

#### MacOS Configuration

- Invert trackpad scroll
- Keyboard configuration:
  - key repeat rate: fast
  - delay until repeat: short
  - turn off all smart spelling correction, capitalization, periods, etc
  - option -> command
  - command -> option
  - caps lock -> control
  - Use fn keys (F1 etc)
  - emacs hotkeys
    ~/Library/KeyBindings/DefaultKeyBinding.dict
    ```
    {
      "~d" = "deleteWordForward:";
      "~f" = "moveWordForward:";
      "~b" = "moveWordBackward:";
    }
    ```
- Dock
  - Remove app icons
  - Turn hiding on
- Set computer name
- Software updates

#### Softwares

Browser
- Enable sync for bookmarks, settings

Password manager
- browser extension

Homebrew; brew install --cask
- iterm2
  - import settings from another computer... auto-loading from com.googlecode.iterm2.plist gives me trouble
    - color scheme (Solarized Dark)
    - pane splitting
- signal
- gitx

##### Emacs build from source

```bash
$ git clone https://git.savannah.gnu.org/git/emacs.git
$ cd emacs
$ git checkout emacs-30.1

$ ./autogen.sh
$ ./configure --with-native-compilation --with-tree-sitter --with-xml2 --with-modules --with-gnutls --with-cairo --prefix=/usr/local
$ make -j$(sysctl -n hw.ncpu)

# Test:
$ src/emacs

$ sudo make install
$ cp nextstep/Emacs.app /Applications/Emacs.app
$ cp nextstep/Emacs.app/Contents/MacOS/Emacs /usr/local/bin/emacs
```

#### Tooling

- [oh-my-zsh](https://github.com/ohmyzsh/ohmyzsh)
  - install iTerm2 shell integration
- [nvm](https://github.com/nvm-sh/nvm)
- brew install:
  - awscli
  - certbot
  - coreutils
  - exiftool
  - ffmpeg
  - gh
  - git
  - jq
  - nvm
  - poetry
  - pyenv
  - terraform
  - the_silver_searcher
  - tree
  - watch
  - wget
  - yarn
- git config
  - set up user.email and user.name:
    git config --system user.email {email}
    git config --system user.name "{First} {Last}"

#### Auth

- [Generate an SSH key](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent#generating-a-new-ssh-key)
  - add public key to GitHub
