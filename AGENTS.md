# AGENTS.md

## Repo at a glance
- Personal macOS dotfiles managed with GNU Stow.
- Main goal: reliable local developer environment with minimal surprise.
- Changes here can affect every shell/editor/git session, so prefer small, targeted edits.

## What lives where
- `shell/`
  - `.shellrc`: shared shell environment, aliases, PATH, helper functions.
  - `.zshrc`: zsh + oh-my-zsh setup; sources `.shellrc`.
  - `.bashrc`: bash setup; sources `.shellrc`.
  - `.shell_system.example`: machine-specific overlay example.
- `git/`
  - `.gitconfig`: core git defaults/aliases and include of `~/.gitconfig_system`.
  - `.gitignore_global`: global ignore patterns.
- `emacs/`
  - `.emacs.d/init.el`: main editor config entrypoint.
  - `.emacs.d/system.el.example`: machine-local example config.
- Root docs:
  - `README.md`: setup/bootstrap and stow usage.
  - `KINESIS_KEYBOARD.md`: keyboard firmware/layout workflow.

## Typical workflows
1. Edit one package at a time (`shell`, `git`, or `emacs`).
2. Run targeted validation commands.
3. Suggest explicit stow apply command for changed package(s).

Example apply commands:
- `stow --target=$HOME --restow shell`
- `stow --target=$HOME --restow git`
- `stow --target=$HOME --restow emacs`

## Validation playbook
- Shell syntax:
  - `bash -n shell/.bashrc`
  - `zsh -n shell/.zshrc`
- JSON syntax (when relevant):
  - `jq -e . <file>`
- For docs, do a quick readability pass.

## Guardrails
- Never add secrets, tokens, or private machine values.
- Prefer additive/compatible alias and env updates unless explicitly requested otherwise.
- Avoid broad rewrites; touch only files relevant to the task.
- If a change may alter startup behavior significantly, call it out clearly in handoff.

## Handoff checklist
- Files changed
- User-visible behavior impact
- Validation commands run
- Exact stow command(s) to apply
