# AGENTS.md

Guidance for AI agents working in this repository.

## What this repo is

A **literate dotfiles** configuration. Nearly every config file (Emacs, Fish,
Starship, WezTerm, Jujutsu, etc.) is *generated* by tangling a single org file,
[`dotfiles.org`](dotfiles.org), with Emacs `org-babel-tangle`.

## Version control: jj, not git

**This repository is tracked with [Jujutsu (jj)](https://github.com/martinvonz/jj), not git.**

- The `.git` directory exists only as jj's backing store — do **not** drive it
  with `git commit`, `git checkout`, etc.
- Use `jj` commands for all version-control operations:
  - `jj status` — working-copy changes (don't use `git status`)
  - `jj diff` — view changes
  - `jj describe` — set the change description
  - `jj new` — start a new change
  - `jj log` — history
- The working copy is itself a commit in jj; there is no staging area and no
  "detached HEAD" concept. `git status` may report a detached HEAD — ignore it.

## Golden rule: edit `dotfiles.org`, never the generated files

All configuration changes go in `dotfiles.org`. The tangled output files
(`~/.emacs.d/init.el`, `~/.config/fish/config.fish`, `~/.config/starship.toml`,
`~/.config/wezterm/wezterm.lua`, and others) are **generated artifacts** — any
edit to them will be overwritten on the next tangle and is therefore wasted.

After editing `dotfiles.org`, regenerate the configs:

```bash
make tangle
```

(Equivalent to running `org-babel-tangle` / `C-c C-v t` from within Emacs.)

## Common tasks (Makefile)

- `make tangle` — regenerate all config files from `dotfiles.org`
- `make bootstrap` — fresh-machine setup: install Emacs via Homebrew, then tangle
- `make setup-tools` — run the dev-tools setup script
- `make install-ghostty` — (Linux only) install latest Ghostty AppImage + desktop launcher
- `make clean` — intentionally a no-op (no manifest of tangled files)

## Conventions

- Keep config and its prose documentation together in `dotfiles.org`; new
  settings should be documented alongside their code block, matching the
  surrounding style.
- Homebrew packages are defined in the Brewfile source block in `dotfiles.org`
  (tangled to `~/Brewfile`) — edit that block, not `~/Brewfile` directly.
- Pinned versions of externally-installed CLI tools (Go/Rust tools, Homebrew
  language runtimes) live in the committed `tool-versions` file (`KEY=VALUE`) at
  the repo root — *not* in `dotfiles.org`. The tangled Brewfile and dev-tools
  setup script read it at runtime (from `~/dotfiles/tool-versions`), so bumping a
  version needs no tangle: edit `tool-versions`, then `make setup-tools` (Go/Rust)
  or `brew bundle` (runtimes). It is hand-edited, unlike the generated files.
- `straight-versions.el` (Emacs package lockfile) is the one committed file that
  is **not** tangled from `dotfiles.org` and **not** hand-edited. It is written
  by `straight-freeze-versions` (`make freeze`) and consumed by
  `straight-thaw-versions` (`make thaw`, run during bootstrap). Re-pin by running
  `make freeze` and committing the result, never by editing it directly.
