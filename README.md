# Dotfiles

Configuration for the programs I use regularly — Emacs, Fish shell, Starship,
Wezterm, and Jujutsu — managed as a single literate [Org](https://orgmode.org/)
file.

All configuration lives in [`dotfiles.org`](dotfiles.org). Running
`org-babel-tangle` extracts ("tangles") the source blocks into the actual config
files on disk. **Edit `dotfiles.org`, never the generated files.**

This repository uses [jj (Jujutsu)](https://github.com/jj-vcs/jj) for version
control (backed by a Git repo, so `git` also works).

## Prerequisites

- **macOS** with [Homebrew](https://brew.sh/) (the setup is macOS-oriented;
  several pieces assume Apple Silicon paths under `/opt/homebrew`).
- **Emacs** — installed as [`emacs-plus@30`](https://github.com/d12frosted/homebrew-emacs-plus)
  by `make bootstrap`. Emacs is required to tangle the config.

## Quick start (fresh machine)

```bash
make bootstrap
```

`bootstrap` will:

1. Tap and trust `d12frosted/emacs-plus` and install `emacs-plus@30`.
2. Tangle all config files from `dotfiles.org`.
3. Create the `/Applications/Emacs.app` shortcut (`emacs-app`).
4. Pre-build all Emacs packages so the first GUI start is fast (`precompile`).

You'll usually also want to install the CLI dev tools:

```bash
make setup-tools
```

## Make targets

| Target | Description |
| --- | --- |
| `make tangle` | Tangle all config files from `dotfiles.org`. Run this after every change. |
| `make bootstrap` | Fresh-machine setup: install Emacs (emacs-plus@30), tangle, create the app shortcut, and precompile. |
| `make precompile` | Native-compile all Emacs packages from the CLI. Re-run after an Emacs version upgrade so the first GUI start is fast. |
| `make emacs-app` | (Re)create the `/Applications/Emacs.app` Finder alias to the emacs-plus bundle. Re-run if the shortcut goes stale after a `brew reinstall`/upgrade. |
| `make setup-tools` | Install CLI dev tools (Rust toolchain, Go tools, Nerd Fonts, Homebrew packages from the Brewfile). |
| `make update-tools` | Update already-installed dev tools (`gup update`, `cargo install-update -a`). |

## Generated files

Tangling `dotfiles.org` writes, among others:

- `~/.emacs.d/init.el` — main Emacs configuration
- `~/.emacs.d/early-init.el` — early Emacs initialization
- `~/.emacs.d/local.el` — machine-specific settings (not committed)
- `~/.emacs.d/precompile.el` — CLI package pre-build script (used by `make precompile`)
- `~/.emacs.d/straight/versions/default.el` — pinned package versions
- `~/.config/fish/config.fish` — Fish shell configuration
- `~/.config/starship.toml` — Starship prompt configuration
- `~/.config/wezterm/wezterm.lua` — Wezterm terminal configuration
- `~/.config/jj/config.toml` — Jujutsu configuration
- `~/Brewfile` — Homebrew package manifest
- `~/bin/setup-dev-tools.fish` — dev tools installer (run by `make setup-tools`)
- `~/bin/emacs-app-shortcut.sh` — `/Applications/Emacs.app` alias helper (run by `make emacs-app`)
- `~/bin/emacs-resolver.sh` — Emacs-based merge conflict resolver for jj

## Making changes

All configuration changes go in `dotfiles.org`, **not** the generated files.
After editing, re-tangle:

```bash
make tangle
```

You can also tangle from within Emacs by opening `dotfiles.org` and running
`org-babel-tangle` (`C-c C-v t`).

## Notes

- **App shortcut on managed Macs:** `/Applications` carries the BSD `sunlnk`
  flag, so a plain `rm /Applications/Emacs.app` fails without `sudo`. `make
  emacs-app` drives the replace through Finder, which works without elevated
  privileges. If macOS still launches a stale copy after an upgrade, refresh
  Launch Services (see the *Installing Emacs.app* section in `dotfiles.org`).
- **Untrusted tap:** `d12frosted/emacs-plus` is a third-party tap. `make
  bootstrap` runs `brew trust` for it automatically; if installing manually you
  may need `brew trust --formula d12frosted/emacs-plus/emacs-plus@30` first.
- **Python LSP:** `basedpyright` is installed globally via Homebrew and used by
  eglot for all Python projects; the `pet` package supplies each project's
  virtualenv so imports resolve without manual activation.
