# Dotfiles

Configuration for a number of programs that I use very regularly like Emacs, Fish shell, Starship and Wezterm.

The config files themselves are all generated from [dotfiles.org](dotfiles.org) using Emacs. This repository uses jj (Jujutsu) for version control.

## Prerequisites

- Emacs (for generating config files from the org file)
- The programs you want to configure (Emacs, Fish, Starship, Wezterm, etc.)

## Setup

1. Clone this repository to your desired location
2. Generate the configuration files from `dotfiles.org`:

### From Emacs
Open `dotfiles.org` in Emacs and run `org-babel-tangle` (typically `C-c C-v t`)

### From Command Line
```bash
emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "~/dotfiles/dotfiles.org")'
```

## Generated Files

The org file generates configuration files to various locations:
- `~/.emacs.d/init.el` - Main Emacs configuration
- `~/.emacs.d/early-init.el` - Early Emacs initialization
- `~/.emacs.d/local.el` - Local machine-specific settings
- `~/.config/fish/config.fish` - Fish shell configuration
- `~/.config/starship.toml` - Starship prompt configuration
- `~/.config/wezterm/wezterm.lua` - Wezterm terminal configuration
- Various other config files as specified in the org file

## Making Changes

**Important**: All configuration changes should be made in `dotfiles.org`, not in the generated files directly. After making changes in the org file, re-run the tangle process to update the actual config files.

## Version Control

This repository uses [jj (Jujutsu)](https://github.com/martinvonz/jj) for version control instead of Git.
