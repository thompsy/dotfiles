.PHONY: tangle clean install bootstrap setup-tools update-tools emacs-app precompile install-ghostty

# Run each recipe in a single bash shell so multi-line scripts share state
SHELL := /bin/bash
.ONESHELL:

# Emacs is installed via the d12frosted/emacs-plus tap.
EMACS_PLUS_TAP = d12frosted/emacs-plus
EMACS_PLUS = emacs-plus@30

# Tangle all config files from the org file
tangle:
	emacs --batch --eval "(require 'org)" --eval "(require 'ob-shell)" --eval "(setq org-confirm-babel-evaluate nil)" --eval '(org-babel-tangle-file "dotfiles.org")'

# Bootstrap on a fresh machine: install Emacs (emacs-plus) via brew, tangle
# configs, create the app shortcut, and pre-build packages.
bootstrap:
	@command -v brew >/dev/null || (echo "Install Homebrew first" && exit 1)
	brew tap $(EMACS_PLUS_TAP)
	brew trust --formula $(EMACS_PLUS_TAP)/$(EMACS_PLUS)
	brew install $(EMACS_PLUS)
	$(MAKE) tangle
	-$(MAKE) emacs-app
	$(MAKE) precompile

# Remove generated files (careful - lists what would be removed first)
clean:
	@echo "Would remove tangled files - not implemented (too destructive without a manifest)"

# Run the dev tools setup script (install missing tools only)
setup-tools:
	fish ~/bin/setup-dev-tools.fish

# Update already-installed dev tools (gup update, cargo install-update -a)
update-tools:
	fish ~/bin/setup-dev-tools.fish --update

# (Re)create the /Applications/Emacs.app shortcut as a Finder alias to the
# emacs-plus app bundle. /Applications is sunlnk-protected, so the replace is
# driven through Finder (no sudo). Re-run after a brew reinstall/upgrade if the
# shortcut goes stale.
emacs-app:
	EMACS_PLUS=$(EMACS_PLUS) ~/bin/emacs-app-shortcut.sh

# Pre-build all Emacs packages from the CLI so the first GUI start is fast.
# Phase 1: load init headless so straight.el clones/byte-compiles packages.
# Phase 2: synchronously native-compile the whole build tree into the
# version-specific eln-cache. The eln-cache is keyed per Emacs version, so
# re-run this after every Emacs version upgrade.
precompile:
	emacs --batch --load ~/.emacs.d/init.el --eval '(message "precompile: init loaded, straight built")'
	emacs --batch --load ~/.emacs.d/precompile.el

# Install the latest Ghostty AppImage to ~/bin and register a desktop launcher (Linux only)
install-ghostty:
	@set -euo pipefail
	if [ "$$(uname -s)" != "Linux" ]; then
		echo "Ghostty AppImage is Linux-only, skipping"; exit 0
	fi
	command -v curl >/dev/null || { echo "curl is required" >&2; exit 1; }
	command -v jq   >/dev/null || { echo "jq is required" >&2; exit 1; }

	case "$$(uname -m)" in
		x86_64|amd64)        arch=x86_64 ;;
		aarch64|arm64)       arch=aarch64 ;;
		*) echo "Unsupported architecture: $$(uname -m)" >&2; exit 1 ;;
	esac

	echo "Querying latest ghostty-appimage release ($$arch)..."
	release="$$(curl -fsSL https://api.github.com/repos/pkgforge-dev/ghostty-appimage/releases/latest)"
	ver="$$(printf '%s' "$$release" | jq -r '.tag_name')"
	url="$$(printf '%s' "$$release" | jq -r --arg arch "$$arch" \
		'.assets[] | select(.name | endswith("-" + $$arch + ".AppImage")) | .browser_download_url')"
	if [ -z "$$url" ] || [ "$$url" = "null" ]; then
		echo "Could not find an AppImage asset for $$arch" >&2; exit 1
	fi

	mkdir -p "$$HOME/bin" "$$HOME/.local/share/applications" "$$HOME/.local/share/icons"
	appimage="$$HOME/bin/ghostty.AppImage"
	echo "Downloading $$ver -> $$appimage"
	curl -fL --progress-bar -o "$$appimage" "$$url"
	chmod +x "$$appimage"

	# Extract the application icon from the AppImage for the launcher
	icon="utilities-terminal"
	tmp="$$(mktemp -d)"
	if ( cd "$$tmp" && "$$appimage" --appimage-extract >/dev/null 2>&1 ); then
		# .DirIcon is the canonical top-level app icon; fall back to a named/any PNG
		# (-L follows the squashfs-root -> AppDir symlink that the extractor creates)
		if [ -f "$$tmp/squashfs-root/.DirIcon" ]; then
			src="$$tmp/squashfs-root/.DirIcon"
		else
			src="$$(find -L "$$tmp/squashfs-root" \( -iname 'com.mitchellh.ghostty.png' -o -iname 'ghostty.png' \) 2>/dev/null | head -1)"
			[ -n "$$src" ] || src="$$(find -L "$$tmp/squashfs-root" -iname '*.png' 2>/dev/null | head -1)"
		fi
		if [ -n "$$src" ]; then
			cp "$$src" "$$HOME/.local/share/icons/com.mitchellh.ghostty.png"
			icon="$$HOME/.local/share/icons/com.mitchellh.ghostty.png"
		fi
	else
		echo "Warning: could not extract icon from AppImage, falling back to a generic icon" >&2
	fi
	rm -rf "$$tmp"

	desktop="$$HOME/.local/share/applications/ghostty.desktop"
	cat > "$$desktop" <<-EOF
	[Desktop Entry]
	Name=Ghostty
	Comment=Fast, native, feature-rich terminal emulator
	Exec=$$appimage
	Icon=$$icon
	Terminal=false
	Type=Application
	Categories=System;TerminalEmulator;
	StartupNotify=true
	EOF

	command -v update-desktop-database >/dev/null && \
		update-desktop-database "$$HOME/.local/share/applications" 2>/dev/null || true

	echo "Installed Ghostty $$ver to $$appimage and registered $$desktop"
