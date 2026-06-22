.PHONY: tangle clean install bootstrap setup-tools update-tools emacs-app precompile

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
