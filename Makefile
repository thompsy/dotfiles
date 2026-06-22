.PHONY: tangle clean install bootstrap setup-tools update-tools emacs-app

# Tangle all config files from the org file
tangle:
	emacs --batch --eval "(require 'org)" --eval "(require 'ob-shell)" --eval "(setq org-confirm-babel-evaluate nil)" --eval '(org-babel-tangle-file "dotfiles.org")'

# Bootstrap on a fresh machine: install Emacs via brew, then tangle
bootstrap:
	@command -v brew >/dev/null || (echo "Install Homebrew first" && exit 1)
	brew install emacs
	$(MAKE) tangle

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
# emacs-plus@30 app bundle. Re-run after a brew reinstall/upgrade if the
# shortcut goes stale.
emacs-app:
	rm -f /Applications/Emacs.app
	osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
