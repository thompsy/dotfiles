.PHONY: tangle clean install bootstrap setup-tools

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

# Run the dev tools setup script
setup-tools:
	fish ~/bin/setup-dev-tools.fish
