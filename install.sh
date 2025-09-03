#!/bin/bash

# install.sh - Fresh machine setup for dotfiles
# This script installs all necessary tools and configurations for the dotfiles setup

set -euo pipefail

# Brewfile location
BREWFILE="./Brewfile"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Detect operating system
detect_os() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        OS="macos"
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        OS="linux"
        sudo apt-get update
        # Install homebrew dependencies. See: https://docs.brew.sh/Homebrew-on-Linux#requirements
        sudo apt-get install -y build-essential procps curl file git gpg \
             autoconf make texinfo \
             libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev \
             libgnutls28-dev libncurses5-dev libjansson-dev libharfbuzz-dev \
             libharfbuzz-bin imagemagick libmagickwand-dev libxml2-dev \
             libcairo2-dev librsvg2-dev libdbus-1-dev libtree-sitter-dev \
             pkg-config libsqlite3-dev

    else
        log_error "Unsupported operating system: $OSTYPE"
        exit 1
    fi
    log_info "Detected OS: $OS"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Verify git configuration
verify_git_config() {
    log_info "Verifying git configuration..."

    local git_name git_email missing_config=false

    git_name=$(git config --get user.name 2>/dev/null || echo "")
    git_email=$(git config --get user.email 2>/dev/null || echo "")

    if [[ -z "$git_name" ]]; then
        log_error "Git user.name is not configured"
        missing_config=true
    fi

    if [[ -z "$git_email" ]]; then
        log_error "Git user.email is not configured"
        missing_config=true
    fi

    if [[ "$missing_config" == true ]]; then
        echo ""
        log_error "Git configuration is incomplete. Please configure git before running this script:"
        echo "  git config --global user.name \"Your Name\""
        echo "  git config --global user.email \"your.email@example.com\""
        echo ""
        exit 1
    fi

    log_success "Git configured as: $git_name <$git_email>"
}

# Install Homebrew (works on both macOS and Linux)
install_homebrew() {
    log_info "Setting up Homebrew..."

    if ! command_exists brew; then
        log_info "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

        # Add Homebrew to PATH for this session
        if [[ "$OS" == "macos" ]]; then
            if [[ -f "/opt/homebrew/bin/brew" ]]; then
                eval "$(/opt/homebrew/bin/brew shellenv)"
            else
                eval "$(/usr/local/bin/brew shellenv)"
            fi
        elif [[ "$OS" == "linux" ]]; then
            eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
        fi

        # Add to shell profile for future sessions
        local shell_profile=""
        if [[ -f "$HOME/.bash_profile" ]]; then
            shell_profile="$HOME/.bash_profile"
        elif [[ -f "$HOME/.zshrc" ]]; then
            shell_profile="$HOME/.zshrc"
        elif [[ -f "$HOME/.bashrc" ]]; then
            shell_profile="$HOME/.bashrc"
        fi

        if [[ -n "$shell_profile" ]]; then
            if [[ "$OS" == "macos" ]]; then
                echo 'eval "$(/opt/homebrew/bin/brew shellenv)" 2>/dev/null || eval "$(/usr/local/bin/brew shellenv)"' >> "$shell_profile"
            elif [[ "$OS" == "linux" ]]; then
                echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' >> "$shell_profile"
            fi
        fi
    else
        log_success "Homebrew already installed"
    fi

    log_info "Updating Homebrew..."
    brew update

    # refresh the shell
    source ~/.bashrc
}

# Install core tools using Homebrew
install_core_tools() {
    log_info "Installing core tools via Homebrew..."

    if [[ ! -f "$BREWFILE" ]]; then
        log_error "Brewfile not found at $BREWFILE"
        exit 1
    fi

    log_info "Installing packages from Brewfile..."
    brew bundle --quiet --file="$BREWFILE"
    log_success "Core tools installation completed"
}

# Install fonts using nerd-installer
install_fonts() {
    log_info "Installing Nerd Fonts..."

    # Download and install nerd-installer v3.4.0
    local installer_dir="/tmp/nerd-installer"
    local installer_url="https://github.com/LionyxML/nerd-installer/archive/refs/tags/3.4.0.tar.gz"

    if [[ ! -d "$installer_dir" ]]; then
        log_info "Downloading nerd-installer v3.4.0..."
        mkdir -p "$installer_dir"

        # Download and extract with proper error checking
        if ! curl -fL "$installer_url" | tar -xz -C "$installer_dir" --strip-components=1; then
            log_error "Failed to download or extract nerd-installer archive"
            return 1
        fi

        log_info "Nerd-installer extracted successfully"
    fi

    if [[ ! -f "$installer_dir/nerd-installer.sh" ]]; then
        log_error "nerd-installer.sh not found in $installer_dir"
        log_info "Available files:"
        find "$installer_dir" -name "*.sh" -o -name "install*" 2>/dev/null || true
        return 1
    fi

    # Make the installer executable
    chmod +x "$installer_dir/nerd-installer.sh"

    # Run nerd-installer to install all fonts
    log_info "Running nerd-installer to install all Nerd Fonts..."
    if "echo '' | $installer_dir/nerd-installer.sh"; then
        log_success "All Nerd Fonts installed successfully"
    else
        log_warning "Some fonts may have failed to install"
    fi

    # Clean up
    rm -rf "$installer_dir"

    log_success "Nerd Fonts installation completed"
}

# Install WezTerm (platform-specific)
install_wezterm() {
    log_info "Installing WezTerm..."

    if command_exists wezterm; then
        log_success "WezTerm already installed"
        return
    fi

    if [[ "$OS" == "macos" ]]; then
        log_info "Installing WezTerm via cask on macOS..."
        brew install --cask wezterm
        log_success "WezTerm installed successfully"
    elif [[ "$OS" == "linux" ]]; then
        log_info "Installing WezTerm via apt on Linux..."

        # Add WezTerm apt repository
        curl -fsSL https://apt.fury.io/wez/gpg.key | sudo gpg --yes --dearmor -o /usr/share/keyrings/wezterm-fury.gpg
        echo 'deb [signed-by=/usr/share/keyrings/wezterm-fury.gpg] https://apt.fury.io/wez/ * *' | sudo tee /etc/apt/sources.list.d/wezterm.list
        sudo chmod 644 /usr/share/keyrings/wezterm-fury.gpg
        sudo apt update
        sudo apt install -y wezterm
        log_success "WezTerm installed successfully"
    fi
}

# Install Emacs (platform-specific)
install_emacs() {
    log_info "Installing Emacs..."

    if command_exists emacs; then
        log_success "Emacs already installed"
        return
    fi

    if [[ "$OS" == "macos" ]]; then
        log_info "Installing emacs-plus@30 on macOS..."
        brew tap d12frosted/emacs-plus
        brew install emacs-plus@30
        log_success "emacs-plus@30 installed successfully"
    elif [[ "$OS" == "linux" ]]; then
        log_info "Building Emacs 30 from source on Linux..."

        # Clone and build Emacs
        local build_dir="/tmp/emacs-build"
        rm -rf "$build_dir"
        mkdir "$build_dir"
        log_info "Downloading Emacs source..."
        curl -s http://www.mirrorservice.org/sites/ftp.gnu.org/gnu/emacs/emacs-30.2.tar.gz | tar -xz -C "$build_dir" --strip-components=1

        pushd "$build_dir"
        log_info "Configuring Emacs build..."
        ./autogen.sh
        ./configure --prefix=/usr/local \
            --with-tree-sitter \
            --with-modules \
            --with-threads \
            --with-imagemagick \
            --with-cairo \
            --with-rsvg \
            --with-dbus \
            --without-xaw3d \
            --with-x-toolkit=gtk3

        log_info "Building Emacs (this may take a while)..."
        make -j"$(nproc)"

        log_info "Installing Emacs to /usr/local..."
        sudo make install

        # Clean up
        popd
        rm -rf "$build_dir"

        log_success "Emacs 30 built and installed successfully"
    fi
}

# Setup configurations
setup_configurations() {
    log_info "Setting up configurations..."

    # Assume we're already in the dotfiles directory
    DOTFILES_DIR="."

    # Check if dotfiles.org exists in current directory
    if [[ ! -f "dotfiles.org" ]]; then
        log_error "dotfiles.org not found in current directory: $(pwd)"
        log_info "Available files:"
        ls -la . || true
        exit 1
    fi

    # Tangle the org file to generate configurations
    log_info "Tangling dotfiles.org to generate configuration files..."
    if command_exists emacs; then
        emacs --batch \
              --eval "(require 'org)" \
              --eval "(require 'ob-shell)" \
              --eval "(setq org-confirm-babel-evaluate nil)" \
              --eval "(org-babel-tangle-file \"$DOTFILES_DIR/dotfiles.org\")" 2>/dev/null
        log_success "Configuration files generated successfully"
    else
        log_error "Emacs not found. Cannot tangle dotfiles.org"
        exit 1
    fi

    # Set Fish as default shell automatically
    if command_exists fish; then
        current_shell=$(basename "$SHELL")
        if [[ "$current_shell" != "fish" ]]; then
            log_info "Setting Fish as default shell..."
            fish_path=$(which fish)
            if ! grep -q "$fish_path" /etc/shells; then
                echo "$fish_path" | sudo tee -a /etc/shells >/dev/null
            fi
            if chsh -s "$fish_path" 2>/dev/null; then
                log_success "Fish set as default shell"
            else
                log_warning "Could not set Fish as default shell (may require logout/login)"
            fi
        else
            log_success "Fish is already the default shell"
        fi
    else
        log_info "Fish not installed, skipping shell change"
    fi
}

# Install additional Emacs packages
install_emacs_packages() {
    log_info "Installing Emacs packages..."

    # Install all-the-icons fonts from within Emacs
    emacs --batch --eval "(progn (package-initialize) (unless (package-installed-p 'all-the-icons) (package-refresh-contents) (package-install 'all-the-icons)) (all-the-icons-install-fonts t))" 2>/dev/null || log_warning "Failed to install all-the-icons fonts"

    log_success "Emacs packages setup completed"
}

# Verify installation
verify_installation() {
    log_info "Verifying installation..."

    local failed_tools=()

    # Check core tools from Brewfile
    if [[ -f "$BREWFILE" ]]; then
        while IFS= read -r line; do
            if [[ $line =~ ^brew\ \"(.+)\"$ ]]; then
                local package="${BASH_REMATCH[1]}"
                local cmd="$package"
                # Handle special cases where command name differs from package name
                case "$package" in
                    "ripgrep")
                        cmd="rg"
                        ;;
                    "rust")
                        cmd="rustc"
                        ;;
                esac

                if ! command_exists "$cmd"; then
                    failed_tools+=("$package")
                fi
            fi
        done < "$BREWFILE"
    fi

    # Check WezTerm differently per OS
    if [[ "$OS" == "macos" ]]; then
        if [[ ! -d "/Applications/WezTerm.app" ]]; then
            failed_tools+=("wezterm")
        fi
    elif [[ "$OS" == "linux" ]]; then
        if ! command_exists wezterm; then
            failed_tools+=("wezterm")
        fi
    fi

    if [[ ${#failed_tools[@]} -eq 0 ]]; then
        log_success "All tools verified successfully!"
        log_info "Next steps:"
        echo "  1. Restart your terminal or source your shell config"
        echo "  2. Open Emacs and let it install packages"
        echo "  3. Configure any tool-specific settings"
        echo ""
        log_success "Dotfiles installation completed! 🎉"
    else
        log_warning "Some tools failed to install or verify:"
        for tool in "${failed_tools[@]}"; do
            echo "  - $tool"
        done
        echo ""
        log_info "You may need to install these manually or check the error logs above."
    fi
}

# Main installation function
main() {
    echo "🚀 Dotfiles Installation Script"
    echo "==============================="
    echo ""

    detect_os
    verify_git_config
    install_homebrew
    install_wezterm
    install_emacs
    install_core_tools
    install_fonts
    setup_configurations
    install_emacs_packages
    verify_installation
}

# Run main function
main "$@"
