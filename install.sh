#!/bin/bash

# install.sh - Fresh machine setup for dotfiles
# This script installs all necessary tools and configurations for the dotfiles setup

set -euo pipefail

# Package lists - easily updatable constants
CORE_PACKAGES=(
    "fish"
    "starship"
    "jj"
)

CORE_CASK_PACKAGES_MACOS=(
)

CORE_PACKAGES_LINUX=(
)

DEVELOPMENT_PACKAGES=(
    "go"
    "node"
    "python@3.12"
    "rust"
    "ripgrep"
    "fd"
)

# Font installation handled by nerd-installer (installs all fonts)

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

    # Update Homebrew
    log_info "Updating Homebrew..."
    brew update
}

# Install core tools using Homebrew
install_core_tools() {
    log_info "Installing core tools via Homebrew..."

    local tools_to_install=()
    local cask_tools_to_install=()

    # Check common packages
    for package in "${CORE_PACKAGES[@]}"; do
        local cmd="$package"
        # Handle special cases where command name differs from package name
        if [[ "$package" == "jj" ]]; then
            cmd="jj"
        fi

        if ! command_exists "$cmd"; then
            tools_to_install+=("$package")
        fi
    done

    # Check OS-specific packages
    if [[ "$OS" == "macos" ]]; then
        for package in "${CORE_CASK_PACKAGES_MACOS[@]}"; do
            local cmd="$package"
            if ! command_exists "$cmd"; then
                cask_tools_to_install+=("$package")
            fi
        done
    else
        for package in "${CORE_PACKAGES_LINUX[@]}"; do
            local cmd="$package"
            if ! command_exists "$cmd"; then
                tools_to_install+=("$package")
            fi
        done
    fi

    # Install regular packages
    if [[ ${#tools_to_install[@]} -gt 0 ]]; then
        log_info "Installing tools: ${tools_to_install[*]}"
        brew install "${tools_to_install[@]}"
    fi

    # Install cask packages (macOS only)
    if [[ ${#cask_tools_to_install[@]} -gt 0 ]]; then
        log_info "Installing cask tools: ${cask_tools_to_install[*]}"
        brew install --cask "${cask_tools_to_install[@]}"
    fi

    if [[ ${#tools_to_install[@]} -eq 0 && ${#cask_tools_to_install[@]} -eq 0 ]]; then
        log_success "All core tools already installed"
    else
        log_success "Core tools installation completed"
    fi
}

# Install development tools using Homebrew
install_development_tools() {
    log_info "Installing development tools via Homebrew..."

    local dev_tools=()

    # Check development packages
    for package in "${DEVELOPMENT_PACKAGES[@]}"; do
        local cmd="$package"
        # Handle special cases where command name differs from package name
        case "$package" in
            "python@3.12")
                cmd="python3"
                ;;
            "ripgrep")
                cmd="rg"
                ;;
            "rust")
                cmd="rustc"
                ;;
        esac

        if ! command_exists "$cmd"; then
            dev_tools+=("$package")
        fi
    done

    if [[ ${#dev_tools[@]} -eq 0 ]]; then
        log_success "All development tools already installed"
        return
    fi

    log_info "Installing development tools: ${dev_tools[*]}"
    brew install "${dev_tools[@]}"

    log_success "Development tools installation completed"
}

# Install fonts using nerd-installer
install_fonts() {
    log_info "Installing Nerd Fonts..."

    # Download and install nerd-installer v3.4.0
    local installer_dir="/tmp/nerd-installer"
    local installer_url="https://github.com/LionyxML/nerd-installer/archive/refs/tags/v3.4.0.tar.gz"
    
    if [[ ! -d "$installer_dir" ]]; then
        log_info "Downloading nerd-installer v3.4.0..."
        mkdir -p "$installer_dir"
        curl -fsSL "$installer_url" | tar -xz -C "$installer_dir" --strip-components=1
    fi

    if [[ ! -f "$installer_dir/install.sh" ]]; then
        log_error "Failed to download nerd-installer"
        return 1
    fi

    # Make the installer executable
    chmod +x "$installer_dir/install.sh"

    # Run nerd-installer to install all fonts
    log_info "Running nerd-installer to install all Nerd Fonts..."
    if "$installer_dir/install.sh"; then
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
        # Install gpg if not present
        if ! command_exists gpg; then
            log_info "Installing gpg..."
            sudo apt update
            sudo apt install -y gpg
        fi
        
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

        # Install build dependencies
        log_info "Installing build dependencies via apt..."
        sudo apt-get update
        sudo apt-get install -y build-essential autoconf make texinfo \
            libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev \
            libgnutls28-dev libncurses5-dev libjansson-dev libharfbuzz-dev \
            libharfbuzz-bin imagemagick libmagickwand-dev libxml2-dev \
            libcairo2-dev librsvg2-dev libdbus-1-dev libtree-sitter-dev \
            pkg-config libsqlite3-dev

        # Clone and build Emacs
        local build_dir="/tmp/emacs-build"
        rm -rf "$build_dir"
        log_info "Cloning Emacs repository (emacs-30 branch)..."
        git clone --depth 1 --branch emacs-30 https://git.savannah.gnu.org/git/emacs.git "$build_dir"

        cd "$build_dir"
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
        cd /
        rm -rf "$build_dir"

        log_success "Emacs 30 built and installed successfully"
    fi
}

# Setup configurations
setup_configurations() {
    log_info "Setting up configurations..."

    # Ensure we're in the dotfiles directory
    DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    cd "$DOTFILES_DIR"

    # Check if dotfiles.org exists
    if [[ ! -f "dotfiles.org" ]]; then
        log_error "dotfiles.org not found in $DOTFILES_DIR"
        exit 1
    fi

    # Tangle the org file to generate configurations
    log_info "Tangling dotfiles.org to generate configuration files..."
    if command_exists emacs; then
        emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$DOTFILES_DIR/dotfiles.org\")" 2>/dev/null
        log_success "Configuration files generated successfully"
    else
        log_error "Emacs not found. Cannot tangle dotfiles.org"
        exit 1
    fi

    # Set Fish as default shell if requested
    if command_exists fish; then
        current_shell=$(basename "$SHELL")
        if [[ "$current_shell" != "fish" ]]; then
            read -p "Set Fish as your default shell? (y/N): " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                fish_path=$(which fish)
                if ! grep -q "$fish_path" /etc/shells; then
                    echo "$fish_path" | sudo tee -a /etc/shells
                fi
                chsh -s "$fish_path"
                log_success "Fish set as default shell"
            fi
        fi
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

    # Check core tools
    local core_tools=("emacs" "fish" "starship" "jj")
    for tool in "${core_tools[@]}"; do
        if ! command_exists "$tool"; then
            failed_tools+=("$tool")
        fi
    done

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

    # Check development tools
    local dev_tools=("go" "node" "python3" "rg" "fd")
    for tool in "${dev_tools[@]}"; do
        if ! command_exists "$tool"; then
            failed_tools+=("$tool")
        fi
    done

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
    install_homebrew
    install_wezterm
    install_emacs
    install_core_tools
    install_development_tools
    install_fonts
    setup_configurations
    install_emacs_packages
    verify_installation
}

# Run main function
main "$@"
