#!/bin/bash

# Docker test script for dotfiles installer

set -euo pipefail

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

log_info() {
    echo -e "${BLUE}[TEST]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Build the test image
log_info "Building Docker test image..."
docker build -f Dockerfile.test -t dotfiles-test .

# Test 1: Basic script execution
log_info "Running basic installation test..."
if docker run --rm dotfiles-test; then
    log_success "Basic installation test passed!"
else
    log_error "Basic installation test failed!"
    exit 1
fi

# Test 2: Interactive shell for manual testing
log_info "Starting interactive container for manual testing..."
echo "You can now manually test the script. Type 'exit' when done."
echo "Available commands in container:"
echo "  ./install.sh          - Run full installation"
echo "  which emacs           - Check if emacs was installed"
echo "  which fish            - Check if fish was installed" 
echo "  brew list             - List installed packages"
echo ""

docker run -it --rm dotfiles-test /bin/bash

log_success "Docker testing completed!"