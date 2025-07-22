#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_DIR="$SCRIPT_DIR/../env"

if [ ! -d "$VENV_DIR" ]; then
    python3 -m venv "$VENV_DIR"
fi

source "$VENV_DIR/bin/activate"
pip install --upgrade pip
pip install beautifulsoup4

if command -v pandoc >/dev/null 2>&1; then
    echo "Pandoc is installed: $(pandoc --version | head -n 1)"
else
    echo "Pandoc is not installed. Please install it before continuing."
    exit 1
fi

echo "Setup complete."
