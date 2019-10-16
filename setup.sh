#!/bin/bash

set -e
set -x

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd)"

ln -s "$DIR/vscode/settings.json" "$HOME/Library/Application Support/Code/User/settings.json"
ln -s "$DIR/vscode/keybindings.json" "$HOME/Library/Application Support/Code/User/keybindings.json"
