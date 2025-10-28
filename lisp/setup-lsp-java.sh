#!/bin/bash

# Setup script for LSP Java support
# This script downloads and configures the Eclipse JDT.LS

set -e

echo "Setting up LSP Java support..."

# Create necessary directories
mkdir -p ~/.emacs.d/java-lsp
mkdir -p ~/repo/.lsp-java-workspace

# Download JDT.LS (Eclipse Language Server for Java)
echo "Downloading Eclipse JDT.LS..."
cd ~/.emacs.d/java-lsp

# Get the latest version of JDT.LS
JDT_VERSION="1.25.0"
JDT_URL="https://www.eclipse.org/downloads/download.php?file=/jdtls/snapshots/jdt-language-server-${JDT_VERSION}-202312181552.tar.gz"

if [ ! -f "jdt-language-server-${JDT_VERSION}.tar.gz" ]; then
    echo "Downloading JDT Language Server ${JDT_VERSION}..."
    curl -L -o "jdt-language-server-${JDT_VERSION}.tar.gz" "${JDT_URL}&r=1"
fi

# Extract if not already extracted
if [ ! -d "jdt-language-server-${JDT_VERSION}" ]; then
    echo "Extracting JDT Language Server..."
    tar -xzf "jdt-language-server-${JDT_VERSION}.tar.gz"
    mv jdt-language-server-${JDT_VERSION} jdt-language-server
fi

# Create a configuration file for JDT.LS
cat > ~/.emacs.d/java-lsp/config.ini << 'EOF'
[General]
# The workspace directory
workspaceRoot=~/repo/.lsp-java-workspace

# The port for the language server (0 for automatic)
port=0

# Enable debug mode
debug=false

# Enable verbose logging
verbose=true

[Java]
# Java home (will be detected automatically if not specified)
# java.home=

# Maven home (optional)
# maven.home=/Users/wangzhixiong/Downloads/apache-maven-3.9.9

# VM arguments
-XX:+UseG1GC
-XX:+UseStringDeduplication
-Xmx1G
-Xms100m
EOF

echo "JDT.LS setup complete!"
echo ""
echo "Configuration details:"
echo "- JDT.LS installed in: ~/.emacs.d/java-lsp/"
echo "- Workspace directory: ~/repo/.lsp-java-workspace/"
echo "- Maven executable: /Users/wangzhixiong/Downloads/apache-maven-3.9.9/bin/mvn"
echo ""
echo "Now you need to:"
echo "1. Restart Emacs"
echo "2. Open a Java file in a Maven project"
echo "3. LSP should automatically start and download dependencies"
echo ""
echo "Useful key bindings in Java mode:"
echo "- C-c l r r: Rename symbol"
echo "- C-c l g g: Go to definition"
echo "- C-c l g r: Find references"
echo "- C-c l a a: Code actions"
echo "- C-c l f f: Format buffer"
echo "- C-c C-c c: Maven compile"
echo "- C-c C-c t: Maven test"
echo "- C-c C-c p: Maven package"
echo "- C-c C-c i: Maven install"
echo "- C-c C-c r: Custom Maven task"