```bash
#!/bin/bash

###############################################################################
# API Testing Tools Installation Script
# Installs Burp Suite, Postman, mitmproxy, jwt_tool, Kiterunner, Arjun, ZAP
###############################################################################

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== API Testing Tools Installer ===${NC}\n"

# Function to print status
status() {
    echo -e "${GREEN}[+]${NC} $1"
}

error() {
    echo -e "${RED}[-]${NC} $1"
}

warning() {
    echo -e "${YELLOW}[*]${NC} $1"
}

# Check if running as root for certain operations
check_sudo() {
    if [[ $EUID -ne 0 ]]; then
        warning "Some operations require sudo. You may be prompted for password."
    fi
}

check_sudo

###############################################################################
# Update System
###############################################################################

read -p "Update system packages? (y/n) " -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    status "Updating system packages..."
    sudo apt update -y
    sudo apt upgrade -y
fi

###############################################################################
# Core Dependencies
###############################################################################

status "Installing core dependencies..."

# Git
if ! command -v git &> /dev/null; then
    status "Installing Git..."
    sudo apt-get install git -y
else
    warning "Git already installed"
fi

# Docker
if ! command -v docker &> /dev/null; then
    status "Installing Docker..."
    sudo apt install docker.io -y
    sudo apt-get install docker-compose -y
else
    warning "Docker already installed"
fi

# Go
if ! command -v go &> /dev/null; then
    status "Installing Go..."
    sudo apt install golang-go -y
else
    warning "Go already installed"
fi

# Sublime Text
if ! command -v subl &> /dev/null; then
    status "Installing Sublime Text..."
    wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | \
        gpg --dearmor | \
        sudo tee /etc/apt/trusted.gpg.d/sublimehq-archive.gpg > /dev/null
    echo "deb https://download.sublimetext.com/ apt/stable/" | \
        sudo tee /etc/apt/sources.list.d/sublime-text.list
    sudo apt-get update
    sudo apt-get install sublime-text -y
else
    warning "Sublime Text already installed"
fi

###############################################################################
# Burp Suite
###############################################################################

status "Setting up Burp Suite..."
if ! command -v burpsuite &> /dev/null; then
    sudo apt-get install burpsuite -y
else
    warning "Burp Suite already installed"
fi

# Download Jython for extensions
status "Downloading Jython for Burp extensions..."
JYTHON_JAR="$HOME/.jython/jython-standalone.jar"
mkdir -p "$HOME/.jython"
if [ ! -f "$JYTHON_JAR" ]; then
    wget https://repo1.maven.org/maven2/org/python/jython-standalone/2.7.2/jython-standalone-2.7.2.jar \
        -O "$JYTHON_JAR" 2>/dev/null || warning "Could not download Jython automatically"
else
    warning "Jython already downloaded"
fi

###############################################################################
# Postman
###############################################################################

status "Installing Postman..."
if ! command -v postman &> /dev/null; then
    sudo wget https://dl.pstmn.io/download/latest/linux64 -O /tmp/postman-linux-x64.tar.gz
    sudo tar -xvzf /tmp/postman-linux-x64.tar.gz -C /opt > /dev/null 2>&1
    sudo ln -sf /opt/Postman/Postman /usr/bin/postman
    rm /tmp/postman-linux-x64.tar.gz
else
    warning "Postman already installed"
fi

###############################################################################
# mitmproxy
###############################################################################

status "Installing mitmproxy2swagger..."
if [ ! -d "/opt/mitmproxy2swagger" ]; then
    pipx install mitmproxy2swagger 
    status "mitmproxy2swagger image built"
else
    warning "mitmproxy2swagger already installed"
fi

###############################################################################
# JWT Toolkit
###############################################################################

status "Installing JWT Toolkit (jwt_tool)..."
if [ ! -d "/opt/jwt_tool" ]; then
    sudo git clone https://github.com/ticarpi/jwt_tool /opt/jwt_tool
    cd /opt/jwt_tool
    sudo python3 -m pip install -r requirements.txt --break-system-packages > /dev/null 2>&1
    sudo chmod +x jwt_tool.py
    sudo ln -sf /opt/jwt_tool/jwt_tool.py /usr/bin/jwt_tool
    status "jwt_tool installed and aliased to /usr/bin/jwt_tool"
else
    warning "jwt_tool already installed"
fi

###############################################################################
# Kiterunner
###############################################################################

status "Installing Kiterunner (kr)..."
if [ ! -d "/opt/kiterunner" ]; then
    sudo git clone https://github.com/assetnote/kiterunner.git /opt/kiterunner
    cd /opt/kiterunner
    sudo make build > /dev/null 2>&1
    sudo ln -sf /opt/kiterunner/dist/kr /usr/bin/kr
    status "Kiterunner installed and aliased to /usr/bin/kr"
else
    warning "Kiterunner already installed"
fi

###############################################################################
# Arjun
###############################################################################

status "Installing Arjun..."
if ! python3 -m pip show arjun > /dev/null 2>&1; then
    sudo git clone https://github.com/s0md3v/Arjun.git /opt/Arjun
    cd /opt/Arjun
    pip3 install arjun --break-system-packages > /dev/null 2>&1
    status "Arjun installed"
else
    warning "Arjun already installed"
fi

###############################################################################
# OWASP ZAP
###############################################################################

status "Installing OWASP ZAP..."
if ! command -v zaproxy &> /dev/null; then
    sudo apt install zaproxy -y
else
    warning "OWASP ZAP already installed"
fi

###############################################################################
# Wordlists
###############################################################################

read -p "Download SecLists? (y/n) " -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [ ! -d "$HOME/wordlists/SecLists" ]; then
        status "Downloading SecLists..."
        mkdir -p "$HOME/wordlists"
        cd "$HOME/wordlists"
        sudo wget -c https://github.com/danielmiessler/SecLists/archive/master.zip -O SecList.zip 2>/dev/null
        sudo unzip -q SecList.zip
        sudo rm SecList.zip
        status "SecLists installed to $HOME/wordlists"
    else
        warning "SecLists already downloaded"
    fi
fi

read -p "Download Hacking-APIs? (y/n) " -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [ ! -d "$HOME/wordlists/Hacking-APIs" ]; then
        status "Downloading Hacking-APIs..."
        mkdir -p "$HOME/wordlists"
        cd "$HOME/wordlists"
        sudo wget -c https://github.com/hAPI-hacker/Hacking-APIs/archive/refs/heads/main.zip -O HackingAPIs.zip 2>/dev/null
        sudo unzip -q HackingAPIs.zip
        sudo rm HackingAPIs.zip
        status "Hacking-APIs installed to $HOME/wordlists"
    else
        warning "Hacking-APIs already downloaded"
    fi
fi

###############################################################################
# Summary
###############################################################################

echo
echo -e "${GREEN}=== Installation Complete ===${NC}"
echo
echo "Installed tools:"
echo "  - Burp Suite Community Edition"
echo "  - Postman"
echo "  - mitmproxy2swagger"
echo "  - JWT Toolkit (jwt_tool)"
echo "  - Kiterunner (kr)"
echo "  - Arjun"
echo "  - OWASP ZAP"
echo
echo "Wordlists location: $HOME/wordlists/"
echo
echo -e "${YELLOW}Next steps:${NC}"
echo "  1. Configure Burp proxy: 127.0.0.1:8080"
echo "  2. Import Burp CA certificate from http://burpsuite"
echo "  3. Start mitmproxy with: mitmweb"
echo "  4. Download ZAP add-ons: Fuzzer, OpenAPI Support"
echo

```
