#!/bin/bash
# Before usage move me to another direcvtory.
# The goal of this script is to use a single Burp Pro Licence on all exegol instance.

# Part 1: Preparation and copying resources

# Get the username and home directory
# Check if running with sudo privileges
if [ "$EUID" -ne 0 ]; then
    echo "Please run this script with sudo."
    exit 1
fi

# Get the original user who ran sudo
USER_NAME="${SUDO_USER:-$USER}"
USER_HOME=$(eval echo "~$USER_NAME")

echo "Detected user: $USER_NAME"
echo "User home directory: $USER_HOME"


USER_JAVA="$USER_HOME/.java"
USER_BurpSuiter="$USER_HOME/.BurpSuite"
BurpsuitePro="/home/marc-antoine/BurpSuitePro"


# Define target directory
TARGET_DIR="$USER_HOME/.exegol/my-resources/"
if [ ! -d "$USER_HOME/.java" ]; then
    echo "exegol environment not found, are you sure it is installed?"
    echo "edit TARGET_DIR with the valid path of my-resources directory."
    exit 1
fi

check_success() {
    if [ $? -ne 0 ]; then
        echo "Error occurred in command: $1. Exiting."
        exit 1
    fi
}


# Check if the directory .java exists or if a specific path is set in JAVA_HOME
if [ -d "$USER_JAVA" ]; then
    JAVA_DEST="$TARGET_DIR"".java"
    if [ -d $JAVA_DEST ]; then
        echo ".java directory already copied"
    else
        cp -r "$USER_JAVA" "$TARGET_DIR"
        check_success "cp -r $USER_JAVA $TARGET_DIR"
        echo ".java directory copied to $TARGET_DIR"
        chown $USER_NAME $JAVA_DEST
        check_success "chown $USER_NAME $JAVA_DEST"
    fi
fi

# Check if the directory .BurpSuite exists or if a specific path is set in BURPSUITE_HOME
if [ -d "$USER_BurpSuiter" ]; then

    BURP_DIR="$TARGET_DIR/.BurpSuite"
    if [ -d $BURP_DIR ]; then
        echo ".BurpSuite directory already copied"
    else
        cp -r "$USER_BurpSuiter" "$TARGET_DIR"
        check_success "cp -r $USER_BurpSuiter $TARGET_DIR"
        echo ".BurpSuite directory copied to $TARGET_DIR"
        chown $USER_NAME $BURP_DIR
        check_success "chown $USER_NAME $BURP_DIR"
    fi
fi

# Check if /opt/BurpSuitePro exists or if a specific path is set in BURPSUITEPRO_HOME
if [ -d "$BurpsuitePro" ]; then
    BURPPRO_DIR="$TARGET_DIR/BurpSuitePro"
    if [ -d $BURPPRO_DIR ]; then
        echo "BurpSuitePro directory already copied"
    else
        cp -r "$BurpsuitePro" "$TARGET_DIR"
        check_success "cp -r $BurpsuitePro $TARGET_DIR"
        echo "BurpSuitePro directory copied to $TARGET_DIR"
        chown $USER_NAME $BURPPRO_DIR
        check_success chown $USER_NAME $BURPPRO_DIR
    fi
else
    echo "Error BurpsuitePro directory not found"

fi

# Part 2: Generate a follow-up script for Docker setup

# Define the follow-up script file
FOLLOW_UP_SCRIPT="$TARGET_DIR/setup_in_docker.sh"
cat << EOF > "$FOLLOW_UP_SCRIPT"
#!/bin/bash

# Check if running inside Docker
if [ -d /.exegol ]; then
    echo "Your environment will be modified."
else
    echo "Not running inside Exegol instance. Exiting."
    exit 1
fi


# Create a new user with the detected username
USER_NAME="$USER_NAME"
useradd -m -G root "\$USER_NAME"

# Create symbolic links for resources in /opt/my-resources
ln -s /opt/my-resources/.java "/home/\$USER_NAME/.java"
ln -s /opt/my-resources/.BurpSuite "/home/\$USER_NAME/.BurpSuite"
ln -s /opt/my-resources/BurpSuitePro "/home/\$USER_NAME/BurpSuitePro"

# Set ownership of the resources to the new user
chown -R "\$USER_NAME" /opt/my-resources/.java /opt/my-resources/.BurpSuite /opt/my-resources/BurpSuitePro




test=("bash" "zsh")

for shell in \${test[@]}; do
    rc_file="\$HOME/."\$shell"rc"
    if [ -f \$rc_file ]; then
        echo -n "Add burp shortcut in $shellrc"
        echo "alias b='sudo -u $USER_NAME /home/$USER_NAME/BurpSuitePro/BurpSuitePro'" >> "\$rc_file"
        echo "alias myburp='sudo -u $USER_NAME /home/$USER_NAME/BurpSuitePro/BurpSuitePro'" >> "\$rc_file"
        cmd_found=1
    fi

done

if (( cmd_found == 0 )); then
    echo "no shell found, myburp alias not created"
fi


EOF

# Make the follow-up script executable
chmod +x "$FOLLOW_UP_SCRIPT"
echo "Follow-up Docker setup script created at $FOLLOW_UP_SCRIPT"
echo "execute it in your exegol container :)"
