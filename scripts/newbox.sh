#!/bin/bash

# Define the target directory
TARGET_DIR="/home/kali/Content/Walkthroughs/Boxes/BlogEntriesMade"

# Get the box name from user
read -p "Enter boxname value: " box_name

# Full path for the new box directory
base_dir="$TARGET_DIR/$box_name"

# Check if the folder structure already exists BEFORE creating it
if [ ! -d "$base_dir" ]; then
    # Create the directory
    mkdir -p "$base_dir"
    
    # Copy template files
    cp -r /home/kali/Notes/Templates/BoxTemplate/BoxTemplate.org "$base_dir/$box_name-box.org"
    cp /home/kali/Notes/Templates/BoxTemplate/Hashes.txt "$base_dir"
    cp /home/kali/Notes/Templates/BoxTemplate/Passwords.txt "$base_dir"
    cp /home/kali/Notes/Templates/BoxTemplate/Users.txt "$base_dir"
    
    # Change to the new directory and create structure
    cd "$base_dir" || { echo "Failed to cd to $base_dir"; exit 1; }
    
    # Use absolute path instead of ~
    ln -s /home/kali/Notes/screenshots .
    
    mkdir -p loot/ticket scans/nmap scans/bloodhound scans/ldap payloads
    
    # Confirmation message
    echo "Folder structure and files created in $base_dir"
else
    echo "Directory $base_dir already exists, not creating."
fi
