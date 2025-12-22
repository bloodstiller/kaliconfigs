#!/bin/bash

# Script to create new penetration test templates
# Author: Martin
# Description: Creates folder structure and copies appropriate test checklist

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Base paths
TEMPLATE_BASE="$HOME/Templates"
WORK_BASE="$HOME/VMShare/Work/Tests"

# Function to display colored messages
print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_info() {
    echo -e "${YELLOW}[INFO]${NC} $1"
}

# Function to sanitize folder names (replace spaces with underscores, remove special chars)
sanitize_name() {
    echo "$1" | tr ' ' '_' | tr -cd '[:alnum:]_-'
}

# Prompt for job details
echo "========================================="
echo "   Penetration Test Template Creator"
echo "========================================="
echo ""

# Get job number
read -p "Enter Job Number: " job_number
if [ -z "$job_number" ]; then
    print_error "Job number cannot be empty!"
    exit 1
fi

# Get job name
read -p "Enter Job Name: " job_name
if [ -z "$job_name" ]; then
    print_error "Job name cannot be empty!"
    exit 1
fi

# Sanitize inputs
job_number=$(sanitize_name "$job_number")
job_name=$(sanitize_name "$job_name")

# Display test type options
echo ""
echo "Select Test Type:"
echo "1) External Infrastructure Pentest"
echo "2) Web Application Pentest"
echo "3) Cloud Penetration Test"
echo "4) Infrastructure + Web Application"
echo "5) Infrastructure + Cloud"
echo "6) Web Application + Cloud"
echo "7) All (Infrastructure + Web Application + Cloud)"
echo ""
read -p "Enter choice [1-7]: " test_type

# Validate test type
case $test_type in
    1|2|3|4|5|6|7)
        ;;
    *)
        print_error "Invalid test type selected!"
        exit 1
        ;;
esac

# Create main folder name
folder_name="${job_name}_${job_number}"
main_path="$WORK_BASE/$folder_name"

print_info "Creating test environment for: $folder_name"

# Check if folder already exists
if [ -d "$main_path" ]; then
    print_error "Folder $folder_name already exists!"
    read -p "Do you want to overwrite? (yes/no): " overwrite
    if [ "$overwrite" != "yes" ]; then
        print_info "Operation cancelled."
        exit 0
    fi
    print_info "Removing existing folder..."
    rm -rf "$main_path"
fi

# Create main folder
mkdir -p "$main_path"
if [ $? -ne 0 ]; then
    print_error "Failed to create main folder!"
    exit 1
fi
print_success "Created main folder: $main_path"

# Create basic folder structure
scans_path="$main_path/scans"
docs_path="$main_path/docs"
screenshots_path="$main_path/screenshots"
report_path="$main_path/report"

mkdir -p "$docs_path"
print_success "Created docs folder"
mkdir -p "$scans_path"
print_success "Created scans folder"
mkdir -p "$screenshots_path"
print_success "Created screenshots folder"
mkdir -p "$report_path"
print_success "Created report folder"

# Create scan subfolders based on test type
# Default scan folders for infrastructure/web tests
scan_folders=("nmap" "nessus" "nikto" "burpsuite" "wordpress" "nuclei")

# Add cloud folders if cloud test is selected
case $test_type in
    3|5|6|7)
        cloud_folders=("AWS" "Azure" "GCP")
        for folder in "${cloud_folders[@]}"; do
            mkdir -p "$scans_path/$folder"
            print_success "Created cloud subfolder: scans/$folder"
        done
        ;;
esac

# Create standard scan folders for non-cloud-only tests
case $test_type in
    1|2|4|5|6|7)
        for folder in "${scan_folders[@]}"; do
            mkdir -p "$scans_path/$folder"
            print_success "Created subfolder: scans/$folder"
        done
        ;;
esac

# Copy appropriate template(s)
case $test_type in
    1)
        # Infrastructure test only
        source_file="$TEMPLATE_BASE/InfraTestTemplate/External_Infrastructure_Pentest_checklist.org"
        dest_file="$main_path/${job_name}_${job_number}_external_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Infrastructure template to: $dest_file"
        else
            print_error "Template not found: $source_file"
        fi
        ;;
    2)
        # Web application test only
        source_file="$TEMPLATE_BASE/WebAppTestTemplate/web_appliction_penetration_test_checklist.org"
        dest_file="$main_path/${job_name}_${job_number}_web_application_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Web Application template to: $dest_file"
        else
            print_error "Template not found: $source_file"
        fi
        ;;
    3)
        # Cloud test only
        source_file="$TEMPLATE_BASE/CloudTestTemplate/CloudTest_Pentest_Template.org"
        dest_file="$main_path/${job_name}_${job_number}_cloud_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Cloud Penetration Test template to: $dest_file"
        else
            print_error "Template not found: $source_file"
        fi
        ;;
    4)
        # Infrastructure + Web Application
        # Infrastructure
        source_file="$TEMPLATE_BASE/InfraTestTemplate/External_Infrastructure_Pentest_checklist.org"
        dest_file="$main_path/${job_name}_${job_number}_external_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Infrastructure template"
        else
            print_error "Infrastructure template not found: $source_file"
        fi
        
        # Web Application
        source_file="$TEMPLATE_BASE/WebAppTestTemplate/web_appliction_penetration_test_checklist.org"
        dest_file="$main_path/${job_name}_${job_number}_web_application_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Web Application template"
        else
            print_error "Web Application template not found: $source_file"
        fi
        ;;
    5)
        # Infrastructure + Cloud
        # Infrastructure
        source_file="$TEMPLATE_BASE/InfraTestTemplate/External_Infrastructure_Pentest_checklist.org"
        dest_file="$main_path/${job_name}_${job_number}_external_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Infrastructure template"
        else
            print_error "Infrastructure template not found: $source_file"
        fi
        
        # Cloud
        source_file="$TEMPLATE_BASE/CloudTestTemplate/CloudTest_Pentest_Template.org"
        dest_file="$main_path/${job_name}_${job_number}_cloud_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Cloud Penetration Test template"
        else
            print_error "Cloud template not found: $source_file"
        fi
        ;;
    6)
        # Web Application + Cloud
        # Web Application
        source_file="$TEMPLATE_BASE/WebAppTestTemplate/web_appliction_penetration_test_checklist.org"
        dest_file="$main_path/${job_name}_${job_number}_web_application_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Web Application template"
        else
            print_error "Web Application template not found: $source_file"
        fi
        
        # Cloud
        source_file="$TEMPLATE_BASE/CloudTestTemplate/CloudTest_Pentest_Template.org"
        dest_file="$main_path/${job_name}_${job_number}_cloud_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Cloud Penetration Test template"
        else
            print_error "Cloud template not found: $source_file"
        fi
        ;;
    7)
        # All three templates
        # Infrastructure
        source_file="$TEMPLATE_BASE/InfraTestTemplate/External_Infrastructure_Pentest_checklist.org"
        dest_file="$main_path/${job_name}_${job_number}_external_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Infrastructure template"
        else
            print_error "Infrastructure template not found: $source_file"
        fi
        
        # Web Application
        source_file="$TEMPLATE_BASE/WebAppTestTemplate/web_appliction_penetration_test_checklist.org"
        dest_file="$main_path/${job_name}_${job_number}_web_application_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Web Application template"
        else
            print_error "Web Application template not found: $source_file"
        fi
        
        # Cloud
        source_file="$TEMPLATE_BASE/CloudTestTemplate/CloudTest_Pentest_Template.org"
        dest_file="$main_path/${job_name}_${job_number}_cloud_pentest_checklist.org"
        
        if [ -f "$source_file" ]; then
            cp "$source_file" "$dest_file"
            print_success "Copied Cloud Penetration Test template"
        else
            print_error "Cloud template not found: $source_file"
        fi
        ;;
esac

# Copy EngagementTodo.org (required for all tests)
engagement_source="$TEMPLATE_BASE/PreTestTemplate/Engagement_Todo.org"
engagement_dest="$main_path/${job_name}_${job_number}_Engagement_Todo.org"

if [ -f "$engagement_source" ]; then
    cp "$engagement_source" "$engagement_dest"
    print_success "Copied EngagementTodo template"
else
    print_error "EngagementTodo template not found: $engagement_source"
fi

# Copy scan.sh (required for all tests)
scan_source="$TEMPLATE_BASE/TestScripts/scans.sh"
scan_dest="$main_path/scans.sh"

if [ -f "$scan_source" ]; then
    cp "$scan_source" "$scan_dest"
    chmod +x "$scan_dest"
    print_success "Copied scan.sh script"
else
    print_error "scan.sh script not found: $scan_source"
fi

# Create additional common files
touch "$main_path/TODO_${job_name}_${job_number}.org"
print_success "Created TODO_${job_name}_${job_number}.org file"

touch "$main_path/NOTES_${job_name}_${job_number}.org"
print_success "Created Notes_${job_name}_${job_number}.org file"

echo ""
print_success "Test environment created successfully!"
print_info "Location: $main_path"
echo ""
echo "Folder structure:"
tree -L 2 "$main_path" 2>/dev/null || ls -lR "$main_path"
