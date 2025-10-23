#!/usr/bin/env bash

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Banner
echo -e "${GREEN}"
echo "=================================="
echo "  Automated Enumeration Script"
echo "=================================="
echo -e "${NC}"

# Prompt for inputs
read -p "Enter target domain (e.g., example.com): " domain
read -p "Enter subdomain (optional, press Enter to skip): " subdomain
read -p "Enter job name: " jobName
read -p "Enter job number: " jobNumber
read -p "Use HTTPS? (y/n, default: y): " use_https

# Set protocol
if [[ $use_https =~ ^[Nn]$ ]]; then
    prefix="http://"
else
    prefix="https://"
fi

# Construct full target
if [ -z "$subdomain" ]; then
    target="$domain"
else
    target="$subdomain.$domain"
fi

webappUrl="$prefix$target"

# Create output directories
outputDir="./scans"
mkdir -p "$outputDir"/{nmap,nikto,whatweb,testssl,dns,logs}

timestamp=$(date +%Y%m%d-%H%M%S)
logfile="$outputDir/logs/$jobName-$jobNumber-$timestamp.log"

# Display scan information
echo -e "\n${YELLOW}Scan Configuration:${NC}"
echo "Target: $target"
echo "Full URL: $webappUrl"
echo "Job Name: $jobName"
echo "Job Number: $jobNumber"
echo "Output Directory: $outputDir"
echo ""
read -p "Proceed with scan? (y/n): " confirm

if [[ ! $confirm =~ ^[Yy]$ ]]; then
    echo "Scan cancelled."
    exit 0
fi

# Request sudo access upfront
echo -e "\n${YELLOW}[*] Requesting sudo access for privileged scans...${NC}"
sudo -v || {
    echo -e "${RED}Error: sudo access required for Nmap scans${NC}"
    exit 1
}


# Log function
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$logfile"
}

log "===== Starting Enumeration for $target ====="
log "Full URL: $webappUrl"

# Check if target is reachable
echo -e "\n${YELLOW}[*] Testing connectivity...${NC}"
if ping -c 1 -W 2 "$target" &>/dev/null; then
    log "Target is reachable"
else
    log "WARNING: Target may not be reachable via ping (could be blocked)"
fi

# Nmap TCP Scan
echo -e "\n${GREEN}[*] Running Nmap TCP scan...${NC}"
log "Starting Nmap TCP scan"
sudo nmap -sV -sC -O -T4 -oA "$outputDir/nmap/$jobName-TCP" "$target" &>/dev/null &
nmap_tcp_pid=$!

# Nmap UDP Top Ports Scan
echo -e "${GREEN}[*] Running Nmap UDP scan (top 20 ports)...${NC}"
log "Starting Nmap UDP scan"
sudo nmap -sU --top-ports 20 -oA "$outputDir/nmap/$jobName-UDP" "$target" &>/dev/null &
nmap_udp_pid=$!

# Nmap Vulnerability Scan
echo -e "${GREEN}[*] Running Nmap vulnerability scripts...${NC}"
log "Starting Nmap vulnerability scan"
sudo nmap --script vuln -oA "$outputDir/nmap/$jobName-vulns" "$target" &>/dev/null &
nmap_vuln_pid=$!

# Nikto Scan
echo -e "${GREEN}[*] Running Nikto scan...${NC}"
log "Starting Nikto scan"
nikto -h "$webappUrl" -output "$outputDir/nikto/$jobName-nikto-results.txt" &>/dev/null &
nikto_pid=$!

# WhatWeb Technology Detection
echo -e "${GREEN}[*] Running WhatWeb technology detection...${NC}"
log "Starting WhatWeb scan"
whatweb "$webappUrl" -a 3 --log-verbose="$outputDir/whatweb/$jobName-tech.txt" &>/dev/null &
whatweb_pid=$!

# SSL/TLS Testing (if HTTPS)
#if [[ $prefix == "https://" ]]; then
    #echo -e "${GREEN}[*] Running SSL/TLS tests...${NC}"
    #log "Starting SSL/TLS tests"
    #testssl.sh --jsonfile "$outputDir/testssl/$jobName-ssl.json" "$webappUrl" &>/dev/null &
    #testssl_pid=$!
#fi

# DNS Enumeration
#echo -e "${GREEN}[*] Running DNS enumeration...${NC}"
#log "Starting DNS enumeration"
#dnsenum --enum "$domain" -o "$outputDir/dns/$jobName-dns.txt" &>/dev/null &
#dnsenum_pid=$!

# Wait for all scans to complete
echo -e "\n${YELLOW}[*] Waiting for all scans to complete...${NC}"
echo "This may take several minutes depending on the target."

wait $nmap_tcp_pid 2>/dev/null && echo -e "${GREEN}✓${NC} Nmap TCP scan complete"
wait $nmap_udp_pid 2>/dev/null && echo -e "${GREEN}✓${NC} Nmap UDP scan complete"
wait $nmap_vuln_pid 2>/dev/null && echo -e "${GREEN}✓${NC} Nmap vulnerability scan complete"
wait $nikto_pid 2>/dev/null && echo -e "${GREEN}✓${NC} Nikto scan complete"
wait $whatweb_pid 2>/dev/null && echo -e "${GREEN}✓${NC} WhatWeb scan complete"

#if [[ $prefix == "https://" ]]; then
    #wait $testssl_pid 2>/dev/null && echo -e "${GREEN}✓${NC} SSL/TLS tests complete"
#fi

#wait $dnsenum_pid 2>/dev/null && echo -e "${GREEN}✓${NC} DNS enumeration complete"

log "===== All scans completed ====="

# Summary
echo -e "\n${GREEN}=================================="
echo "  Scan Complete!"
echo "==================================${NC}"
echo "Results saved to: $outputDir"
echo ""
echo "Quick summary files:"
echo "  • Nmap TCP: $outputDir/nmap/$jobName-TCP.nmap"
echo "  • Nikto: $outputDir/nikto/$jobName-nikto-results.txt"
echo "  • WhatWeb: $outputDir/whatweb/$jobName-tech.txt"
#if [[ $prefix == "https://" ]]; then
    #echo "  • SSL/TLS: $outputDir/testssl/$jobName-ssl.json"
#fi
#echo "  • DNS: $outputDir/dns/$jobName-dns.txt"
echo ""
echo "Log file: $logfile"
