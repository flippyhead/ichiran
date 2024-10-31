#!/bin/bash

set -e  # Exit on any error

echo "Starting main container init..."
if ! init-sbcl; then
    echo "Failed to initialize SBCL core!"
    exit 1
fi

echo "Starting errata initialization in background..."
init-errata &

echo "Starting suffix initialization in background..."
init-suffixes &

echo "All set, awaiting commands."
sleep infinity