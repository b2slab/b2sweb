#!/bin/bash

# Local Quarto project directory
QUARTO_PROJECT_DIR="./"
REMOTE_USER="alex"  # Your username on b2slab.upc.edu
REMOTE_HOST="b2slab.upc.edu"
REMOTE_DIR="/var/www/html"  # Deployment directory on the remote server

# Build the Quarto website locally
echo "Building Quarto site..."
cd "$QUARTO_PROJECT_DIR" || exit
quarto render

# Ensure the build was successful
if [ $? -ne 0 ]; then
    echo "Quarto build failed."
    exit 1
fi

# Sync the _site folder to the remote server
echo "Deploying to remote server..."
rsync -avz --delete -e "ssh -p 2244" "$QUARTO_PROJECT_DIR/_site/" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"

if [ $? -eq 0 ]; then
    echo "Deployment successful!"
else
    echo "Deployment failed!"
    exit 1
fi
