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


# Get the last 3 commit messages
LAST_COMMITS=$(git log --oneline -3 | sed ':a;N;$!ba;s/\n/\\n/g')




# Sync the _site folder to the remote server
echo "Deploying to remote server..."
rsync  --no-perms -avz --delete -e "ssh -p 2244" "$QUARTO_PROJECT_DIR/_site/" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"

if [ $? -eq 0 ]; then
    curl -i -X POST -H 'Content-Type: application/json' -d "{
        \"channel_id\": \"s5dhga8r9bncubmfha7bc4e4ic\",
        \"text\": \"New version of the website has been **successfully** deployed.\\n\\n**Last 3 commits:**\\n\`\`\`$LAST_COMMITS\`\`\`\\n*A message by Alex-bot* :tada:\"
    }" https://b2s.club/hooks/m6abej4eojfizcttsk9zm5giyh

else
#curl -i -X POST -H 'Content-Type: application/json' -d '{"channe_idl": "s5dhga8r9bncubmfha7bc4e4ic", "text": "New version of website failed to deploy.  *A message by Alex-bot*  :tada:"}'  https://b2s.club/hooks/m6abej4eojfizcttsk9zm5giyh

    echo "Deployment failed!"
    exit 1
fi
