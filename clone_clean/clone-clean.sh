#!/bin/bash

# Check if a Git URL is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <git-repo-url>"
  exit 1
fi

# Extract repo name from URL
REPO_URL="$1"
REPO_NAME=$(basename "$REPO_URL" .git)

# Clone the repo
git clone "$REPO_URL"

# Check if clone succeeded
if [ $? -ne 0 ]; then
  echo "Failed to clone repository."
  exit 1
fi

# Remove the .git directory
rm -rf "$REPO_NAME/.git"

echo "Repository '$REPO_NAME' cloned without .git directory."
