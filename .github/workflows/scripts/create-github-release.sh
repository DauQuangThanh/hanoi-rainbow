#!/usr/bin/env bash
set -euo pipefail

# create-github-release.sh
# Create a GitHub release with all template zip files
# Usage: create-github-release.sh <version>

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <version>" >&2
  exit 1
fi

VERSION="$1"

# Remove 'v' prefix from version for release title
VERSION_NO_V=${VERSION#v}

gh release create "$VERSION" \
  .genreleases/rainbow-template-copilot-sh-"$VERSION".zip \
  .genreleases/rainbow-template-copilot-ps-"$VERSION".zip \
  .genreleases/rainbow-template-claude-sh-"$VERSION".zip \
  .genreleases/rainbow-template-claude-ps-"$VERSION".zip \
  .genreleases/rainbow-template-gemini-sh-"$VERSION".zip \
  .genreleases/rainbow-template-gemini-ps-"$VERSION".zip \
  .genreleases/rainbow-template-cursor-agent-sh-"$VERSION".zip \
  .genreleases/rainbow-template-cursor-agent-ps-"$VERSION".zip \
  .genreleases/rainbow-template-opencode-sh-"$VERSION".zip \
  .genreleases/rainbow-template-opencode-ps-"$VERSION".zip \
  .genreleases/rainbow-template-qwen-sh-"$VERSION".zip \
  .genreleases/rainbow-template-qwen-ps-"$VERSION".zip \
  .genreleases/rainbow-template-windsurf-sh-"$VERSION".zip \
  .genreleases/rainbow-template-windsurf-ps-"$VERSION".zip \
  .genreleases/rainbow-template-codex-sh-"$VERSION".zip \
  .genreleases/rainbow-template-codex-ps-"$VERSION".zip \
  .genreleases/rainbow-template-kilocode-sh-"$VERSION".zip \
  .genreleases/rainbow-template-kilocode-ps-"$VERSION".zip \
  .genreleases/rainbow-template-auggie-sh-"$VERSION".zip \
  .genreleases/rainbow-template-auggie-ps-"$VERSION".zip \
  .genreleases/rainbow-template-roo-sh-"$VERSION".zip \
  .genreleases/rainbow-template-roo-ps-"$VERSION".zip \
  .genreleases/rainbow-template-codebuddy-sh-"$VERSION".zip \
  .genreleases/rainbow-template-codebuddy-ps-"$VERSION".zip \
  .genreleases/rainbow-template-amp-sh-"$VERSION".zip \
  .genreleases/rainbow-template-amp-ps-"$VERSION".zip \
  .genreleases/rainbow-template-shai-sh-"$VERSION".zip \
  .genreleases/rainbow-template-shai-ps-"$VERSION".zip \
  .genreleases/rainbow-template-q-sh-"$VERSION".zip \
  .genreleases/rainbow-template-q-ps-"$VERSION".zip \
  .genreleases/rainbow-template-bob-sh-"$VERSION".zip \
  .genreleases/rainbow-template-bob-ps-"$VERSION".zip \
  --title "Hanoi Rainbow - $VERSION_NO_V" \
  --notes-file release_notes.md
