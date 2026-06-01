#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXT_DIR="${ROOT_DIR}/vscode-hydrangea"
EXT_ID="hydrangea-project.hydrangea-vscode"
EXT_VERSION="0.1.0"
LINK_TARGET="${HOME}/.vscode-server/extensions/${EXT_ID}-${EXT_VERSION}"

cd "${EXT_DIR}"

if [ ! -d node_modules ]; then
  npm ci --no-audit --no-fund
fi

npm run prepare:assets

mkdir -p "$(dirname "${LINK_TARGET}")"
ln -sfn "${EXT_DIR}" "${LINK_TARGET}"

if [ "${1:-}" = "--prepare-only" ]; then
  exit 0
fi

CODE_CLI="$(find "${HOME}/.vscode-server/bin" -path '*/bin/remote-cli/code' -type f -print -quit 2>/dev/null || true)"
if [ -n "${CODE_CLI}" ]; then
  npm run package
  "${CODE_CLI}" --install-extension "${EXT_DIR}/hydrangea-vscode.vsix" --force
else
  echo "Hydrangea VS Code extension linked at ${LINK_TARGET}. Reopen the workspace if highlighting is not visible yet."
fi
