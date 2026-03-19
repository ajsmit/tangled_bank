#!/usr/bin/env bash
set -euo pipefail

: "${QUARTO_VERSION:=1.8.27}"

ARCH="linux-amd64"
TARBALL="quarto-${QUARTO_VERSION}-${ARCH}.tar.gz"
URL="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/${TARBALL}"
INSTALL_DIR="${HOME}/.local/quarto/${QUARTO_VERSION}"
BIN_DIR="${HOME}/.local/bin"

mkdir -p "${INSTALL_DIR}" "${BIN_DIR}"

if [ ! -x "${INSTALL_DIR}/bin/quarto" ]; then
  echo "Installing Quarto ${QUARTO_VERSION}"
  curl -fsSL "${URL}" -o "/tmp/${TARBALL}"
  rm -rf "${INSTALL_DIR}"
  mkdir -p "${INSTALL_DIR}"
  tar -xzf "/tmp/${TARBALL}" -C "${INSTALL_DIR}" --strip-components=1
fi

ln -sf "${INSTALL_DIR}/bin/quarto" "${BIN_DIR}/quarto"
export PATH="${BIN_DIR}:${PATH}"

quarto --version
