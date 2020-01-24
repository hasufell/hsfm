#!/bin/sh

set -eu

SCRIPT_DIR="$(CDPATH="" cd -- "$(dirname -- "$0")" && pwd -P)"

cd "${SCRIPT_DIR}"

# install ghcup
if ! [ -e "${SCRIPT_DIR}"/.ghcup/bin/ghcup ] ; then
    mkdir -p "${SCRIPT_DIR}"/.ghcup/bin
    curl --proto '=https' --tlsv1.2 -sSf https://gitlab.haskell.org/haskell/ghcup/raw/master/ghcup > "${SCRIPT_DIR}"/.ghcup/bin/ghcup
    chmod +x "${SCRIPT_DIR}"/.ghcup/bin/ghcup
fi

# set up environment
export PATH="${SCRIPT_DIR}/.ghcup/bin:$PATH"
export GHCUP_INSTALL_BASE_PREFIX="${SCRIPT_DIR}"

# get ghc version from cabal.project
ghc_ver=$(grep with-compiler cabal.project | awk '{print $2}' | sed 's/ghc-//')

# install ghc
if ! ghcup list -t ghc -c installed -r | grep -q "${ghc_ver}" ; then
    ghcup install "${ghc_ver}"
fi

# install cabal-install
if [ -z "$(ghcup list -t cabal-install -c installed -r)" ] ; then
    ghcup install-cabal
fi

[ -e "${SCRIPT_DIR}"/bin ] || mkdir "${SCRIPT_DIR}"/bin

# install binary
cabal v2-install \
    --installdir="${SCRIPT_DIR}"/bin \
    --install-method=copy \
    --overwrite-policy=always

echo "Binary installed in: ${SCRIPT_DIR}/bin"

