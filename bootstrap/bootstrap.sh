#!/usr/bin/env bash

if [[ $UID != 0 ]]; then
	echo "Please run me as root"
	exit 1
fi;

CHEZMOI_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )/../" &> /dev/null && pwd )

mkdir -p /root/.config/fish

ln -sfn "${CHEZMOI_DIR}/dot_config/nixos" /etc/nixos
cp "${CHEZMOI_DIR}/bootstrap/root.config.fish" /root/.config/fish/config.fish
git config --global credential.helper store

pushd /etc/nixos
nix flake update
nixos-rebuild switch --upgrade
