#!/bin/bash

nix-build release.nix -A mobile-wallet-backend
docker load < result
docker tag mobile-wallet-backend:latest plutus4binarapps/mobile-wallet-backend:latest
docker push plutus4binarapps/mobile-wallet-backend:latest