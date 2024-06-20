#!/bin/bash

# Run database schema initialization using psql new terminal
gnome-terminal -- bash -c "psql -U adming10x -d postgres -a -f database/schema.sql"

# Build the project
cabal build

# Run cabal run in a new terminal
gnome-terminal -- bash -c "cabal run sportvenue; exec bash"

# Run cabal test in another new terminal
gnome-terminal -- bash -c "cabal run sportvenue-test; exec bash"
