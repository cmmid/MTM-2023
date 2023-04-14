#!/usr/bin/env bash

# This script is for updating names of files in a consistent way for solutions / scripts
git mv solutions/$1 solutions/$2
git mv scripts/$1 scripts/$2