#!/usr/bin/env bash

# This script is for comparing solutions / scripts for parallel structure
diff scripts/$1 solutions/$1 | more
