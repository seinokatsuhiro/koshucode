#!/bin/sh
koshu-inout.sh $* -o IMPORT-RANK.md ./import-rank.k ../*/data/IMPORT.k
koshu-inout.sh $* -o DIR-RANK.md -f PATH ./dir-rank.k
