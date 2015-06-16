#!/bin/sh
# List relmap operators
echo '|== OP : koshu-rop /rop --order' \
  | koshu -i | sed -n "s/^|--.*'//p"
