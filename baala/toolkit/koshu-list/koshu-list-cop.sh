#!/bin/sh
# List term-content operators
echo '|== OP : koshu-cop /cop --order' \
  | koshu -i | sed -n "s/^|--.*'//p"
