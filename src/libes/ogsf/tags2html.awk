# need a couple more awks to generate a clean tag file first

awk -F@ ' { printf "<A NAME=\"%s\">\n<PRE>\n%s\n</PRE>\n</A>\n\n\n", $1, $2 } ' < tags.clean > protos.html
