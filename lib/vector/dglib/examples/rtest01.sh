#!/bin/sh

#
# This test captures correctness of flattening/unflattening operations
# asserting that the input graph to ./unflatten be identical to its
# output.
# There are a number of implicit tests here:
# - When a graph is unflattened all informations kept in the buffers
#   are moved to an avl-tree using gnGrpAddLink(). Link insertions
#   identical to those made by a user program are performed.
#   Before returning gnGrpUnflatten() destroys the buffers.
# - When flattening buffers are rebuilt from the avl-tree.
#

echo "create a large graph and save it to 'g1.grp'"
(./cr_large_graph -g g1.grp > /dev/null) || (echo "error"; return 1) || exit 1
echo "done"

echo "read 'g1.grp'; unflatten it; flatten back again and save it to 'g2.grp'"
(./unflatten -g g1.grp -o g2.grp > /dev/null) || (echo "error"; return 1) || exit 1
echo "done"

echo "convert 'g1.grp' to text 'g1.grp.txt'"
(./view -g g1.grp > g1.grp.txt) || (echo "error"; return 1) || exit 1
echo "done"

echo "convert 'g2.grp' to text 'g2.grp.txt'"
(./view -g g2.grp > g2.grp.txt) || (echo "error"; return 1) || exit 1
echo "done"

echo "compare 'g1.grp.txt' with 'g2.grp.txt'"
(diff -q g1.grp.txt g2.grp.txt && \
	 echo "'g1.grp.txt' and 'g2.grp.txt' are identical") ||
	(echo "'g1.grp.txt' and 'g2.grp.txt' differ") || exit 1
exit 0
