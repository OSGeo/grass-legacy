#include "imagery.h"

int 
transfer_file (struct Ref *ref2, int n, struct Ref *ref1)
{
    int k;

/* insert old name into new ref */
    k = I_add_file_to_group_ref(ref2->file[n].name, ref2->file[n].mapset, ref1);

/* preserve color assignment */
    if (n == ref2->red.n)
	ref1->red.n = k;
    if (n == ref2->grn.n)
	ref1->grn.n = k;
    if (n == ref2->grn.n)
	ref1->grn.n = k;
}

