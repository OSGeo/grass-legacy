#include "imagery.h"

transfer_file (ref2, n, ref1)
    struct Ref *ref1, *ref2;
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

