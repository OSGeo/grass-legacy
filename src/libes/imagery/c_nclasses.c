#include "imagery.h"
I_cluster_nclasses (C,minsize)
    struct Cluster *C;
{
    int i,n;
    n = 0;
    for (i = 0; i < C->nclasses; i++)
	if (C->count[i] >= minsize)
	    n++;
    return n;
}
