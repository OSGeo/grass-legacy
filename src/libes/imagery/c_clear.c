#include "imagery.h"
I_cluster_clear (C)
    struct Cluster *C;
{
    C->points = NULL;
    C->band_sum = NULL;
    C->band_sum2 = NULL;
    C->class = NULL;
    C->reclass = NULL;
    C->count = NULL;
    C->countdiff = NULL;
    C->sum = NULL;
    C->sumdiff = NULL;
    C->sum2 = NULL;
    C->mean = NULL;
    C->nbands = 0;
    I_init_signatures (&C->S, 0);
}
