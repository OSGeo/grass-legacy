#include "glob.h"

int *
get_neighbor_list (pt, count)
    int pt;
    int *count;
{
    *count = pointlist.neighbors[pt][0];
    return &pointlist.neighbors[pt][1];
}

int
get_number_of_neighbors (pt)
    int pt;
{
    return pointlist.neighbors[pt][0];
}

int
get_neighbor (pt, n)
    int pt, n;
{
    return pointlist.neighbors[pt][n+1];
}

void
unset_neighbor (pt, n)
{
    pointlist.neighbors[pt][n+1] = -1;
}
