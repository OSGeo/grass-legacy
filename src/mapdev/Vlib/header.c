#include "V_.h"

Vect_print_header (Map)
    struct Map_info *Map;
{
    struct dig_head *dhead;

    dhead = &(Map->head);

    printf("\nSelected information from dig header\n") ;
    printf(" Organization:  %s\n", dhead->organization) ;
    printf(" Map Name:      %s\n", dhead->map_name) ;
    printf(" Source Date:   %s\n", dhead->source_date) ;
    printf(" Orig. Scale:   %d\n", dhead->orig_scale) ;

    return 0;
}
