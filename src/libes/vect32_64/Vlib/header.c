#include "V_.h"
#include "Vect.h"

int Vect_print_header ( struct Map_info *Map)
{
    struct dig_head *dhead;

    dhead = &(Map->head);

    fprintf (stdout,"\nSelected information from dig header\n") ;
    fprintf (stdout," Organization:  %s\n", dhead->organization) ;
    fprintf (stdout," Map Name:      %s\n", dhead->map_name) ;
    fprintf (stdout," Source Date:   %s\n", dhead->source_date) ;
    fprintf (stdout," Orig. Scale:   %ld\n", dhead->orig_scale) ;

    return 0;
}
