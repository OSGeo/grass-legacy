#include "global.h"

int do_del(struct Map_info *Map)
{
    struct ilist *List;
    int i;
    int removed = 0;

    List = select_lines(Map);

    if ( List->n_values == 0 ) {
        /* this case should not happen, see args.c for details */
        G_warning("cats, coord, id, polygon or bbox must be specified");
    }

    if (List->n_values == 0) {
        G_message(_("No features found"));
        return 0;
    }
    G_debug ( 1, "  %d lines selected", List->n_values );

    if (i_flg->answer) 
        fprintf(stdout,"id=");

    /* delete */
    for ( i = 0; i < List->n_values; i++) {
        G_debug ( 2, "Line [%d] deleted", List->value[i] );
        Vect_delete_line(Map, List->value[i]); 
        if (i_flg->answer) 
            fprintf(stdout,"%d%s", List->value[i], i < List->n_values-1 ? "," : "\n");
        removed ++;
    }

    G_message(_("[%d] features deleted"), removed);

    return 1;
}
