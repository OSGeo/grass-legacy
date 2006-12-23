#include "global.h"

int do_del(struct Map_info *Map)
{
    struct ilist *List;
    int i;


    /* cats or coord or bbox */
    if(cat_opt->answer != NULL) {
        
        List = sel_by_cat(Map);
    }
    else if (coord_opt->answer != NULL) {

        List = sel_by_coordinates(Map);
    }
    else if (bbox_opt->answer != NULL) {

        List = sel_by_bbox(Map);
    }
    else if (poly_opt->answer != NULL) {

        List = sel_by_polygon(Map);
    }
    else {
        /* this case should not happen, see args.c for details */
        G_warning("cats, coord or bbox must be specified");
    }

    if (List->n_values == 0) {
        G_message(_("No features found"));
        return 0;
    }
    G_debug ( 1, "  %d lines selected", List->n_values );

    /* delete */
    for ( i = 0; i < List->n_values; i++) {
        G_debug ( 2, "Line [%d] deleted", List->value[i] );
        Vect_delete_line(Map, List->value[i]); 
    }
    return 1;
}
