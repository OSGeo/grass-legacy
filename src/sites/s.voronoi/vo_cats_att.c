#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "site.h"

/* Writes out label points and attribute information
   generate = true -> generate serial category number and keep site
                      category values as labels
   generate = false -> keep site category numbers and the first
                       string attribute found
*/

int
write_cats_att (char *outname, FILE *fd_att, FILE *fd_site,
                char *mapName, struct Cell_head window, int generate)
{
    int dims,                   /* number of dimensions */
        cat,                    /* category number type */
        strs,                   /* number of string attributes */
        dbls,                   /* number of double attributes */
        catnum,                 /* category number */
        i,
        label;
    Site *ss;
    struct Categories cats;
    char lbl[MAX_SITE_STRING];

    fprintf(stderr, "Reading site file\n");

    G_init_cats((CELL) 0, mapName, &cats);

    if(G_site_describe(fd_site, &dims, &cat, &strs, &dbls) != 0)
        return -1;

    if(!generate && (cat != CELL_TYPE))
        return -2;

    if((ss = (Site *) G_site_new_struct(cat, dims, strs, dbls)) == NULL) {
        return -3;
    }

    catnum = 0;
    while (G_site_get(fd_site, ss) == 0)
    {
        if(generate) {
            catnum = catnum + 1;
            switch(ss->cattype) {
                case CELL_TYPE:
                    sprintf(lbl, "%d", ss->ccat);
                    break;
                case FCELL_TYPE:
                    sprintf(lbl, "%f", ss->fcat);
                    break;
                case DCELL_TYPE:
                    sprintf(lbl, "%f", ss->dcat);
                    break;
            }
        } else {
            catnum = ss->ccat;
            if(strs != 0)
                strcpy(lbl,ss->str_att[0]);
            else
                strcpy(lbl,"");
        }
        if(write_att(fd_att, 'A', ss->east, ss->north, catnum) < 0)
            return -4;
        if(G_set_cat(catnum, lbl, &cats) < 0) return -4;
    }

    fprintf(stderr, "Writing vector cats\n");
    if(G_write_vector_cats(outname, &cats) < 0) return -4;
    G_free_cats(&cats);
    G_site_free_struct(ss);
    return 0;
}










