#include "gis.h"

long
get_cat(type)
    char *type;
{
    char buffer[256] ;
    long cat ;

    for(;;)
    {
        printf("Enter the category number for this %s: ", type) ;
        if(!G_gets(buffer)) continue; ;
        if(sscanf(buffer,"%ld", &cat) != 1) continue;
		break;
    }
    return cat;
}

char *
get_label(cat, labels)
    long cat;
    struct Categories *labels;
{
    static char buffer[1024] ;

    for(;;)
    {
        printf("Enter a label for category %ld [%s] ",
		    cat, G_get_cat ((CELL)cat, labels)) ;
        if(!G_gets(buffer)) continue; ;
	G_strip(buffer);
	break;
    }
    return buffer;
}

get_category(fd, type, labels)
    FILE *fd;
    char *type;
    struct Categories *labels;
{
    long cat;
    char *lbl;

    R_stabilize(); /* force out all graphics */
    do
    {
	printf ("\n");
	cat = get_cat(type);
	lbl = get_label (cat, labels);
	printf ("%ld [%s]\n", cat, *lbl?lbl:G_get_cat((CELL)cat, labels));
    } while (!G_yes("Look ok? ", -1));
    if (*lbl)
	G_set_cat ((CELL)cat, lbl, labels);

    fprintf (fd, "= %ld %s\n", cat, lbl);
}
