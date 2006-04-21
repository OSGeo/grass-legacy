#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/glocale.h>

long 
get_cat (char *type)
{
    char buffer[256] ;
    long cat ;

    for(;;)
    {
        fprintf (stdout, _("Enter the category number for this %s: "), type) ;
        if(!G_gets(buffer)) continue; ;
        if(sscanf(buffer,"%ld", &cat) != 1) continue;
		break;
    }
    return cat;
}

char *
get_label (long cat, struct Categories *labels)
{
    static char buffer[1024] ;

    for(;;)
    {
        fprintf (stdout, _("Enter a label for category %ld [%s] "),
		    cat, G_get_cat ((CELL)cat, labels)) ;
        if(!G_gets(buffer)) continue; ;
	G_strip(buffer);
	break;
    }
    return buffer;
}

int 
get_category (FILE *fd, char *type, struct Categories *labels)
{
    long cat;
    char *lbl;

    R_stabilize(); /* force out all graphics */
    do
    {
	fprintf (stdout,"\n");
	cat = get_cat(type);
	lbl = get_label (cat, labels);
	fprintf (stdout,"%ld [%s]\n", cat, *lbl?lbl:G_get_cat((CELL)cat, labels));
    } while (!G_yes(_("Look ok? "), 1));
    if (*lbl)
	G_set_cat ((CELL)cat, lbl, labels);

    fprintf (fd, "= %ld %s\n", cat, lbl);
    return(0) ;
}
