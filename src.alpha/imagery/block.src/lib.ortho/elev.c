/**********************************************************
* I_get_block_elev (block, elev_layer, mapset_elev, tl, math_exp, units, nd);
* I_put_block_elev (block, elev_layer, mapset_elev, tl, math_exp, units, nd);
**********************************************************/
#include "dba_imagery.h"

/* Put the "elev" name into the block file "ELEV" */
I_put_block_elev (block, elev, mapset_elev, tl, math_exp, units, nd)
    char *block;
    char *elev;
    char *mapset_elev;
    char *tl;
    char *math_exp;
    char *units;
    char *nd;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    FILE *fd;
    FILE *I_fopen_block_elev_new();
    
/*    G_suppress_warnings(1); */
    fd = I_fopen_block_elev_new(block) ;
/*    G_suppress_warnings(0); */
    if (!fd) return 0;

    fprintf (fd, "elevation layer :%s\n", elev);
    fprintf (fd, "mapset elevation:%s\n", mapset_elev);
    fprintf (fd, "location        :%s\n", tl);
    fprintf (fd, "math expresion  :%s\n", math_exp);
    fprintf (fd, "units           :%s\n", units);
    fprintf (fd, "no data values  :%s\n", nd);
}

/* Return the elev name from the block file ELEV */
I_get_block_elev (block, elev, mapset_elev, tl, math_exp, units, nd)
    char *block;
    char *elev;
    char *mapset_elev;
    char *tl;
    char *math_exp;
    char *units;
    char *nd;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    FILE *fd;
    FILE *I_fopen_block_elev_old();
    
    G_suppress_warnings(1);
    fd = I_fopen_block_elev_old(block) ;
    G_suppress_warnings(0);
    if (!fd) 
      {
	sprintf (buf, "unable to open elev file for block [%s] in mapset [%s]", block, G_mapset());
	G_warning (buf);
	return 0;
      }
    fgets(buf, sizeof buf, fd);
	sscanf (buf, "elevation layer :%s\n", elev);
    fgets(buf, sizeof buf, fd);
        sscanf (buf, "mapset elevation:%s\n", mapset_elev);
    fgets(buf, sizeof buf, fd);
        sscanf (buf, "location        :%s\n", tl);
     fgets(buf, sizeof buf, fd);   
        sscanf (buf, "math expresion  :%s\n", math_exp);
    fgets(buf, sizeof buf, fd);
        sscanf (buf, "units           :%s\n", units);
    fgets(buf, sizeof buf, fd);
        sscanf (buf, "no data values  :%s\n", nd);
    return (1);
}






