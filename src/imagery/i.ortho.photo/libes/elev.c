/**********************************************************
* I_get_group_elev (group, elev_layer, mapset_elev, tl, math_exp, units, nd);
* I_put_group_elev (group, elev_layer, mapset_elev, tl, math_exp, units, nd);
**********************************************************/
#include "orthophoto.h"

/* Put the "elev" name into the block file "ELEV" */
I_put_group_elev (group, elev, mapset_elev, tl, math_exp, units, nd)
    char *group;
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
    FILE *I_fopen_group_elev_new();
    
/*    G_suppress_warnings(1); */
    fd = (FILE *) I_fopen_group_elev_new(group) ;
/*    G_suppress_warnings(0); */
    if (fd == NULL) return;

    fprintf (fd, "elevation layer :%s\n", elev);
    fprintf (fd, "mapset elevation:%s\n", mapset_elev);
    fprintf (fd, "location        :%s\n", tl);
    fprintf (fd, "math expresion  :%s\n", math_exp);
    fprintf (fd, "units           :%s\n", units);
    fprintf (fd, "no data values  :%s\n", nd);
}

/* Return the elev name from the block file ELEV */
I_get_group_elev (group, elev, mapset_elev, tl, math_exp, units, nd)
    char *group;
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
    FILE *I_fopen_group_elev_old();
    
    if(!I_find_group_elev_file(group)) return 0;
    G_suppress_warnings(1);
    fd = I_fopen_group_elev_old(group) ;
    G_suppress_warnings(0);
    if (!fd) 
    {
       sprintf (buf, "unable to open elevation file for group [%s] in mapset [%s]", group, G_mapset());
       G_warning (buf);
       sleep(3);
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
    fclose(fd);
    return (1);
}


