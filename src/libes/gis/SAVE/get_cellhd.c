/* %W% %G% */
/**********************************************************************
 *
 *  G_get_cellhd (name, mapset, cellhd)
 *      char *name                   name of map
 *      char *mapset                 mapset that map belongs to
 *      struct Cell_head *cellhd    structure to hold cell header info
 *
 *  Reads the cell file header information associated with map layer "map"
 *  in mapset "mapset" into the structure "cellhd".
 *
 *  returns:     0  if successful
 *              -1  on fail
 *
 *  note:   a warning message is printed if the file is
 *       an illegal reclass file, an invalid reclass file, or missing.
 *
 *       Cell header files may contain either grid cell header 
 *       information or reclass information.   If it is a reclass
 *       file, it will give the map and mapset names of the actual
 *       grid cell file being reclassed.  G_get_cellhd(), upon 
 *       reading reclass information will go read the cell header
 *       information for the referenced file.  Only one reference is 
 *       allowed.
 **********************************************************************/

#include "gis.h"
#include "windround.h"
G_get_cellhd (name, mapset, cellhd)
    char *name ;
    char *mapset ;
    struct Cell_head *cellhd ;
{
    char err[100];
    char *type;
    int flag ;
    int try ;
    char real_name[126] ;
    char real_mapset[126] ;

    strcpy(real_name, name) ;
    strcpy(real_mapset, mapset) ;
    try = 0 ;
    flag = -4 ;

    while ( (flag == -4) && (try < 2) )
    {
	flag = G__get_cellhd (real_name, real_mapset, cellhd) ;
	try ++ ;
    }

    switch (flag)
    {
    case -4:
	type = "illegal reclass";
	break;
    case -3:
	type = "invalid reclass";
	break;
    case -2:
	type = "missing";
	break;
    case -1:
	type = "invalid";
	break;
    default:
	return 0;
    }

    sprintf(err,"cell header for [%s] in mapset [%s] %s", name, mapset, type);
    G_warning (err);
    return -1;
}

G__get_cellhd (name, mapset, cellhd)
    char *name ;
    char *mapset ;
    struct Cell_head *cellhd ;
{
    FILE *fptr ;
    char buff[128] ;
    char label[20] ;
    char arg[128] ;
    char i ;
    double value ;

    G_zero (cellhd, sizeof (struct Cell_head));
    if (!(fptr = G_fopen_old ("cellhd", name, mapset)))
	return -2;

    if (fgets(buff,sizeof(buff),fptr) == NULL)
    {
	fclose(fptr) ;
	return -1 ;
    }

/* Check to see if this cellhd is really a reclass file */

    if (!strncmp(buff,"reclas",6))
    {
/* pick up the new mapset and file names */
	for (i=0; i<2; i++)
	{
	    if (fgets(buff,sizeof buff,fptr) == NULL)
	    {
		fclose(fptr) ;
		return -3 ;
	    }
	    sscanf(buff,"%[^:]:%s", label, arg) ;
	    if (! strncmp(label, "maps", 4))
		strcpy(mapset, arg) ;
	    else if (! strncmp(label, "name", 4))
		strcpy(name, arg) ;
	    else
	    {
		fclose(fptr) ;
		return -3 ;
	    }
	} 
	fclose(fptr) ;
	return -4 ;
    }

/* Zero the cellhead */
    cellhd->format  = 0 ;
    cellhd->rows    = 0 ;
    cellhd->cols    = 0 ;
    cellhd->proj    = -1 ;
    cellhd->zone    = -1 ;
    cellhd->compressed = -1;
    cellhd->ew_res  = 0.0 ;
    cellhd->ns_res  = 0.0 ;
    cellhd->north   = 0.0 ;
    cellhd->south   = 0.0 ;
    cellhd->east    = 0.0 ;
    cellhd->west    = 0.0 ;

    do
    {
	sscanf(buff,"%[^:]:%lf", label, &value) ;
	if (! strncmp(label, "proj", 4))
	    cellhd->proj    = (int) value ;
	else if (! strncmp(label, "zone", 4))
	    cellhd->zone    = (int) value ;
	else if (! strncmp(label, "nort", 4))
	    cellhd->north   = value ;
	else if (! strncmp(label, "sout", 4))
	    cellhd->south   = value ;
	else if (! strncmp(label, "west", 4))
	    cellhd->west    = value ;
	else if (! strncmp(label, "east", 4))
	    cellhd->east    = value ;
	else if (! strncmp(label, "e-w ", 4))
	    cellhd->ew_res  = value ;
	else if (! strncmp(label, "n-s ", 4))
	    cellhd->ns_res  = value ;
	else if (! strncmp(label, "form", 4))
	    cellhd->format  = (int) value ;
	else if (! strncmp(label, "comp", 4))
	    cellhd->compressed  = (int) value ;
    } while (fgets(buff,sizeof buff,fptr)) ;
    fclose (fptr);

/* Check for reasonableness */

    if (cellhd->ew_res > 0)
	cellhd->cols = (cellhd->east - cellhd->west) / cellhd->ew_res + WINDOW_ROUND ;
    else
	return -1;

    if (cellhd->cols <= 0)
	return -1;

    if (cellhd->ns_res > 0)
	cellhd->rows = (cellhd->north - cellhd->south) / cellhd->ns_res + WINDOW_ROUND ;
    else
	return -1;

    if (cellhd->rows <= 0)
	return -1;

    return 1 ;
}
