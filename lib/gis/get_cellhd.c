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
 *  note:   a warning message for errors encountered.
 *
 *       Cell header files may contain either grid cell header 
 *       information or reclass information.   If it is a reclass
 *       file, it will specify the map and mapset names of the actual
 *       grid cell file being reclassed.  G_get_cellhd(), upon 
 *       reading reclass information will go read the cell header
 *       information for the referenced file.  Only one reference is 
 *       allowed.
 **********************************************************************/

#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "glocale.h"


/*!
 * \brief read the raster header
 *
 * The raster header for the raster file
 * <b>name</b> in the specified <b>mapset</b> is read into the
 * <b>cellhd</b> structure.
 * If there is an error reading the raster header file, a diagnostic message is 
 * printed and -1 is returned. Otherwise, 0 is returned.
 *
 *  \param name
 *  \param mapset
 *  \param cellhd
 *  \return int
 */

int G_get_cellhd (name, mapset, cellhd)
    char *name ;
    char *mapset ;
    struct Cell_head *cellhd ;
{
    FILE *fd;
    int is_reclass;
    char real_name[GNAME_MAX], real_mapset[GMAPSET_MAX];
    char buf[1024];
    char *tail;
    char *err, *G__read_Cell_head();

/*
    is_reclass = G_is_reclass (name, mapset, real_name, real_mapset);
    if (is_reclass < 0)
    {
	sprintf (buf,"Can't read header file for [%s in %s]\n", name, mapset);
	tail = buf + strlen(buf);
	strcpy (tail, "It is a reclass file, but with an invalid format");
	G_warning(buf);
	return -1;
    }
*/
    is_reclass = (G_is_reclass (name, mapset, real_name, real_mapset) > 0);
    if (is_reclass)
    {
	fd = G_fopen_old ("cellhd", real_name, real_mapset);
	if (fd == NULL)
	{
	    sprintf (buf,_("Can't read header file for [%s in %s]\n"), name, mapset);
	    tail = buf + strlen(buf);
	    sprintf (tail, _("It is a reclass of [%s in %s] "), real_name, real_mapset);
	    tail = buf + strlen(buf);
	    if (!G_find_cell (real_name, real_mapset))
		sprintf (tail, _("which is missing"));
	    else
		sprintf (tail, _("whose header file can't be opened"));
	    G_warning (buf);
	    return -1;
	}
    }
    else
    {
	fd = G_fopen_old ("cellhd", name, mapset);
	if (fd == NULL)
	{
	    sprintf (buf, _("Can't open header file for [%s in %s]"), name, mapset);
	    G_warning (buf);
	    return -1;
	}
    }

    err = G__read_Cell_head (fd, cellhd, 1);
    fclose (fd);

    if (err == NULL)
	return 0;

    sprintf (buf, _("Can't read header file for [%s in %s]\n"), name, mapset);
    tail = buf + strlen(buf);
    if (is_reclass)
    {
	sprintf (tail, _("It is a reclass of [%s in %s] whose header file is invalid\n"),
		real_name, real_mapset);
    }
    else
	sprintf (tail, _("Invalid format\n"));
    tail = buf + strlen(buf);
    strcpy (tail, err);
    free (err);
    G_warning (buf);
    return -1;
}
