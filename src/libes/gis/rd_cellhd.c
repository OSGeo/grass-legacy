/* read cell header, or window.
   returns NULL if ok, error message otherwise
   note:  the error message can be freed using free().
*/

#include "gis.h"
#include "glocale.h"
#include <string.h>

#define ERROR(x,line) return error(x,line)
static int scan_item(char *,char *,char *);
static int scan_int(char *,int *);
static char *error(char *,int);

#define F_PROJ   1
#define F_ZONE   2
#define F_NORTH  3
#define F_SOUTH  4
#define F_EAST   5
#define F_WEST   6
#define F_EWRES  7
#define F_NSRES  8
#define F_FORMAT 9
#define F_COMP   10
#define F_COLS   11
#define F_ROWS   12

#define SET(x) flags|=(1<<x)
#define TEST(x) (flags&(1<<x))

char *G__read_Cell_head ( FILE *fd,
    struct Cell_head *cellhd,int is_cellhd)
{
    int flags;
    char *err, *value;
    struct Key_Value *cellhd_file;

    flags = 0;

/* initialize the cell header */
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

    cellhd_file = G_fread_key_value( fd );
   
/* determine projection, zone first */

    value = G_find_key_value("proj", cellhd_file);
    if(value)
    {	
        if (!scan_int (value, &cellhd->proj))
	    ERROR(value,0);
        SET(F_PROJ);       
    }

    value = G_find_key_value("zone", cellhd_file);
    if(value)
    {	
        if (!scan_int (value, &cellhd->zone))
	    ERROR(value,0);
	SET(F_ZONE);
    }
    if (!TEST(F_PROJ))
	ERROR (_("projection field missing"),0);
    if (!TEST(F_ZONE))
	ERROR (_("zone field missing"),0);

/* read the other info */

    value = G_find_key_value("north", cellhd_file);
    if(value)
    {	
	    if (!G_scan_northing (value, &cellhd->north, cellhd->proj))
		ERROR(value,0);
	    SET(F_NORTH);
    }

    value = G_find_key_value("south", cellhd_file);
    if(value)
    {	
	    if (!G_scan_northing (value, &cellhd->south, cellhd->proj))
		ERROR(value,0);
	    SET(F_SOUTH);
    }

    value = G_find_key_value("east", cellhd_file);
    if(value)
    {	
	    if (!G_scan_easting (value, &cellhd->east, cellhd->proj))
		ERROR(value,0);
	    SET(F_EAST);
    }

    value = G_find_key_value("west", cellhd_file);
    if(value)
    {	
	    if (!G_scan_easting (value, &cellhd->west, cellhd->proj))
		ERROR(value,0);
	    SET(F_WEST);
    }

    value = G_find_key_value("e-w resol", cellhd_file);
    if(value)
    {	
	    if (!G_scan_resolution (value, &cellhd->ew_res, cellhd->proj))
		ERROR(value,0);
	    if (cellhd->ew_res <= 0.0)
		ERROR(value,0);
	    SET(F_EWRES);
    }

    value = G_find_key_value("n-s resol", cellhd_file);
    if(value)
    {	
	    if (!G_scan_resolution (value, &cellhd->ns_res, cellhd->proj))
		ERROR(value,0);
	    if (cellhd->ns_res <= 0.0)
		ERROR(value,0);
	    SET(F_NSRES);
    }
   
    value = G_find_key_value("rows", cellhd_file);
    if(value)
    {	
	    if (!scan_int (value, &cellhd->rows))
		ERROR(value,0);
	    if (cellhd->rows <= 0)
		ERROR(value,0);
	    SET(F_ROWS);
    }
   
    value = G_find_key_value("cols", cellhd_file);
    if(value)
    {	
	    if (!scan_int (value, &cellhd->cols))
		ERROR(value,0);
	    if (cellhd->cols <= 0)
		ERROR(value,0);
	    SET(F_COLS);
    }
   
    value = G_find_key_value("format", cellhd_file);
    if(value)
    {	
	    if (!scan_int (value, &cellhd->format))
		ERROR(value,0);
	    SET(F_FORMAT);
    }
   
    value = G_find_key_value("compressed", cellhd_file);
    if(value)
    {	
	    if (!scan_int (value, &cellhd->compressed))
		ERROR(value,0);
	    SET(F_COMP);
    }

    if( G_find_key_value("top", cellhd_file) ||
	G_find_key_value("bottom", cellhd_file) ||
	G_find_key_value("cols3", cellhd_file) ||
	G_find_key_value("rows3", cellhd_file) ||
	G_find_key_value("depths", cellhd_file) ||
	G_find_key_value("e-w resol3", cellhd_file) ||
	G_find_key_value("n-s resol3", cellhd_file) ||
	G_find_key_value("t-b resol", cellhd_file) )
        G_warning("GRASS >=6.x 3-D settings were found and may be lost");
   
    G_free_key_value( cellhd_file );

/* check some of the fields */
    if (!TEST(F_NORTH))
	ERROR (_("north field missing"),0);
    if (!TEST(F_SOUTH))
	ERROR (_("south field missing"),0);
    if (!TEST(F_WEST))
	ERROR (_("west field missing"),0);
    if (!TEST(F_EAST))
	ERROR (_("east field missing"),0);
    if (!TEST(F_EWRES) && !TEST(F_COLS))
	ERROR (_("cols field missing"),0);
    if (!TEST(F_NSRES) && !TEST(F_ROWS))
	ERROR (_("rows field missing"),0);
/* This next stmt is commented out to allow wr_cellhd.c to write
 * headers that will be readable by GRASS 3.1
    if ((TEST(F_ROWS) && TEST(F_NSRES))
    ||  (TEST(F_COLS) && TEST(F_EWRES)))
	ERROR ("row/col and resolution information can not both appear ",0);
 */


/* Adjust and complete the cell header  */
    if((err = G_adjust_Cell_head(cellhd, TEST(F_ROWS), TEST(F_COLS))))
	ERROR (err,0);

    return NULL;
}

static int scan_item(char *buf, char *label, char *value)
{
/* skip blank lines */
    if (sscanf (buf, "%1s", label) != 1)
	return 0;

/* skip comment lines */
    if (*label == '#')
	return 0;

/* must be label: value */
    if (sscanf (buf, "%[^:]:%[^\n]", label, value) != 2)
	return -1;
    G_strip (label);
    G_strip (value);
    return 1;
}

static int scan_int(char *buf, int *n)
{
    char dummy[3];

    *dummy = 0;
    return (sscanf (buf, "%d%1s", n, dummy) == 1 && *dummy == 0);
}

static char *error( char *msg, int line)
{
    char buf[1024];

    if (line)
	sprintf (buf, _("line %d: <%s>"), line, msg);
    else
	sprintf (buf, "<%s>", msg);

    return G_store(buf);
}
