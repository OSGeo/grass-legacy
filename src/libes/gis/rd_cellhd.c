/* read cell header, or window.
   returns NULL if ok, error message otherwise
   note:  the error message can be freed using free().
*/

#include "gis.h"
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
    char buf[1024];
    char label[200];
    char value[200];
    int line;
    int flags;
    char *G_adjust_Cell_head();
    char *err;

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

/* determine projection, zone first */

    fseek (fd, 0L, 0);
    for (line = 1; G_getl (buf, sizeof (buf), fd); line++)
    {
	if (TEST(F_PROJ) && TEST(F_ZONE))
	    break;

	switch (scan_item (buf, label, value))
	{
	case -1: ERROR(buf,line);
	case 0:  continue;
	case 1:  break;
	}
	if (strncmp (label, "proj", 4) == 0)
	{
	    if (TEST(F_PROJ))
		ERROR ("duplicate projection field", line);

	    if (!scan_int (value, &cellhd->proj))
		ERROR(buf,line);

	    SET(F_PROJ);
	    continue;
	}
	if (strncmp (label, "zone", 4) == 0)
	{
	    if (TEST(F_ZONE))
		ERROR ("duplicate zone field", line);

	    if (!scan_int (value, &cellhd->zone))
		ERROR(buf,line);

	    SET(F_ZONE);
	    continue;
	}
    }
    if (!TEST(F_PROJ))
	ERROR ("projection field missing",0);
    if (!TEST(F_ZONE))
	ERROR ("zone field missing",0);

/* read the other info */
    fseek (fd, 0L, 0);
    for (line = 1; G_getl (buf, sizeof (buf), fd); line++)
    {
	switch (scan_item (buf, label, value))
	{
	case -1: ERROR(buf,line);
	case 0:  continue;
	case 1:  break;
	}

	if (strncmp (label, "proj", 4) == 0)
	    continue;
	if (strncmp (label, "zone", 4) == 0)
	    continue;

	if (strncmp (label, "nort", 4) == 0)
	{
	    if (TEST(F_NORTH))
		ERROR("duplicate north field", line);
	    if (!G_scan_northing (value, &cellhd->north, cellhd->proj))
		ERROR(buf,line);
	    SET(F_NORTH);
	    continue;
	}
	if (strncmp (label, "sout", 4) == 0)
	{
	    if (TEST(F_SOUTH))
		ERROR("duplicate south field", line);
	    if (!G_scan_northing (value, &cellhd->south, cellhd->proj))
		ERROR(buf,line);
	    SET(F_SOUTH);
	    continue;
	}
	if (strncmp (label, "east", 4) == 0)
	{
	    if (TEST(F_EAST))
		ERROR("duplicate east field", line);
	    if (!G_scan_easting (value, &cellhd->east, cellhd->proj))
		ERROR(buf,line);
	    SET(F_EAST);
	    continue;
	}
	if (strncmp (label, "west", 4) == 0)
	{
	    if (TEST(F_WEST))
		ERROR("duplicate west field", line);
	    if (!G_scan_easting (value, &cellhd->west, cellhd->proj))
		ERROR(buf,line);
	    SET(F_WEST);
	    continue;
	}
	if (strncmp (label, "e-w ", 4) == 0)
	{
	    if (TEST(F_EWRES))
		ERROR("duplicate e-w resolution field", line);
	    if (!G_scan_resolution (value, &cellhd->ew_res, cellhd->proj))
		ERROR(buf,line);
	    if (cellhd->ew_res <= 0.0)
		ERROR(buf,line);
	    SET(F_EWRES);
	    continue;
	}
	if (strncmp (label, "n-s ", 4) == 0)
	{
	    if (TEST(F_NSRES))
		ERROR("duplicate n-s resolution field", line);
	    if (!G_scan_resolution (value, &cellhd->ns_res, cellhd->proj))
		ERROR(buf,line);
	    if (cellhd->ns_res <= 0.0)
		ERROR(buf,line);
	    SET(F_NSRES);
	    continue;
	}
	if (strncmp (label, "rows", 4) == 0)
	{
	    if (TEST(F_ROWS))
		ERROR("duplicate rows field", line);
	    if (!scan_int (value, &cellhd->rows))
		ERROR (buf, line);
	    if (cellhd->rows <= 0)
		ERROR (buf, line);
	    SET(F_ROWS);
	    continue;
	}
	if (strncmp (label, "cols", 4) == 0)
	{
	    if (TEST(F_COLS))
		ERROR("duplicate cols field", line);
	    if (!scan_int (value, &cellhd->cols))
		ERROR (buf, line);
	    if (cellhd->cols <= 0)
		ERROR (buf, line);
	    SET(F_COLS);
	    continue;
	}
	if (strncmp (label, "form", 4) == 0)
	{
	    if (TEST(F_FORMAT))
		ERROR("duplicate format field", line);
	    if (!scan_int (value, &cellhd->format))
		ERROR(buf,line);
	    SET(F_FORMAT);
	    continue;
	}
	if (strncmp (label, "comp", 4) == 0)
	{
	    if (TEST(F_COMP))
		ERROR("duplicate compressed field", line);
	    if (!scan_int (value, &cellhd->compressed))
		ERROR(buf,line);
	    SET(F_COMP);
	    continue;
	}
	ERROR(buf,line);
    }

/* check some of the fields */
    if (!TEST(F_NORTH))
	ERROR ("north field missing",0);
    if (!TEST(F_SOUTH))
	ERROR ("south field missing",0);
    if (!TEST(F_WEST))
	ERROR ("west field missing",0);
    if (!TEST(F_EAST))
	ERROR ("east field missing",0);
    if (!TEST(F_EWRES) && !TEST(F_COLS))
	ERROR ("cols field missing",0);
    if (!TEST(F_NSRES) && !TEST(F_ROWS))
	ERROR ("rows field missing",0);
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
	sprintf (buf, "line %d: <%s>", line, msg);
    else
	sprintf (buf, "<%s>", msg);

    return G_store(buf);
}
