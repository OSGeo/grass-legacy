/*
 * $Id$
 */

#include <string.h>
#include "gis.h"

void G_init_timestamp (struct TimeStamp *ts)
{
    ts->count = 0;
}

void G_set_timestamp (struct TimeStamp *ts, DateTime *dt)
{
    datetime_copy (&ts->dt[0],dt);
    ts->count = 1;
}

void G_set_timestamp_range (
    struct TimeStamp *ts,
    DateTime *dt1,DateTime *dt2)
{
    datetime_copy (&ts->dt[0], dt1);
    datetime_copy (&ts->dt[1], dt2);
    ts->count = 2;
}

int G__read_timestamp (FILE *fd, struct TimeStamp *ts)
{
    char buf[1024];
    char comment[2];

    while (fgets(buf, sizeof(buf), fd))
    {
	if (sscanf (buf, "%1s", comment) != 1 || *comment == '#')
	    continue;
	return (G_scan_timestamp (ts, buf) > 0 ? 0 : -1);
    }
    return -2; /* nothing in the file */
}

int G__write_timestamp ( FILE *fd, struct TimeStamp *ts)
{
    char buf[1024];

    if (G_format_timestamp (ts, buf) < 0)
	return -1;
    fprintf (fd, "%s\n", buf);
    return 0;
}

int G_format_timestamp (
    struct TimeStamp *ts,
    char *buf)
{
    char temp1[128], temp2[128];
    *buf = 0;
    if (ts->count > 0)
    {
	if (datetime_format (&ts->dt[0],temp1) != 0)
	    return -1;
    }
    if (ts->count > 1)
    {
	if (datetime_format (&ts->dt[1],temp2) != 0)
	    return -1;
    }
    if (ts->count == 1)
	strcpy (buf, temp1);
    else if (ts->count == 2)
	sprintf (buf, "%s / %s", temp1, temp2);

    return 1;
}

int G_scan_timestamp (
    struct TimeStamp *ts,
    char *buf)
{
    char temp[1024], *t;
    char *slash;
    DateTime dt1, dt2;

    G_init_timestamp(ts);
    for (slash = buf; *slash; slash++)
	if (*slash == '/')
	    break;
    if (*slash)
    {
	t = temp;
	while (buf != slash)
	    *t++ = *buf++;
	*t = 0;
	buf++;
	if (datetime_scan(&dt1,temp) != 0 || datetime_scan(&dt2,buf) != 0)
		return -1;
	G_set_timestamp_range (ts, &dt1, &dt2);
    }
    else
    {
	if(datetime_scan (&dt2, buf) != 0 )
	    return -1;
	G_set_timestamp (ts, &dt2);
    }
    return 1;
}
	
int G_get_timestamps (
    struct TimeStamp *ts,
    DateTime *dt1,DateTime *dt2,
    int *count)
{
    *count = 0;
    if (ts->count > 0)
    {
	datetime_copy (dt1, &ts->dt[0]);
	*count = 1;
    }
    if (ts->count > 1)
    {
	datetime_copy (dt2, &ts->dt[1]);
	*count = 2;
    }

    return 0;
}

/* write timestamp file
 * 1 ok
 * -1 error - can't create timestamp file
 * -2 error - invalid datetime in ts
 */
static int write_timestamp (
    char *maptype,char *mapname,char *element,char *filename,
    struct TimeStamp *ts)
{
    FILE *fd;
    int stat;

    fd = G_fopen_new (element, filename);
    if (fd == NULL)
    {
	G_warning (
		"Can't create timestamp file for %s map %s in mapset %s",
		maptype, mapname, G_mapset());
	return -1;
    }

    stat = G__write_timestamp (fd, ts);
    fclose (fd);
    if (stat == 0)
	return 1;
    G_warning (
	    "Invalid timestamp specified for %s map %s in mapset %s",
	    maptype, mapname, G_mapset());
    return -2;
}

/* read timestamp file
 * 0 no timestamp file
 * 1 ok
 * -1 error - can't open timestamp file
 * -2 error - invalid datetime values in timestamp file
 */
static int read_timestamp (
    char *maptype,char *mapname,char *mapset,
    char *element,char *filename,
    struct TimeStamp *ts)
{
    FILE *fd;
    int stat;

    if (!G_find_file2 (element, filename, mapset))
	return 0;
    fd = G_fopen_old (element, filename, mapset);
    if (fd == NULL)
    {
	G_warning (
		"Can't open timestamp file for %s map %s in mapset %s",
		maptype, mapname, mapset);
	return -1;
    }

    stat = G__read_timestamp (fd, ts);
    fclose (fd);
    if (stat == 0)
	return 1;
    G_warning (
	    "Invalid timestamp file for %s map %s in mapset %s",
	    maptype, mapname, mapset);
    return -2;
}

#define RAST_MISC "cell_misc"
#define VECT_MISC "dig_misc"

int G_read_raster_timestamp (
    char *name,char *mapset,
    struct TimeStamp *ts)
{
    char element[128];

    sprintf (element, "%s/%s", RAST_MISC, name);
    return read_timestamp ("raster", name, mapset, element, "timestamp", ts);
}

int G_remove_raster_timestamp (char *name)
{
    char element[128];

    sprintf (element, "%s/%s", RAST_MISC, name);
    return G_remove(element, "timestamp");
}

int G_read_vector_timestamp (
    char *name,char *mapset,
    struct TimeStamp *ts)
{
    char element[128];

    sprintf (element, "%s/%s", VECT_MISC, name);
    return read_timestamp ("vector", name, mapset, element, "timestamp", ts);
}

int G_remove_vector_timestamp (char *name)
{
    char element[128];

    sprintf (element, "%s/%s", VECT_MISC, name);
    return G_remove(element, "timestamp");
}

int G_write_raster_timestamp (
    char *name,
    struct TimeStamp *ts)
{
    char element[128];

    sprintf (element, "%s/%s", RAST_MISC, name);
    return write_timestamp ("raster", name, element, "timestamp", ts);
}

int G_write_vector_timestamp (
    char *name,
    struct TimeStamp *ts)
{
    char element[128];

    sprintf (element, "%s/%s", VECT_MISC, name);
    return write_timestamp ("vector", name, element, "timestamp", ts);
}
