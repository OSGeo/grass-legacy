#include "gis.h"
gethead (fd, cellhd)
FILE *fd;
struct Cell_head *cellhd;
{
	int ok;
	int n,s,e,w,r,c;
	long offset;
	char label[100], value[100];
	char buf[1024];
	char *err, *G_adjust_Cell_head();
	int G_scan_northing();
	int G_scan_easting();
	int scan_int();

	n=s=e=w=r=c=0;

	cellhd->zone = G_zone();
	cellhd->proj = G_projection();
	ok = 1;

	while (1)
	{
		offset = ftell (fd);
		if (!G_getl(buf,sizeof buf, fd)) break;
		*label = *value = NULL;
		sscanf (buf, "%[^:]:%s", label, value);
		if (*label == NULL) continue;	/* ignore blank lines */
		if (*value == NULL) break;         /* this is not part of the header */

		if (strcmp (label, "north") == 0)
		{
			if(!extract (n++, label, value, &cellhd->north, cellhd->proj,
			    G_scan_northing)) ok = 0;
			continue;
		}

		if (strcmp (label, "south") == 0)
		{
			if(!extract (s++, label, value, &cellhd->south, cellhd->proj,
			    G_scan_northing)) ok = 0;
			continue;
		}

		if (strcmp (label, "east") == 0)
		{
			if(!extract (e++, label, value, &cellhd->east, cellhd->proj,
			    G_scan_easting)) ok = 0;
			continue;
		}

		if (strcmp (label, "west") == 0)
		{
			if(!extract (w++, label, value, &cellhd->west, cellhd->proj,
			    G_scan_easting)) ok = 0;
			continue;
		}

		if (strcmp (label, "rows") == 0)
		{
			if(!extract (r++, label, value, &cellhd->rows, cellhd->proj,
			    scan_int)) ok = 0;
			continue;
		}

		if (strcmp (label, "cols") == 0)
		{
			if(!extract (c++, label, value, &cellhd->cols, cellhd->proj,
			    scan_int)) ok = 0;
			continue;
		}
		error ("illegal line in header");
		error (buf);
		ok = 0;
	}
	if (!ok)
		return 0;
	if(missing(n,"north")) ok = 0;
	if(missing(s,"south")) ok = 0;
	if(missing(e,"east")) ok = 0;
	if(missing(w,"west")) ok = 0;
	if(missing(r,"rows")) ok = 0;
	if(missing(c,"cols")) ok = 0;
	if (!ok)
		return 0;
	if (err = G_adjust_Cell_head (cellhd, 1, 1))
	{
		error (err);
		return 0;
	}

	fseek (fd, offset, 0) ;
	return 1;
}

static
scan_int (s, i, proj)
char *s;
int *i;
{
	char dummy[3];

	*dummy = 0;

	if (sscanf (s, "%d%1s", i, dummy) != 1)
		return 0;
	if (*dummy)
		return 0;
	if (*i <= 0)
		return 0;
	return 1;
}

static
extract (count, label, value, data, proj, scanner)
char *label;
char *value;
char *data;
int (*scanner)();
{
	char msg[1024];
	if (count)
	{
		sprintf (msg, "duplicate \"%s\" field in header", label);
		error (msg);
		return 0;
	}
	if (scanner (value, data, proj))
		return 1;
	sprintf (msg, "illegal \"%s\" value in header", label);
	error (msg);
	sprintf (msg, "  %s: %s", label, value);
	error (msg);
	return 0;
}

static
missing (count, label)
char *label;
{
	char msg[200];
	if (count) return 0;
	sprintf (msg, "\"%s\" field missing from header", label);
	error(msg);
	return 1;
}

static
error(msg)
char *msg;
{
	static int first = 1;
	char *G_program_name();

	if (first)
	{
		fprintf (stderr, "%s: ** errors detected in header section **\n\n",
		    G_program_name());
		first = 0;
	}
	fprintf (stderr, "  %s\n", msg);
}
