#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "local_proto.h"

#define DOT 	   "."		/* for determining data type -tw */
#define INT	   "int"
#define FLOAT      "float"
#define DOUBLE     "double"
#define TMPBUFSIZE 8192
#define SEEK_SET   0
#define SEEK_CUR   1

static int error(char *);
static int missing(int,char *);
static int extract(int,char *,char *,void *,int,int (*)());
static int scan_int(char *,int *,int);

int gethead (
FILE *fd,
struct Cell_head *cellhd,
RASTER_MAP_TYPE *d_type,
DCELL *mult,
char **nval)
{
	int ok ;		/* ??? -tw */
	int n,s,e,w,r,c;
	char label[100], value[100];
	char buf[1024];
	char *err;
	int ret, len;

	n = s = e = w = r = c = 0;

	cellhd->zone = G_zone();
	cellhd->proj = G_projection();

/*	while (n == 0 || s== 0 || e == 0 || w == 0 || r == 0 || c == 0)*/
        while(1)
	{
	  if (!G_getl(buf,sizeof buf, fd)) break;
	  len = strlen(buf);
	  *label = *value = '\0';
	  if(NULL == G_strstr(buf, ":")) break;
	  if(sscanf (buf, "%[^:]:%s", label, value)!=2) break;
	  if (*label == '\0') continue;	/* ignore blank lines */

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
  /* try to read optional header fields */

          if (strcmp (label, "type") == 0) /* if not exist then the file scan */
	  {
	    if (*d_type < 0) { /* if data type not set on command line */
		if (!strncmp(value, INT, strlen(INT))) 
			       *d_type = CELL_TYPE;

		else if (!strncmp(value, FLOAT, strlen(FLOAT))) 
			       *d_type = FCELL_TYPE; 
		
		else if (!strncmp(value, DOUBLE, strlen(DOUBLE))) 
			       *d_type = DCELL_TYPE;

		else 
		{
		     G_warning("illegal type field: using type int ");
		     *d_type= CELL_TYPE;
                }
	    }
	    else 
	       G_warning("ignoring type filed in header, type is set on command line");
	    continue;
	  }

	  if (strcmp (label, "multiplier") == 0)
	  {
	    if(G_is_d_null_value(mult)) /* if mult not set on commant line */
	    {
		  if(sscanf(value, "%lf", mult) != 1)
		  {
		     G_warning("illegal multiplier field: using 1.0");
		     *mult = 1.0;
                  }
	    }
	    else 
	       G_warning("ignoring multiplier filed in header, multiplier is set on command line");
	    continue;
	  }

	  if (strcmp (label, "null") == 0)
	  {
	    if(!(*nval)) /* if null val string not set on command line */
		*nval = G_store(value);
	    else 
	       G_warning("ignoring null filed in header, null string is set on command line");
	    continue;
	  }

      } /* while */
      /* the line read was not a header line, but actually
      the first data line, so put it back on the stack and break */
      fseek(fd, -(len+1), SEEK_CUR);
      missing(n,"north") ;
      missing(s,"south") ;
      missing(e,"east") ;
      missing(w,"west") ;
      missing(r,"rows") ;
      missing(c,"cols") ;
	  
      if(!(*nval)) *nval = G_store("*");
      if(G_is_d_null_value(mult))
	    *mult = 1.0;
  /* if data type is not set, then scan data to find out data type */
      if(*d_type<0)
      {
          ret = file_scan(fd);
          if (!ret)
              *d_type = DCELL_TYPE;
          else if (ret == 1) 
              *d_type = CELL_TYPE;
          else {
              error("ERROR: in ascii data format");
              return 0;
          }
      }

      if (err = G_adjust_Cell_head (cellhd, 1, 1))
      {
        error (err);
        return 0;
      }
      return 1;
}

static int scan_int ( char *s, int *i,int proj)
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

static int scan_string (char *s, char *i, int proj)
{
        char dummy[3];

        *dummy = 0;

        if (sscanf (s, "%s%1s", i, dummy) != 1)
                return 0;
        if (*dummy)
                return 0;
        return 1;
}

static int extract ( int count,
char *label, char *value,
void *data, int proj,
int (*scanner)())
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

static int missing (int count, char *label)
{
	char msg[200];
	if (count) return 0;
	sprintf (msg, "\"%s\" field missing from header", label);
	error(msg);
	return 1;
}

static int error( char *msg)
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

	return 0;
}

/* file_scan(): determine data type in ascii format */
int 
file_scan (FILE *fd)
{
  long curpos;
  char tmpbuf[TMPBUFSIZE];
  int  size = TMPBUFSIZE;

  if ((curpos = ftell(fd)) == -1)
	return -1;
  while (!feof(fd)) {
    if (size != fread(tmpbuf, sizeof(char), size, fd)) {
      if (!feof(fd))
	return -1;
    }
    if (strstr(tmpbuf, DOT) != NULL) {
        fseek(fd, curpos-1L, SEEK_SET);
	return 0;
    }
  }
  fseek(fd, curpos-1L, SEEK_SET);
  return 1;
}
