#include "config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    char *mapset ;
    char name[40] ;
    unsigned char buffer[256] ;
    struct Cell_head cellhd ;
    int cellhd_ok;
    int compressed_old;
    int compressed_new;
    int fd;
    off_t filesize;
    off_t offset, prev_offset;
    int rows_old, rows_new;
    int nbytes;
    int quiet;
    char rname[40], rmapset[40];

    G_gisinit(argv[0]) ;
    quiet = 0;
    if (argc == 3)
    {
	if (strcmp (argv[1],"-")==0)
	{
	    argv++;
	    argc--;
	    quiet = 1;
	}
    }
    if (argc >= 2)
    {
	strcpy(name, argv[1]) ;
	fprintf (stdout,"Edit header for [%s]\n", name);
	mapset = G_mapset() ;
	if (G_find_cell (name, mapset) == NULL)
	{	
	    sprintf (buffer, "%s not found\n", name);
	    G_fatal_error (buffer);
	    exit(1);
	}
    }
    else
    {
	mapset = G_ask_cell_in_mapset("For what layer shall the header file be edited?", name) ;
	if (mapset == NULL)
		exit(0);
    }
    if (G_is_reclass (name, mapset, rname, rmapset) > 0)
    {
	if (strcmp (mapset, rmapset) == 0)
	{
	    fprintf (stderr, "[%s] is a reclass of [%s] - can't edit the header\n",
		name, rname);
	    fprintf (stderr, "Run support on [%s]\n", rname);
	}
	else
	    fprintf (stderr, "[%s] is a reclass of [%s in %s] - can't edit the header\n",
		name, rname, rmapset);
	exit(1);
    }

/* open the cell file */
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    {
	sprintf (buffer, "%s - can't open raster file\n", name);
	G_fatal_error (buffer);
	exit(1);
    }

/* determine file size */
    filesize = lseek (fd, 0L, 2);
    if (filesize == 0)
    {
	sprintf (buffer, "%s - empty raster file\n", name);
	G_fatal_error (buffer);
	exit(1);
    }
    if (filesize < 0)
    {
	sprintf (buffer, "%s - error reading raster file\n", name);
	G_fatal_error (buffer);
	exit(1);
    }

    G_suppress_warnings (quiet);
    cellhd_ok = G_get_cellhd(name, mapset, &cellhd) >= 0;
    G_suppress_warnings (0);
    if (!cellhd_ok)
    {
	G_zero (&cellhd, sizeof (cellhd));
	cellhd.proj = G_projection();
	cellhd.zone = G_zone();
    }
    else
	cellhd.format++;      /* set to number of bytes per cell (for now) */

/*
 * Determine compression type, if any, without consulting cellhd
 *
 * In a compressed file, there is an array of row addresses at the
 * beginning of the file.  Try to read the address array.
 * If the file really is compressed, the addresses will increase,
 * the last one will be the same as the filesize, and the number of
 * row addresses will be one more than the number of rows in the file.
 *
 * If the file matches these conditions, it is probably compressed.
 * The probability of this being wrong is very small.
 * So we will take a safe route that doesn't annoy the user:
 *  If the cellhd wasn't valid, verify the compression with the user.
 *  else if the cellhd says something different, ask the user.
 *  else don't bother the user about it.
 *
 * note: 3.0 address are in machine independent format
 *       pre 3.0 are true longs
 */

/* look for pre3.0 compression */
    compressed_old = 0;
    lseek (fd, 0L, 0);
    if (read (fd, buffer, 3) == 3 &&
	buffer[0] == 251 && buffer[1] == 255 && buffer[2] == 251)
    {
	rows_old = 0;
	offset = -1;
	while (next_row_addr (fd, &offset, 0))
	{
	    if (rows_old > 0 && offset <= prev_offset) break;
	    if (offset >= filesize) break;
	    prev_offset = offset;
	    rows_old++;
	}
	if (offset == filesize)
	    compressed_old = 1; /* it really is old format compressed */
    }

/* look for 3.0 compression */
    compressed_new = 0;
    lseek (fd, 0L, 0);
    if (read (fd, buffer, 1) == 1 && buffer[0] > 0)
    {
	nbytes = buffer[0];
	rows_new = 0;
	offset = -1;
	while (next_row_addr (fd, &offset, nbytes))
	{
	    if (rows_new > 0 && offset <= prev_offset) break;
	    if (offset >= filesize) break;
	    prev_offset = offset;
	    rows_new++;
	}
    }
    if (offset == filesize)
	compressed_new = 1;


/*
 * now check these results against cellhd.compressed
 * cellhd.compressed values are
 * -1 pre 3.0 cellhd - compression unknown (by cellhd alone)
 *  0 not compressed (3.0)
 *  1 compressed (3.0)
 */

/* 
*/
fprintf (stdout,"cellhd compression: %d\n", cellhd.compressed);
fprintf (stdout,"3.0 compression %sindicated\n", compressed_new?"":"not ");
fprintf (stdout,"pre 3.0 compression %sindicated\n", compressed_old?"":"not ");
hitreturn();
/*
*/

/*
 * if we must create a brand new cell header, first find out if the file 
 * is compressed?
 */
    if (!cellhd_ok)
    {
	sprintf (buffer, "[%s] appears to be compressed. Is it? ", name);
	cellhd.compressed = 0;
	if ((compressed_new || compressed_old) && G_yes (buffer, -1))
	{
	    if (compressed_new && compressed_old)
	    {
		while (1)
		{
		    fprintf (stdout,"Please indicate the type of compression\n");
		    fprintf (stdout,"  1. Pre 3.0 compression\n");
		    fprintf (stdout,"  2. 3.0 compression\n");
		    if (!G_gets(buffer)) continue;
		    G_strip (buffer);
		    if (strcmp (buffer, "1") == 0) break;
		    if (strcmp (buffer, "2") == 0) break;
		}
		switch (*buffer)
		{
		case '1': compressed_new = 0; break;
		case '2': compressed_old = 0; break;
		}
	    }
	    if (compressed_new)
	    {
		cellhd.compressed = 1;
		cellhd.rows = rows_new;
	    }
	    else
	    {
		cellhd.compressed = -1;
		cellhd.rows = rows_old;
	    }
	}
    }
    else
    {
	if ((cellhd.compressed < 0) && !compressed_old)
	    cellhd.compressed = 0;
	if ((cellhd.compressed == 0) && compressed_new)
	{
	    fprintf (stdout,"\n***\n");
	    fprintf (stdout,"The header for [%s] says the file is not compressed. ", name);
	    fprintf (stdout,"The file appears to be compressed.\n");
	    fprintf (stdout,"Most likely the header is wrong, but I want you to decide.\n");
	    if (G_yes ("Is the file compressed? ", -1))
	    {
		cellhd.compressed = 1;
	    }
	}
	else if ((cellhd.compressed != 0) && !compressed_new)
	{
	    fprintf (stdout,"\n*** WARNING ***\n\n");
	    fprintf (stdout,"The header for [%s] says the file is compressed. ", name);
	    fprintf (stdout,"The file does NOT appear to be compressed.\n");
	    fprintf (stdout,"Most likely the header is wrong, but I want you to decide.\n");
	    if (!G_yes ("Is the file really compressed? ", -1))
	    {
		cellhd.compressed = 0;
	    }
	}
    }

    if ((cellhd.compressed < 0 && rows_old != cellhd.rows)
    ||  (cellhd.compressed > 0 && rows_new != cellhd.rows))
    {
	int rows;

	rows = (cellhd.compressed>0?rows_new:rows_old);
	fprintf (stdout,"\n*** WARNING ***\n");
	fprintf (stdout,"Header indicates %d row%s in the cell file, but\n",
		cellhd.rows, cellhd.rows==1?"":"s");
	fprintf (stdout,"the actual file format indicates %d row%s\n",
		rows, rows==1?"":"s");
	if (G_yes("Should this discrepancy be corrected? ", -1))
	    cellhd.rows = rows;
    }

    while (1)
    {
	ask_format (name, &cellhd, filesize);
	if (cellhd.compressed == 0)
	{
	    if(check_uncompressed (&cellhd, filesize))
		break;
	}
	else if (cellhd.compressed < 0)
	{
	    if(check_old_compressed (&cellhd, fd))
		break;
	}
	else
	{
	    if(check_new_compressed (&cellhd, fd))
		break;
	}
	hitreturn();
    }

    if(G_edit_cellhd (&cellhd,1) < 0)
	exit(0);

/* adjust from nbytes to nbytes-1 */
/* FP map should be back to -1 */
/* if (cellhd.format > 0) */
	cellhd.format--;

/* Write new header out */
    if (G_put_cellhd(name, &cellhd) == -1)
    {
	sprintf (buffer, "unable to write header for %s", name);
	G_fatal_error(buffer) ;
    }
    else
	fprintf (stdout,"header for [%s] updated\n", name);
    exit(0) ;
}
