/* %W% %G% */
/*
**  Vpatch  file1 file2 .... composite
**
**   patch 2 or more vector files together createing composite
**
**  the program is very straight forward.  expand header, copy info from
**  each map one at a time into the new map.  then go back and write the new
**  header out.  last thing to do is to 'cat' all the dig_att files together
**
**  no checking is done for overlapping lines.
**  header information will have to be editted afterwards.
*/

/*
**  Written by Dave Gerdes  8/1988
**  US Army Construction Engineering Research Lab
*/
#include <stdio.h>
#include "gis.h"
#include "digit.h"
#include "dig_head.h"

static struct line_pnts Points;

main (argc, argv)
    int argc;
    char *argv[];
{
    register int i, ret, error;
    char path[1024], filename[1024];
    FILE *Out, *In, *fopen ();
    struct head Head;

    setbuf (stdout, NULL);
    G_gisinit (argv[0]);
    if (argc < 4)
	fprintf (stderr, "Usage: %s file1 file2 [fileN ...] dest\n", argv[0]),
	exit (-1);

    G__file_name (path, "", "", G_mapset());

    error = 0;
    for (i = 1 ; i < argc-1 ; i++)
    {
	sprintf (filename, "%s/dig/%s", path, argv[i]);
	if (0 > access (filename, 0))
	{
	    fprintf (stderr, "Cannot read '%s'\n", filename);
	    error++;
	}
    }
    sprintf (filename, "%s/dig/%s", path, argv[argc-1]);
    if (0 <= access (filename, 2))
    {
	fprintf (stderr, "Vector file '%s' already exists\n", argv[argc-1]);
	exit (-1);
    }
    if ((Out = fopen (filename, "w")) == NULL)
    {
	fprintf (stderr, "Cannot write '%s'\n", filename);
	error++;
    }

    if (error)
	exit (-1);

    Points.alloc_points = 0;
    /* place holder */
    dig_write_head_binary (Out, &Head);

    fprintf (stdout, "\n");
    for (i = 1 ; i < argc-1 ; i++)
    {
	fprintf (stdout, "    Patching file %s\n", argv[i]);
	sprintf (filename, "%s/dig/%s", path, argv[i]);
	if ((In = fopen (filename, "r")) == NULL)
	    continue;
	if (i == 1)  /* first time round */
	{
	    /* initialize Head struct */
	    dig_read_head_binary (In, &Head);
	}
	ret = patch (In, Out, &Head);
	if (ret < 0)
	    fprintf (stderr, "Error reading file '%s'.  Some data may not be correct\n");
	fclose (In);
    }
    strcpy (Head.map_name, "Output from Vpatch");
    strcpy (Head.your_name, G_whoami ());
    dig_write_head_binary (Out, &Head);
    fclose (Out);

    fprintf (stdout, "\n  Patching Attribute files\n");

    sprintf (filename, "%s/dig_att/%s", path, argv[argc-1]);
    if ((Out = fopen (filename, "w")) == NULL)
    {
	fprintf (stderr, "Could not write to new attribute file\n");
	goto end;
    }
    for (i = 1 ; i < argc-1 ; i++)
    {
	sprintf (filename, "%s/dig_att/%s", path, argv[i]);
	if ((In = fopen (filename, "r")) == NULL)
	{
	    fprintf (stderr, "Warning, no attribute file for '%s'\n", argv[i]);
	    continue;
	}
	docat (In, Out);
	fclose (In);
    }
    fclose (Out);

end:
    fprintf (stdout, "\n");
    fprintf (stdout, "Patch complete. You must now run support.vect to build dig_plus file.\n");
    fprintf (stdout, "   Intersections at borders will have to be snapped. Try having\n   support.vect run with a very small snapping threshold\n\n");
    fprintf (stdout, "   Lines common between files will need to be editted.\n\n");
    fprintf (stdout, "   The header information may also need to be editted\n\n");

    exit (0);
}

patch (In, Out, Head)
    FILE *In, *Out;
    struct head *Head;
{
    register int itype;
    struct head local_head;
    long offset, ftell ();

    dig_read_head_binary (In, &local_head);
    fseek (Out, 0l, 2);

    Head->orig_scale = GREATER (Head->orig_scale, local_head.orig_scale);
    /*  this is not in the file
    Head->digit_thresh = GREATER (Head->digit_thresh, local_head.digit_thresh);
    */
    Head->map_thresh = GREATER (Head->map_thresh, local_head.map_thresh);
    Head->N = GREATER (Head->N, local_head.N);
    Head->E = GREATER (Head->E, local_head.E);
    Head->W = LESSER (Head->W, local_head.W);
    Head->S = LESSER (Head->S, local_head.S);

    offset = ftell (In);
    while ((itype = dig__Read_line (&Points, In, offset)) > 0)
    {
	dig__Write_line (Out, (char) itype, &Points);
	offset = ftell (In);
    }
    if (itype != -2)
	return (-1);
    return (0);
}

docat (In, Out)
    FILE *In, *Out;
{
    char buf[BUFSIZ];

    while (fgets (buf, BUFSIZ, In) != NULL)
	fputs (buf, Out);
}
