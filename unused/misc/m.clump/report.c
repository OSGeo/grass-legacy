#include "glob.h"

/* report a point and all points connected to this point
 * either directly or thru intermediary points
 * returns 1: something reported
 *         0: nothing reported
 */

#define INDENT " "
#define NOINDENT ""

int
report_point (fd, pt)
    FILE *fd;
    int pt;
{
    int n;
    int pt2;
    int count;

    if (pointlist.reported[pt]) /* already reported, skip */
	return 0;
    pointlist.reported[pt] = 1;

    count = get_number_of_neighbors(pt);
    if (count <= 0) 	/* no neighbors - must be a duplicate point */
	return 0;

    /* print the point itself */
    read_and_print (fd, pointlist.fd, pointlist.offset[pt], NOINDENT);

    /* print the neighbors */
    for (n = 0; n < count; n++)
    {
	pt2 = get_neighbor (pt, n);
	if (pt2 >=0) 	/* if still connected */
	    read_and_print (fd, pointlist.fd, pointlist.offset[pt2], INDENT);
    }

    /* recursively report all neighbors to get the clump */
    for (n = 0; n < count; n++)
    {
	pt2 = get_neighbor (pt, n);
	if (pt2 >=0) 	/* if still connected */
	    report_point (fd, pt2);
    }
    return 1; /* something was reported */
}

void
read_point (fd, offset, buf, len)
    FILE *fd;
    long offset;
    char *buf;
    int len;
{
    fseek (fd, offset, 0);
    if (!readline (fd, buf, len))
    {
	fprintf (stderr, "ERROR: internal or system error has occured\n");
	exit(1);
    }
}

void
write_point (fd, buf, indent)
    FILE *fd;
    char *buf, *indent;
{
    char *b;

    for (b = buf; *b; b++)
	if (*b != ' ')
	    break;
    fprintf (fd, "%s%s\n", indent, b);
}

int
read_and_print (outfd, infd, offset, indent)
    FILE *outfd, *infd;
    long offset;
    char *indent;
{
    char buf[4096];

    read_point (infd, offset, buf, sizeof buf);
    write_point (outfd, buf, indent);
}
