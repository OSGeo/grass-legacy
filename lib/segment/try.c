#include <grass/segment.h>

#define NCOLS 100
#define NROWS 100
#define SROWS 8
#define SCOLS 8
#define LEN   2
#define NSEGS 4

int main(int argc, char **argv)
{
    SEGMENT seg;
    char data[NROWS*NCOLS*LEN];
    int row, col;
    int i;
    int fd;
    char junk[3];

fprintf (stdout,"creating seg.file\n");
    fd = creat ("seg.file", 0666);
    if (fd < 0)
    {
	perror ("seg.file");
	exit(1);
    }
    segment_format (fd, NROWS,NCOLS,SROWS,SCOLS,LEN);
    close (fd);

fprintf (stdout,"opening seg.file\n");
    fd = open ("seg.file", 2);
    if (fd < 0)
    {
	perror ("seg.file");
	exit(1);
    }
    segment_init (&seg, fd, NSEGS);

    fprintf (stdout,"rows %d, cols %d (len %d)\n", seg.nrows, seg.ncols, seg.len);
    if (seg.nrows != NROWS || seg.ncols != NCOLS || seg.len != LEN)
    {
	fprintf (stdout,"OOPS - wrong segment file\n");
	exit(1);
    }
fprintf (stdout,"writing seg.file\n");
    for (row = 0 ; row < NROWS; row++)
    {
	for (col = 0; col < NCOLS; col++)
	{
		data[col*2] = row;
		data[col*2 + 1] = col;
	}
	segment_put_row (&seg, data, row);
    }
    while(1)
    {
	for (i = 0; i < seg.nseg; i++)
	    if (seg.scb[i].n >= 0)
	    {
		fprintf (stdout,"segment %d age %d",
			seg.scb[i].n, seg.scb[i].age);
		if (i == seg.cur)
		    fprintf (stdout," current");
		fprintf (stdout,"\n");
	    }
	fprintf (stdout,"\nenter row col: ");
	if (!fgets(data,20,stdin)) break;
	if (sscanf (data, "%1s", junk) != 1) continue;
	if (sscanf (data, "%d%d", &row, &col) != 2)
	    fprintf (stdout,"??\n");
	else if(row < 0 || row >= NROWS || col < 0 || col >= NCOLS)
	    fprintf (stdout,"bad row/col value(s)\n");
	else
	{
	    segment_get (&seg, data, row, col);
	    fprintf (stdout,"data = %d %d\n", data[0], data[1]);
	}
    }
    segment_release (&seg) ;
    close (fd);
}
