#include <stdio.h>
#include "GL/gl.h"


static int first = 1;
static long xsiz, ysiz;
static unsigned long *Buffer;

dumprect (name)
    char *name;
{
    FILE *fp;

    if (first)
    {
	getsize (&xsiz, &ysiz);
	if (NULL == (Buffer = (unsigned long *)malloc (xsiz * ysiz * sizeof (long))))
	{
	    fprintf (stderr, "Out of memory\n");
	    return -1;
	}
	first = 0;
    }

    fp = fopen (name, "w");
    /* write dims */
    fwrite (&xsiz, sizeof (int), 1, fp);
    fwrite (&ysiz, sizeof (int), 1, fp);

    lrectread (0, 0, xsiz-1, ysiz-1, Buffer);
    if (ysiz != fwrite (Buffer, xsiz * sizeof (long), ysiz, fp))
	fprintf (stderr, "Write failed file '%s'\n", name);

    fclose (fp);
    return (0);
}

dumprect2 (name)
    char *name;
{
    FILE *fp;

    if (first)
    {
	getsize (&xsiz, &ysiz);
	if (NULL == (Buffer = (unsigned long *)malloc (xsiz * ysiz * sizeof (long))))
	{
	    fprintf (stderr, "Out of memory\n");
	    return -1;
	}
	first = 0;
    }

    fp = fopen (name, "w");
    /* write dims */
    fwrite (&xsiz, sizeof (int), 1, fp);
    fwrite (&ysiz, sizeof (int), 1, fp);

    lrectread (0, 0, xsiz-1, ysiz-1, Buffer);
    if (ysiz != fwrite (Buffer, xsiz * sizeof (long), ysiz, fp))
	fprintf (stderr, "Write failed file '%s'\n", name);

    fclose (fp);

    fp = fopen (name, "r");
    fread (&xsiz, sizeof (int), 1, fp);
    fread (&ysiz, sizeof (int), 1, fp);

    if (ysiz != fread (Buffer, xsiz * sizeof (long), ysiz, fp))
	fprintf (stderr, "Write failed file '%s'\n", name);

    lrectwrite (0, 0, xsiz-1, ysiz-1, Buffer);
    swapbuffers ();
    fclose (fp);

    return (0);
}

loadrect (name)
    char *name;
{
    FILE *fp;

    if (first)
    {
	getsize (&xsiz, &ysiz);
	if (NULL == (Buffer = (unsigned long *)malloc (xsiz * ysiz * sizeof (long))))
	{
	    fprintf (stderr, "Out of memory\n");
	    return -1;
	}
	first = 0;
    }

    fp = fopen (name, "r");
    fread (&xsiz, sizeof (int), 1, fp);
    fread (&ysiz, sizeof (int), 1, fp);
/*DEBUG*/  fprintf (stderr, "Xdim %d  Ydim %d\n", xsiz, ysiz);

    if (ysiz != fread (Buffer, xsiz * sizeof (long), ysiz, fp))
	fprintf (stderr, "Write failed file '%s'\n", name);

    lrectwrite (0, 0, xsiz-1, ysiz-1, Buffer);
    swapbuffers ();
    fclose (fp);

    return (0);
}
