/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.   The last part of the name can be parsed off and used as
 * a pointer name to the saved image.
 */

#include <stdio.h>
#include <gl.h>

#define LESSER(a,b)  ((a) < (b) ? (a) : (b))

extern int SCREEN_BOTTOM;

static short *buffer;
static long bufsize = 0;


char *malloc (), *realloc ();
Panel_save(name, top, bottom, left, right)
	char *name ;
{
    FILE *fp;
    int needed;
    int xsiz, ysiz;
    short x1, y1, x2, y2;
    int remaining, cnt, offset, size, inc;

    if (NULL == (fp = fopen (name, "w")))
	return (-1);

    if (top < bottom)
	swap (&top, &bottom);
    top ++;
    ysiz = top - bottom + 1;

    if (right < left)
	swap (&right, &left);
    right ++;
    xsiz = right - left + 1;
    
    needed = xsiz * ysiz * sizeof (short);

    if (bufsize == 0)
    {
	buffer = (short *) malloc (needed);
	bufsize = needed;
    }
    else
	if (bufsize < needed)
	{
	    buffer = (short *) realloc ((char *) buffer, needed);
	    bufsize = needed;
	}

    if (buffer == NULL)
    {
	fprintf (stderr, "Alloc failed in Panel Save\n\r");
	return (-1);
    }


    x1 = (short) (left);
    y1 = (short) ((SCREEN_BOTTOM - top));
    x2 = (short) (right);
    y2 = (short) ((SCREEN_BOTTOM - bottom)); 

    rectread (x1, y1, x2, y2, buffer);

    fwrite (&x1, sizeof (x1), 1, fp);
    fwrite (&y1, sizeof (y1), 1, fp);
    fwrite (&x2, sizeof (x2), 1, fp);
    fwrite (&y2, sizeof (y2), 1, fp);

    remaining = needed;
    offset = 0;
    fwrite (buffer, needed, 1, fp);
    /*
    inc = BUFSIZ * 10;
    for (cnt = 0 ; remaining > 0 ; cnt ++)
    {
	size = LESSER (remaining, inc);
	fwrite (buffer + offset, size, 1, fp);
	remaining -= size;
	offset += size;
    }
    */

    fclose (fp);
    

    
    return 0;
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
	char *name ;
{
    FILE *fp;
    int needed;
    int xsiz, ysiz;
    short x1, y1, x2, y2;
    int remaining, cnt, offset, size;

    if (NULL == (fp = fopen (name, "r")))
	return (-1); 

    fread (&x1, sizeof (x1), 1, fp);
    fread (&y1, sizeof (y1), 1, fp);
    fread (&x2, sizeof (x2), 1, fp);
    fread (&y2, sizeof (y2), 1, fp);

    size = BUFSIZ * 10;
    offset = 0;

    size = ((x2 - x1 + 1) * (y2 - y1 + 1) * sizeof (short));
    fread (buffer, size, 1, fp);
    /*
    while (1)
    {
	if (0 >= fread (buffer + offset, size, 1, fp))
	    break;

	offset += size;
    }
    */

    rectwrite (x1, y1, x2, y2, buffer);

    fclose (fp);

    return 0; 
}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
    char *name ;
{
    return unlink (name);
}

static
swap (a, b)
    int  *a, *b;
{
    int tmp;

    tmp = *a;
    *a = *b;
    *b = tmp;
}
