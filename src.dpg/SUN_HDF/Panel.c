
#include "stdio.h"
#include "df.h"
#include "graphics.h"
#include <pixrect/pixrect.h>
#include <pixrect/pr_io.h>
#include <pixrect/cg8var.h>

/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.
 */

unsigned char *findoffset ();

/*
 * NOTE:  All the save/restore stuff taken directly from the 
 *    Pixrect Reference Manual or the Sunview 1 Programmers Guide
 */
Panel_save(name, top, bottom, left, right)
        char *name ;
{
        Pixrect    *pr;
        FILE       *pfile ;
        int        width = right - left;
        int        height = bottom - top + 1;

        int        type = RT_STANDARD;
        int        copy_flag = 1;
        colormap_t *pcolormap = 0;
	unsigned char red[256], green[256], blue[256];
	unsigned char pal[256][3];
	int ret;
	int i;
	unsigned char *start;

	/*
        pfile = fopen(name, "w") ;
	if (pfile == NULL)
	{
	    fprintf (stderr, "Panel_save(%s)", name);
	    perror("");
	    return;
	}
	*/

        /* pad width to 32bit boundry for pixrect bug */
        if(width % 4)
                width += width % 4;

        pr = mem_create(width, height, 8);

        pw_read(pr, 0, 0, width, height, PIX_SRC, pixwin, left, top);
/*DEBUG*/ fprintf (stderr, "PANEL: Dumping %d %d to '%s'\n", width, height, name);
	pw_getcolormap (pixwin, 0, 256, red, green, blue);
	for (i = 0 ; i < 256 ; i++)
	{
	    pal[i][0] = red[i];
	    pal[i][1] = green[i];
	    pal[i][2] = blue[i];
	}
	DFR8setpalette (pal);

	/*
	start = findoffset (pr->pr_data);
	*/
	start = (unsigned char *) mpr_d (pr)->md_image;
	ret = DFR8putimage (name, start, width, height, DFTAG_RLE);
	if (ret < 0)
	    fprintf (stderr, "HFR8putimage ERROR %d\n", DFerror);

        pr_destroy(pr);
}

unsigned char *
findoffset (ptr)
    unsigned char *ptr;
{
    unsigned char *p;
    int i;

    while (1)
    {
	while (*ptr != 172)
	    ptr++;
	p = ptr;
	for (i = 0 ; i < 50 ; i++)
	{
	    if (*p++ != 172)  /* GRAY  in 256 colormode */
		goto bottom;
	}
	return (ptr);
bottom:
	ptr++;
	continue;
    }
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
        char *name ;
{
#ifdef FOO
        FILE *pfile ;
        Pixrect *pr, *pr_load();
        colormap_t *colormap = NULL;
        int left, top, width, height;

        /*colormap.type = RMT_NONE;*/
        pfile = fopen(name, "r") ;
	if (pfile == NULL)
	{
	    fprintf (stderr, "Panel_restore(%s)", name);
	    perror("");
	    return;
	}

        fread(&left,  sizeof(int), 1, pfile);
        fread(&top,   sizeof(int), 1, pfile);
        fread(&width, sizeof(int), 1, pfile);
        fread(&height,sizeof(int), 1, pfile);
        pr = pr_load(pfile, colormap);
        pw_rop(pixwin,left,top,width,height,PIX_SRC,pr,0,0);

        pr_destroy(pr);
        fclose(pfile) ;
#endif
}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
        char *name ;
{
        unlink(name) ;
}

