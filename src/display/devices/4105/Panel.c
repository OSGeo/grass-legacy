
#include "stdio.h"
#include "graphics.h"

/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.
 */

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

        pfile = fopen(name, "w") ;
	if (pfile == NULL)
	{
	    fprintf (stderr, "Panel_save(%s)", name);
	    perror("");
	    return;
	}

        /* pad width to 32bit boundry for pixrect bug */
        if(width % 4)
                width += width % 4;

        pr = mem_create(width, height, 8);

        pw_read(pr, 0, 0, width, height, PIX_SRC, pixwin, left, top);
        fwrite(&left,   sizeof(int), 1, pfile);
        fwrite(&top,    sizeof(int), 1, pfile);
        fwrite(&width,  sizeof(int), 1, pfile);
        fwrite(&height, sizeof(int), 1, pfile);
        pr_dump(pr, pfile, pcolormap, type, copy_flag);

        pr_destroy(pr);
        fclose(pfile);
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
        char *name ;
{
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
}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
        char *name ;
{
        unlink(name) ;
}

