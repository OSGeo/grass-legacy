/****************************************************************************
 *
 * MODULE:       g.mkfontcap
 * AUTHOR(S):    Paul Kelly
 * PURPOSE:      Generates the font configuration file by scanning various
 *               directories for GRASS stroke and Freetype-compatible fonts.
 *              
 * COPYRIGHT:    (C) 2007 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <string.h>
#include <grass/gis.h>

#include "local_proto.h"

/**
 * \brief Find Stroke fonts and store them in a global GFONT_CAP struct
 * 
 * The directory $GISBASE/fonts is listed to find all stroke fonts (i.e.
 * files with a .hmp extension).
 * Information on each font is stored in the global GFONT_CAP struct, 
 * fontcap, to be used by the main program.
 **/

void find_stroke_fonts(void)
{
    char *dirpath;
    char **dirlisting;
    int numfiles, i;
     
    G_asprintf (&dirpath, "%s/fonts", G_gisbase());
   
    dirlisting = G__ls (dirpath, &numfiles);
   
    for (i = 0; i < numfiles; i++)
    {
    	if (!strstr(dirlisting[i], ".hmp"))
    	    continue;

    	if (totalfonts >= maxfonts)
    	{
    	    maxfonts += 20;   
            fontcap = (struct GFONT_CAP *) G_realloc(fontcap, 
		                         maxfonts * sizeof(struct GFONT_CAP));
    	}        
       
        /* Path */
        G_asprintf(&fontcap[totalfonts].path, "%s%c%s", dirpath, 
		                              HOST_DIRSEP, dirlisting[i]);
        G_convert_dirseps_to_host(fontcap[totalfonts].path);
        /* Name */
    	*(strstr(dirlisting[i], ".hmp")) = '\0';
    	fontcap[totalfonts].name = G_store(dirlisting[i]);
    	fontcap[totalfonts].longname = G_store(dirlisting[i]);
        /* Font Type */
        fontcap[totalfonts].type = GFONT_STROKE;
        /* These two probably not relevant */
        fontcap[totalfonts].index = 0;
        fontcap[totalfonts].encoding = G_store("utf-8");       
        totalfonts++;
       
        G_free(dirlisting[i]);       
    }
    G_free(dirlisting);
   
}
