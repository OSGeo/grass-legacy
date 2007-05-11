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

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>

#include <grass/config.h>
#include <grass/gis.h>
#include <grass/freetypecap.h>

#include "local_proto.h"

#ifdef HAVE_FT2BUILD_H
#include <ft2build.h>
#include FT_FREETYPE_H

struct freetype_face
{
   FT_Face face; /**< Freetype font face object */    
   int index;    /**< Face index within the font file; FT_Face should provide 
		  *   this information but it seems to be always 0 */
   char *path;   /**< Full pathname to the font file described by FT_Face */
};

static FT_Library ftlibrary;
static struct freetype_face *ftfaces;
static int num_ftfaces;
static size_t memsize;

static void find_fonts(const char *);
static void free_freetype_face(struct freetype_face);

#endif /* HAVE_FT2BUILD_H */

/**
 * \brief Find Freetype fonts and store them in a global GFONT_CAP struct
 * 
 * The directories specified by the global variables searchdirs and 
 * numsearchdirs are recursively scanned to find all Freetype-compatible
 * fonts. Information on all the fonts is then transferred into the global
 * GFONT_CAP struct, fontcap, to be used by the main program.
 **/

void find_freetype_fonts(void)
{
#ifdef HAVE_FT2BUILD_H
    int i;
   
    if (FT_Init_FreeType(&ftlibrary) != 0)
        G_fatal_error("Unable to initialise Freetype");
   
    ftfaces = NULL;
    num_ftfaces = 0;
    memsize = 0;
   
    for (i = 0; i < numsearchdirs; i++)
        find_fonts(searchdirs[i]);

    if ((totalfonts + num_ftfaces) >= maxfonts)
        maxfonts += num_ftfaces;
   
    fontcap = (struct GFONT_CAP *) G_realloc(fontcap, 
		                         maxfonts * sizeof(struct GFONT_CAP));
    for (i = 0; i < num_ftfaces; i++)
    {
        char buf[GPATH_MAX];
        char *buf_ptr;
       
        strncpy(buf, ftfaces[i].path, GPATH_MAX);
        if (strchr(buf, HOST_DIRSEP))
	    buf_ptr = strrchr(buf, HOST_DIRSEP) + 1;
        else
	    buf_ptr = buf;
        if (strchr(buf_ptr, '.'))
            *(strrchr(buf_ptr, '.')) = '\0';
        if (ftfaces[i].index > 0)
            G_asprintf(&fontcap[totalfonts].name, "%s%d", buf_ptr, ftfaces[i].index);
        else
	    fontcap[totalfonts].name = G_store(buf_ptr);
        /* There might not be a style name but there will always be a
	 * family name. */
        if (ftfaces[i].face->style_name == NULL)
	    fontcap[totalfonts].longname = G_store(ftfaces[i].face->family_name);
        else
            G_asprintf(&fontcap[totalfonts].longname, "%s %s", 
		   ftfaces[i].face->family_name, ftfaces[i].face->style_name);
        fontcap[totalfonts].path = G_store(ftfaces[i].path);
        fontcap[totalfonts].index = ftfaces[i].index;
        fontcap[totalfonts].type = GFONT_FREETYPE;
        fontcap[totalfonts].encoding = G_store("utf-8");
        totalfonts++;
       
        free_freetype_face(ftfaces[i]);
    }
   
    FT_Done_FreeType(ftlibrary);
#endif /* HAVE_FT2BUILD_H */
    return;
   
}

#ifdef HAVE_FT2BUILD_H

/**
 * \brief Recursively scans a specified directory and stores information on
 *        all font files found.
 * 
 * Any directories found are recursively scanned in the same way as the 
 * parent. Any files found are attempted to be opened using FT_New_Face()
 * to discover if they are valid font files or not. If they are, information
 * on all the font faces in each file is stored in an array of freetype_face
 * structs. Recursive structure based on lib/init/clean_temp.c by Roberto 
 * Flor.
 * 
 * \param dirpath String containing directory to be scanned
 **/

static void find_fonts(const char *dirpath)
{
    char filepath[GPATH_MAX];
    DIR *curdir;
    struct dirent *cur_entry;
    struct stat info;

    curdir = opendir(dirpath);
    if (curdir == NULL )
	return;

    /* loop over current dir */
    while ((cur_entry = readdir(curdir)))
    {
	if (cur_entry->d_name[0] == '.')
	    continue; /* Skip hidden files */
		
	sprintf(filepath, "%s%c%s", dirpath, HOST_DIRSEP, cur_entry->d_name);

	if (stat(filepath, &info))
	    continue; /* File is unreadable */

	if ( S_ISDIR(info.st_mode)) 
		find_fonts(filepath); /* Recurse into next directory */
	else
        {
	    /* It's a file; we'll try opening it with Freetype to see if
	     * it's a valid font. */
	    FT_Long index, facesinfile;
	    index = facesinfile = 0;
	   
	    do
	    {   			
                if (num_ftfaces >= memsize)
                {
	            memsize += 20;
	            ftfaces = (struct freetype_face *) G_realloc(ftfaces, 
			              memsize * sizeof(struct freetype_face));
                }
	       
	        if (FT_New_Face(ftlibrary, filepath, index, &(ftfaces[num_ftfaces].face)) == 0)
		{		     
	            facesinfile = ftfaces[num_ftfaces].face->num_faces;
	            /* Only use scalable fonts */
	            if (ftfaces[num_ftfaces].face->face_flags & FT_FACE_FLAG_SCALABLE)
		    {		    
		        /* The FT_Face object should contain the index but in
			 * practice it seemed to be always 0 so including it
			 * explicitly here instead */
	            	ftfaces[num_ftfaces].index = index;
	            	ftfaces[num_ftfaces].path = G_store(filepath);
		        num_ftfaces++;
		    }
	            else
		        /* Discard this FT_Face structure and use it again */
		    	FT_Done_Face(ftfaces[num_ftfaces].face);
			
		}
	    } while (++index < facesinfile);
	    
	}       	       
    }
   
    closedir(curdir);
   
    return;
}

/**
 * \brief Free the memory used by a freetype_face struct
 * 
 * \param ftface Struct to have its memory freed
 **/

static void free_freetype_face(struct freetype_face ftface)
{
   FT_Done_Face(ftface.face);
   G_free(ftface.path);
   
   return;
}
	     
#endif /* HAVE_FT2BUILD_H */
