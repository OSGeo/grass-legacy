/****************************************************************************
 *
 * MODULE:       GRASS GIS library - ls.c
 * AUTHOR(S):    Paul Kelly
 * PURPOSE:      Functions to list the files in a directory.
 * COPYRIGHT:    (C) 2007 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>

#include <grass/gis.h>
#include <grass/config.h>

#ifdef HAVE_SYS_IOCTL_H
#  include <sys/ioctl.h>
#endif


static int cmp_names(const void *aa, const void *bb)
{
   char * const *a = aa;
   char * const *b = bb;
   
   return strcmp(*a, *b);
}

/**
 * \brief Stores a sorted directory listing in an array
 * 
 * The filenames in the specified directory are stored in an array of
 * strings, then sorted alphabetically.
 * 
 * 
 * \param dir       Directory to list
 * \param num_files Pointer to an integer in which the total number of
 *                  files listed will be stored
 * 
 * \return          Pointer to array of strings containing the listing
 **/

char **G__ls(const char *dir, int *num_files)
{
    struct dirent *dp;
    DIR *dfd;
    char **dir_listing = NULL;
    int n = 0;

    if ((dfd = opendir(dir)) == NULL)
       G_fatal_error("Can't open directory %s", dir);

    while ((dp = readdir(dfd)) != NULL)
    {
       if(dp->d_name[0] != '.') /* Don't list hidden files */
       {
          dir_listing = (char **)G_realloc(dir_listing, 
					   (1 + n) * sizeof(char *));
          dir_listing[n] = G_store(dp->d_name);
          n++;
       }
    }

    /* Sort list of filenames alphabetically */
    qsort(dir_listing, n, sizeof(char *), cmp_names);
   
    *num_files = n;
    return dir_listing;
}

/**
 * \brief Prints a directory listing to a stream, in prettified column format
 * 
 * A replacement for system("ls -C"). Lists the contents of the directory
 * specified to the given stream, e.g. stderr. Tries to determine an 
 * appropriate column width to keep the number of lines used to a minimum
 * and look pretty on the screen.
 * 
 * \param dir    Directory to list
 * \param stream Stream to print listing to
 **/

void G_ls(const char *dir, FILE *stream)
{
    int i, n;
    char **dir_listing;
   
    int perline;
    int field_width, max_len = 0;
    int screen_width = 80; /* Default width of 80 columns */
   
    dir_listing = G__ls(dir, &n);

#ifdef TIOCGWINSZ
    /* Determine screen_width if possible */
    {	
        struct winsize size;

        if (ioctl(fileno(stream), TIOCGWINSZ, (char *) &size) == 0)
	    screen_width = size.ws_col;
    }   
#endif       

    for (i=0; i < n; i++)
    {	
        /* Find maximum filename length */
        if (strlen(dir_listing[i]) > max_len)
            max_len = strlen(dir_listing[i]);
    }
    
    /* Num filenames that will fit per line (+1 because of space after name) */
    perline = screen_width / (max_len + 1);
    /* Field width to accomodate longest filename */
    field_width = screen_width / perline;

    for (i=0; i < n; i++)
        /* Print filenames in left-justified fixed-width fields, adding
	 * a newline after every 'perline' names */
        fprintf(stream, "%-*s%s", field_width, dir_listing[i], 
		                  (i + 1) % perline? "" : "\n");

    if (n % perline)
        /* Closing newline required */
        fprintf(stream, "\n");
   
    return;
}
