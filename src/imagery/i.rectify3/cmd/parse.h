/*======================================================================
Filename:   parse.h
Module:	    i.rectify3 (cmd)
Author:	    Mike Baba

Modifications:
30 Oct 93    - mbaba       - original 
======================================================================*/

#ifndef _parse_h
#define _parse_h


typedef struct {
   char *group;	             /* Group specified. */

   char *source_name;        /* name of image to be rectified (from) */
   char *target_name;        /* name of the target (resultant) file (to) */
   char *target_mapset;      /* name of the target mapset */

   char *north;             /* target window region */
   char *south;
   char *east;
   char *west;
   char *nsres;
   char *ewres;
   char *res;

   short overwrite;  	     /* True if -o flag spccified. */
   short current;  	     /* True if -c flag specified. */
   short minimal;    	     /* True if -m flag specified. */
   short quiet;	      	     /* True if -q flag specified. */
   short verbose;	     /* True if -v flag specified. */

} tRect_Data;

char *Parse( int, char **, tRect_Data   *);
int check_files(struct Ref, tRect_Data *);
int check_window(tRect_Data *);
int get_window(struct Ref, tRect_Data *);
#endif

