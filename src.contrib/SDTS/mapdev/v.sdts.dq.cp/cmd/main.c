/************************************************************
*
*     v.sdts.dq
*
*     Usage:
*      v.sdts.dq_cmd [-f] map=name 
		   [HL=name] [PA=name] [AA=name] [LC=name] [CG=name]
*        
*     
*     Flags:
*       -f   force overwriting existing files

*     Parameters:
*          map   vector file associated with these data quality files
*           HL   lineage (HL) data quality input file
*           PA   positional accuracy (PA) data quality input file
*           AA   attribute accuracy (AA) data quality input file
*           LC   logical consistency (LC) data quality input file
*           CG   completeness (CG) data quality input file

*
*        each specified data quality input file, if found, will be
*        copied into the dig_misc/<vector_name>/sdts_meta/

*
*
************************************************************/
#include <string.h>
#include "gis.h"

#define DQ_MODULES 5
#define MAX_FILENAME 11

enum dq_module 
{ lineage, position, attribute, consistency, completeness};

char *dq_prefix[] =
{
  "HL", "PA", "AA", "LC", "CG", NULL
  };

char *dq_name[] =
{
  "Lineage", "Positional Accuracy", "Attribute Accuracy", 
  "Logical Consistency", "Completeness", NULL
  };

struct Option *opt[DQ_MODULES];
struct Flag *flag;
char map_fname [MAX_FILENAME + 1];

main  (argc, argv)
     int argc;
     char **argv;
{
  register struct Option *p_opt, *map_opt;
  enum dq_module i;
  char buf [1000], dq_fname[30];
  char errmsg[1000];
  char current_DQ_path[500];
  char *mapset;

  G_gisinit (argv[0]);

  map_opt = G_define_option ();
  map_opt->key		= "map";
  map_opt->description	= "vector file associated with the data quality files";
  map_opt->required	= YES;
  map_opt->type		= TYPE_STRING;
  map_opt->gisprompt	= "old, dig, vector";

  p_opt = opt[lineage] = G_define_option ();
  p_opt->key		= "HL";
  p_opt->description 	= "lineage (HL) data quality input file";
  p_opt->type 		= TYPE_STRING;

  p_opt = opt[position] = G_define_option ();
  p_opt->key		= "PA";
  p_opt->description 	= "positional accuracy (PA) data quality input file";
  p_opt->type		= TYPE_STRING;

  p_opt = opt[attribute] = G_define_option ();
  p_opt->key		= "AA";
  p_opt->description	= "attribute accuracy (AA) data quality input file";
  p_opt->type 		= TYPE_STRING;

  p_opt = opt[consistency] = G_define_option ();
  p_opt->key		= "LC";
  p_opt->description	= "logical consistency (LC) data quality input file";
  p_opt->type		= TYPE_STRING;
	
  p_opt = opt[completeness] = G_define_option ();
  p_opt->key		= "CG";
  p_opt->description	= "completeness (CG) data quality input file";
  p_opt->type		= TYPE_STRING;

  
  flag = G_define_flag ();
  flag->key		= 'f';
  flag->description	= "force overwriting existing files";


    if (G_parser (argc, argv))
      exit (-1);

  
    if ((mapset = G_find_vector (map_opt->answer, "")) == NULL)
    {
      sprintf (errmsg, "Could not find Vector file %s\n", map_opt->answer);
      G_fatal_error (errmsg);
    }

    if (strcmp (mapset, G_mapset()) != 0)
    {
        sprintf (errmsg, "Currently, only files proper to your mapset can be part of an SDTS transfer.\n   DQ reports for <%s> in <%s> cannot be exported.\n", map_opt->answer, mapset);
        G_fatal_error (errmsg);
    }

	sprintf (current_DQ_path, "dig_misc/%s/sdts_meta",  map_opt->answer);

    G__make_mapset_element (current_DQ_path); 

    for ( i = lineage; i <= completeness ; i++)
    {
      char *filename;

      filename = opt[i]->answer;
      
      if (filename != NULL) /* file name was given on command line */
	  {
	  
	  if (!file_ok (filename))
	  {
		  fprintf (stderr, "'%s' is an invalid file.", filename);
		  continue;
	  }

	  /*DQ filenames = 2-char prefix + '_' + vector_name; e.g. "HL_streams"*/

	    strcpy (dq_fname, dq_prefix[i]);
	    strcat (dq_fname, "_");
	    strncat (dq_fname, map_opt->answer, MAX_FILENAME);

	    if (flag->answer ||
	      !G_find_file (current_DQ_path, dq_fname, G_mapset()))
	    {
	         sprintf (buf,"cp %s %s/%s/%s/%s", filename,
		       G_location_path (), G_mapset (), current_DQ_path, dq_fname);

	        if (!system (buf))
	          fprintf(stderr,"%s Data Quality file for <%s> installed.\n" , dq_name[i],  map_opt->answer);
	    }
	    else
	     fprintf(stderr,"%s Data Quality file for %s already exists.\n  Use overwrite option.\n" , dq_name[i],  map_opt->answer);
	  }
    }
}


/* file_ok: return non-zero = success
*                         0 = failure
*/

#include <sys/stat.h>

file_ok (filename)
   char *filename;
{
   struct stat info;

   if (stat (filename, &info) != 0)
      return 0;

   return (S_ISREG (info.st_mode));

}

