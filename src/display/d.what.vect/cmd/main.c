/*
 * $Id$
 *
 * attempt to auto-select vector maps displayed in monitor (like d.zoom)
 *
 * d.what.vect
*/

#define MAIN
#include <string.h>
#include "gis.h"
#include "display.h"
#include "Vect.h"
#include "raster.h"
#include "what.h"

/* Vector map grabbing taken from d.zoom */

int main(int argc, char **argv)
{
  struct Flag *once, *terse;
  struct Option *opt1;
  struct GModule *module;
  char *mapset, *openvect();
  char temp[128], *str;
  int i, j, level, width, mwidth;
    
  /* Initialize the GIS calls */
  G_gisinit (argv[0]) ;

  /* have a look if vector maps are already drawn in monitor */
  if (R_open_driver() != 0)
    G_fatal_error ("No graphics device selected");
  
  if(D_get_dig_list (&vect, &nvects) < 0)
	vect = NULL;
  else
    {
	vect = (char **)G_realloc(vect, (nvects+1)*sizeof(char *));
	vect[nvects] = NULL;
    }
  R_close_driver();

  once = G_define_flag();
  once->key = '1';
  once->description = "Identify just one location";
  
  opt1 = G_define_option() ;
  opt1->key        = "map" ;
  opt1->type       = TYPE_STRING ;
  opt1->multiple   = YES;
  if (vect)
          opt1->answers = vect;
  opt1->required   = NO ;
  opt1->gisprompt  = "old,dig,vector" ;
  opt1->description= "Name of existing vector map" ;
  
  terse = G_define_flag();
  terse->key = 't';
  terse->description = "Terse output. For parsing by programs";
  
  module = G_define_module();
  module->description = 
    "Allows the user to interactively query a vector map layer "
    "at user-selected locations within the current geographic region.";

  if(!vect)
    {
    	  opt1->required = YES;
    }
    	  	      
  if((argc > 1 || !vect) && G_parser(argc,argv))
    exit(-1);

  if (opt1->answers && opt1->answers[0])
      vect = opt1->answers;

  /* Look at maps given on command line */

  if(vect)
    {
      for(i=0; vect[i]; i++);
      nvects = i;

      for(i=0; i<nvects; i++)
	{
	  mapset = openvect(vect[i]);
	  if(mapset == NULL)
	    {
              fprintf (stderr, "Unable to open %s\n", vect[i]) ;
              exit(1) ;
	    }
	}
    }

  Map = (struct Map_info *) G_malloc(nvects * sizeof(struct Map_info));
  Cats = (struct Categories *) G_malloc(nvects * sizeof(struct Categories));

  width = mwidth = 0;
  for(i=0; i<nvects; i++)
    {
      str = strchr(vect[i], '@');
      if(str) j = str - vect[i];
      else    j = strlen(vect[i]);
      if(j > width)
        width = j;

      mapset = openvect(vect[i]);
      j = strlen(mapset);
      if(j > mwidth)
        mwidth = j;

      level = Vect_open_old (&Map[i], vect[i], mapset);
      if (level < 0)
        {
	  sprintf(temp, "Vector file [%s] not available", vect[i]);
          G_fatal_error (temp);
	}

      if (level < 2)
        {
	  sprintf(temp, "%s: You must first run v.support on vector file", vect[i]);
          G_fatal_error (temp);
	}

      if (G_read_vector_cats(vect[i], mapset, &Cats[i]) < 0)
        Cats[i].num = -1  ;
    }

  if (R_open_driver() != 0)
    G_fatal_error ("No graphics device selected");
  D_setup(0);

  what(once->answer, terse->answer, width, mwidth); 

  for(i=0; i<nvects; i++)
      Vect_close (&Map[i]);

  R_close_driver();
  R_pad_freelist(vect, nvects);
  
  exit(0);
}



