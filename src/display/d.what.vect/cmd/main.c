#define MAIN
#include "gis.h"
#include "display.h"
#include "Vect.h"
#include "raster.h"
#include "whatvect.h"


int main(int argc, char **argv)
{
  struct Flag *once, *terse;
  struct Option *opt1;
  struct GModule *module;
  char *name, *mapset, *openvect();
  struct Map_info Map;
  struct Categories Cats;
  int level;
  
  /* Initialize the GIS calls */
  G_gisinit (argv[0]) ;
  
  once = G_define_flag();
  once->key = '1';
  once->description = "Identify just one location";
  
  opt1 = G_define_option() ;
  opt1->key        = "map" ;
  opt1->type       = TYPE_STRING ;
  opt1->required   = YES ;
  opt1->gisprompt  = "old,dig,vector" ;
  opt1->description= "Name of existing vector map" ;
  
  terse = G_define_flag();
  terse->key = 't';
  terse->description = "Terse output. For parsing by programs";
  
  module = G_define_module();
  module->description = 
    "Allows the user to interactively query a vector map layer "
    "at user-selected locations within the current geographic region.";

  if(G_parser(argc,argv))
    exit(-1);

  name = opt1->answer;

  /* Look at maps given on command line */

  mapset = openvect (name);
  if (mapset == NULL)
    {
      fprintf (stderr, "Unable to open %s\n", name) ;
      exit(1) ;
    }
  

  if (R_open_driver() != 0)
    G_fatal_error ("No graphics device selected");
  D_setup(0);

  level = Vect_open_old (&Map, name, mapset);
  if (level < 0)
    G_fatal_error ("Can't open vector file");

  if (level < 2)
    G_fatal_error ("You must first run v.support on vector file");
  
  if (G_read_vector_cats(name, mapset, &Cats) < 0)
    Cats.num = -1  ;
  
  what(once->answer, terse->answer, &Map, &Cats); 
  R_close_driver();
  Vect_close (&Map);

  exit(0);
}



