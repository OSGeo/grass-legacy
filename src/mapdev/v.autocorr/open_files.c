#include	<stdio.h>
#include <string.h>
#include	"gis.h"
#include	"Vect.h"

#define		DIG_DIR		"dig"
#define		PLUS_DIR	"dig_plus"
#define		ATT_DIR		"dig_att"

int 
open_dig_files (char *name, FILE **fp_plus, struct Map_info *Map, struct Plus_head *Plus)
{

  static char dig_file[128];
  static char dig_plus_file[128];
  static char att_file[128];
  static char *mapset;
  char errmsg[80];

  mapset = G_store (G_mapset ());

  /* check for existance of support directories  */
  G__make_mapset_element (PLUS_DIR);
  G__make_mapset_element (ATT_DIR);

  /* get the full path to files */
  G__file_name (dig_file, DIG_DIR, name, mapset);
  G__file_name (att_file, ATT_DIR, name, mapset);
  G__file_name (dig_plus_file, PLUS_DIR, name, mapset);
  strncpy (Plus->Dig_name, name, HEADSTR);


  /* Unsupported library routine */
  /* Kids, don't try this at home. */
  /*
   * if (0 > V2__init_for_create_plus (Map, name)) { G_fatal_error ("Cannot
   * open input file"); }
   * 
   */

  /**************************************************************************/

  if ((*fp_plus = fopen (dig_plus_file, "r")) == NULL)
    G_fatal_error ("Can't open dig_plus_file");

  if ((Map->att_fp = fopen (att_file, "r+")) == NULL)
    G_fatal_error ("Cannot open attribute file");

  /* Map->digit_file = dig_file; */
  Map->plus_file = dig_plus_file;
  Map->att_file = att_file;

  return (0);
}
