/* Cell-file area extraction */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* December, 1987 */

/* 
** last modified by Dave Gerdes 8 1988
**  cleaned up the options and created dig and dig_att files
*/

#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "extr_areas.h"

int main (int argc, char *argv[])
{
  char *input, *output;
  struct GModule *module;

  G_gisinit(argv[0]);

  module = G_define_module();
  module->description =
	"Extracts area edges from a raster map layer and converts "
	"data to GRASS vector format.";

  if (syntax(argc,argv,&input,&output))
  {
    fprintf(stderr,"Usage:  %s cell_file digit_file\n",argv[0]);
    exit(-1);
  }
#ifdef DEBUG
  freopen("debug.out","w",stdout);
  setbuf(stdout,0);
#endif

  io_init ();
  fprintf(stdout,"Opening files\n");
  open_file(input,output);
  fprintf(stdout,"Performing extraction\n");
  extract_areas();
  fprintf(stdout,"Consolidating area information\n");
  re_map_areas();
  fprintf(stdout,"Closing files\n");
  close_file();
  exit(0);
}

