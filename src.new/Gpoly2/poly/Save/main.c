/* Cell-file area extraction */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* December, 1987 */

#include <stdio.h>
#include "gis.h"
#include "extr_areas.h"


main(argc,argv)
int argc;
char *argv[];
{
  char *input, *output;

  if (syntax(argc,argv,&input,&output))
  {
    fprintf(stderr,"Usage:  %s cell_file digit_file [-aAbl]\n",argv[0]);
    exit(-1);
  }
#ifdef DEBUG
  freopen("debug.out","w",stdout);
  setbuf(stdout,0);
#endif
  G_gisinit("AREA_EXTRACTION");
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

