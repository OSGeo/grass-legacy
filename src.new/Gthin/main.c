/* Cell-file line thinning */

/* Mike Baba */
/* DBA Systems */
/* Fairfax, Va */
/* Jan 1990 */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* January - February 1988 */

#include <stdio.h>
#include "gis.h"

char *error_prefix;

main(argc,argv)
int argc;
char *argv[];
{
  char *input, *output;

  if (argc != 3)
  {
    fprintf(stderr,"Usage:  %s input_cell_file thin_cell_file\n",argv[0]);
    exit(-1);
  }
  input = argv[1];
  output = argv[2];
  error_prefix = argv[0];
  G_gisinit(argv[0]);
  open_file(input);
  thin_lines();
  close_file(output);
  exit(0);
}

