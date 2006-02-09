#include <grass/gis.h>
#include "globals.h"

 
/*****************************************************************************/
void openfiles (CELL *rowbuf[3])
{
  long             band;	/* loop index counting the bands            */
  char tempstr[100];
  char *mapset;
 
  /* open input files */
  for (band=0; band<NBANDS; band++) {
    if ((mapset = G_find_cell(inputfiles[band], "")) == NULL) {
      sprintf(tempstr, "Unable to find input cell map <%s>.", 
	      inputfiles[band]);
      G_fatal_error(tempstr);
    }
    if ((fd_input[band] = G_open_cell_old(inputfiles[band],mapset)) < 0)
      G_fatal_error("Error in opening input file");
  }
 
  /* open output files */
  for (band=0; band<NBANDS; band++) {
    if ((fd_output[band] = G_open_cell_new(outputfiles[band])) < 0)
      G_fatal_error("Error in opening output file");
  }
 
  /* allocate the cell row buffer */
  for (band=0; band<NBANDS; band++) {
    if ((rowbuf[band]=G_allocate_cell_buf()) == NULL)
      G_fatal_error("Unable to allocate the input row buffer");
  }

}
 
