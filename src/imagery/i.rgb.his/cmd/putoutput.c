#include "gis.h"
#include "globals.h"


int 
putoutput (void)
{

  int i,j;
  CELL *rowbuf;
  int band;
  unsigned char *tempbuf;
  char tempstr[100];

  if ((rowbuf=G_allocate_cell_buf()) == NULL)
    G_fatal_error("Unable to allocate cell map buffer.");

  for (band=0; band<Nbands; band++) {
    tempbuf = offsetO[band];
    for (i=0; i<Dim.row; i++) {
      for (j=0; j<Dim.column; j++) {
	rowbuf[j] = (CELL) *tempbuf;
	tempbuf++;
      }
      G_put_raster_row(fd_output[band], rowbuf, CELL_TYPE);
    }
    G_close_cell(fd_output[band]);
    sprintf(tempstr, "Gcell.colors %s grey", outputfiles[band]);
    system(tempstr);
  }
  free(rowbuf);

}
