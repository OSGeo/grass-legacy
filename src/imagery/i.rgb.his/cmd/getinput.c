
#include "gis.h"
#include "globals.h"

 
/*****************************************************************************/
void getinput()
{
  long             band;	/* loop index counting the bands            */
  unsigned char    *temp_thebuf; /* temporary pointer to input buffer buf_in */
  char             command[7];	/* string to specify the command            */
  char             input_file[20]; /* variable for specifying input file    */
  char             output_file[20]; /* variable for specifying ouput file    */
  char tempstr[100];
  char *mapset;
  int i,j;
  CELL *rowbuf;
 
  /* get number of bands */
  Nbands = 3;
 
  /* get dimension of the image */
  Dim.row = G_window_rows();
  Dim.column = G_window_cols();

  /* open input files */
  for (band=0; band<Nbands; band++) {
    if ((mapset = G_find_cell(inputfiles[band], "")) == NULL) {
      sprintf(tempstr, "Unable to find input cell map <%s>.", 
	      inputfiles[band]);
      G_fatal_error(tempstr);
    }
    if ((fd_input[band] = G_open_cell_old(inputfiles[band],mapset)) < 0)
      G_fatal_error("Error in opening input file");
  }
 
  /* open output files */
  for (band=0; band<Nbands; band++) {
    if ((fd_output[band] = G_open_cell_new(outputfiles[band])) < 0)
      G_fatal_error("Error in opening output file");
  }
 
  /* allocate the cell row buffer */
  if ((rowbuf=G_allocate_cell_buf()) == NULL)
    G_fatal_error("Unable to allocate the input row buffer");

  /* allocate dynamic space for output storage */
  if ((buf_out = (unsigned char *) G_calloc((Nbands*Dim.row*Dim.column+1),
					    sizeof(char))) == NULL)
    G_fatal_error("Error allocating dynamic storage for output buffer\n");
 
  /* allocate dynamic space for input storage and read input files */
  if ((buf_in = (unsigned char *) G_calloc((Nbands*Dim.row*Dim.column+1),
					   sizeof(char))) == NULL)
    G_fatal_error("Error allocating dynamic memory for I/O manipulation\n");
  else {
    temp_thebuf = buf_in;
    for (band=0; band<Nbands; band++) {
      offsetI[band] = temp_thebuf;
      i=0;
      for (i=0; i<Dim.row; i++) {
	if(G_get_map_row(fd_input[band], rowbuf, i) < 0)
	  G_fatal_error("Error while reading cell map.");
	for (j=0; j<Dim.column; j++) {
	  *temp_thebuf = (unsigned char) rowbuf[j];
	  temp_thebuf++;
	}
      }
/*      while(i<(Dim.row*Dim.column)) {
	*temp_thebuf = (unsigned char) getc(fp_input[band]);
	temp_thebuf++;
	i++;
      }
*/
    }
  }
  
  free(rowbuf);
}
 
