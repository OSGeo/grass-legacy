/*****************************************************************************
Read the data output is 16-bit depth
******************************************************************************/
#include "dev.h"
#include "gis.h"


dataread (infile, outfile, eindat, datapo)
char	eindat[];
struct header *datapo;
FILE *infile;
int outfile;
{
	register int	jj, i, j;
	register signed short *inputbuf;
	int	elev;
	int	lat, lon, rs, dbc;
        CELL *cell, **buffer;
        
	/*get space for data */
	if ((inputbuf = (signed short * )calloc(datapo->col, sizeof(signed short))) == 0)
		err ("Can't allocate core...\n");

	cell = G_allocate_cell_buf();
        buffer = (CELL **)G_malloc(datapo->row * sizeof(CELL *));
	for(i = 0; i < datapo->row; i++)
	   buffer[i] = (CELL *)G_malloc(datapo->col * sizeof(CELL));
	    
	printf ("Reading data record of %s\n", eindat);
	printf ("\nThe data set is 16-bit...no compression!\n");

	/*here loop to read the real elevation data. with 1"x1" resolution, there are 
          1201 records each with 721 elevation datas. */

	
	fprintf(stderr,"Reading data from %s\n",eindat);
	
	
	for (jj = 0 ; jj < datapo ->row ; jj++) {

        
		if (fread (&rs, 1, 1, infile) == NULL)
			err ("ERROR reading recognition sentinel\n");

		if (fread (&dbc, 3, 1, infile) == NULL)
			err ("ERROR reading data block count\n");

		if (fread (&lon, 2, 1, infile) == NULL)
			err ("ERROR reading longitude\n");

		if (fread (&lat, 2, 1, infile) == NULL)
			err ("ERROR reading latitude\n");


		/*here the loop for the elevations of one latitude*/

		if (fread (inputbuf, sizeof(signed short) * datapo->col, 1, infile) == NULL)
			err ("ERROR reading elevation data\n");
			
	        for(i = 0; i < datapo->col; i++)
	           buffer[jj][i] = inputbuf[i];
	           
		
		if (feof(infile)) {
			printf("End of input file reached\n");
			break;    
		}

		/*this is the  32-bit checksum of the elevation data*/

		if (fread (&elev, 4, 1, infile ) == NULL)
			err ("ERROR reading 4-bit elev data\n");

	}/*end of for*/
	
        fprintf(stderr,"Writing data...\n");
	for(i = datapo->col; i > 0; i--){
	   for(j = 0; j < datapo->row; j++)
	      cell[j] = buffer[j][i-1];
	      
	   G_put_map_row(outfile,cell);		
        }

}


