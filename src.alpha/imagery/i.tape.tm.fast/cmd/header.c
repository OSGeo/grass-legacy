/****************************************************************/
/* NAME:	header						*/
/*								*/
/* FUNCTION:	read in parameters from the header file		*/
/*								*/
/* USAGE:	header(vol_number_only)				*/
/* 								*/
/* INPUT:	int vol_number_only				*/
/*								*/
/* OUTPUT:	none						*/
/****************************************************************/
#include "tape.h"

#define debug stderr

/* read the volume directory file */

header(vol_number_only)
{
    char *header_item();
    char *version;
    int b;
    int bnd_exist;

    version = header_item(1,2);

    if ( strcmp(version,"SC") == 0 ) {
      text_rec_90();
      tape.nrows = atoi(header_item(1109,1112));
      tape.ncols = atoi(header_item(1087,1090));
    }
    else if ( strcmp(version,"PR") == 0) {
      text_rec_91();
      tape.nrows = atoi(header_item(1108,1112));
      tape.ncols = atoi(header_item(1086,1090));
      tape.vol = atoi(header_item(439,439)); /* vol number of the image spans */
    }
    else {  
      fprintf(stderr, "\nWARNING: Format not known !!!!!!!!\n");
      exit(0);
    }

    tape.nbands = 0;
    tape.interleaving = BSQ;

/* Initializing the band presentation */
    for (b = 0; b < THEMATIC_MAPPER_NBANDS; b++)
      tape.bnd_present[b] = 0;

    for (b = 0; b < THEMATIC_MAPPER_NBANDS; b++) {
      bnd_exist = atoi(header_item(1361+b,1361+b));
      if (bnd_exist >= 1 && bnd_exist <= 7) {
        tape.nbands++;
        tape.bnd_present[bnd_exist-1] = 1;                          
	tape.band[bnd_exist-1].vol = tape.vol;
      }
    }
    
    if (vol_number_only)
	return;
    tape.nvols = atoi(header_item(441,441)); /* no. of vols the image spans */

/* EROS only provides TM tape in BSQ format */

    tape.interleaving = BSQ;
    tape.tapebufsize = atoi(header_item(1406,1410));
    tape.blocking_factor = atoi(header_item(1386,1389));

    for ( b = 0; b < THEMATIC_MAPPER_NBANDS; b++ ) 
    {
	if (tape.bnd_present[b])
	    tape.band[b].nrows = tape.nrows;
	    tape.band[b].ncols = tape.ncols;
    }
}
