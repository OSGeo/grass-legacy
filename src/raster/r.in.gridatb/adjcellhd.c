#include "local_proto.h"


int
adjcellhd(cellhd)
	struct	Cell_head	*cellhd;
{
	int	retval;


	retval = 0;

	if(G_set_window(cellhd) < 0){
		fprintf(stderr, "\n** ERROR: setting window header **\n");
		retval = 1;
	}

	if(cellhd->rows != G_window_rows()){
		fprintf(stderr, "\n** ERROR: rows changed **\n");
		retval = 1;
	}

	if(cellhd->cols != G_window_cols()){
		fprintf(stderr, "\n** ERROR: cols changed **\n");
		retval = 1;
	}


	return(retval);
}

