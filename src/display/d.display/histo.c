#include "windows.h"
#include "lproto.h"
#include "raster.h"
#include "gis.h"
#include "D.h"
#include "variables.h"

int histo_plot()
{
	FILE *temp_file ;
	FILE *fptr ;
	char *temp_file_name ;
	char buffer[256] ;
	int min_cat ;
	int max_cat ;
	int max_val ;
	int rng_cat ;
	int cat ;
	int val ;

	if (*mapname == '\0')
	{
		fprintf (stdout,"You must draw a raster map before using this option\n") ;
		do_pause() ;
		return -1;
	}

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	Dchoose(LO3.name) ;
	Derase("black") ;
	R_close_driver();

/* Run r.stats on the current displayed map */
	temp_file_name = G_tempfile() ;
	sprintf(buffer, "-cv '%s@%s' > %s", mapname, mapset, temp_file_name) ;
	gorun("r.stats", buffer) ;

/* Read through temp file to find max category and area numbers */
	temp_file = fopen(temp_file_name, "r") ;
	min_cat = 0 ;
	max_cat = 0 ;
	max_val = 0 ;
	while(NULL != fgets(buffer, 80, temp_file))
	{
		sscanf(buffer,"%d:%d", &cat, &val) ;
		if (cat > max_cat) max_cat = cat ;
		if (val > max_val) max_val = val ;
		if (cat < min_cat) min_cat = cat ;
	}
	rng_cat = max_cat - min_cat ;
	fclose(temp_file) ;
	if (rng_cat == 0 || max_val == 0)
		return(-1) ;

/* Read through temp file plotting the histogram */
	if (NULL != (fptr = popen("d.graph", "w")))
	{
		fprintf(fptr, "color white\n") ;
		fprintf(fptr, "move  5 10\n") ;
		fprintf(fptr, "draw  5 90\n") ;
		fprintf(fptr, "draw 95 90\n") ;
		fprintf(fptr, "draw 95 10\n") ;
		fprintf(fptr, "draw  5 10\n") ;
		fprintf(fptr, "move  1  2\n") ;
		fprintf(fptr, "size 4. 8.\n") ;
		fprintf(fptr, "text 0\n") ;
		fprintf(fptr, "move  1 92\n") ;
		fprintf(fptr, "text %d\n", max_val) ;
		fprintf(fptr, "color green\n") ;

		temp_file = fopen(temp_file_name, "r") ;
		while(NULL != fgets(buffer, 80, temp_file))
		{
			sscanf(buffer,"%d:%d", &cat, &val) ;
			fprintf(fptr, "move %d %d\n",
				10 + (80 * (cat - min_cat) / rng_cat), 10) ;
			fprintf(fptr, "draw %d %d\n",
				10 + (80 * (cat - min_cat) / rng_cat),
				10 + (80 * val / max_val) ) ;
		}
		fclose(temp_file) ;
		pclose(fptr) ;
	}

	return 0;
}
