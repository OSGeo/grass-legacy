/* Main Routine, Initializes GRASS environment and parses the Command-Line */

#include "string.h"
#include "gis.h"
#include "tools.h"

extern int G_fatal_error(char*);
extern int G_get_cellhd(char*,char*,struct Cell_head*);
extern int G_set_window(struct Cell_head*);

int main_c_ ( cargc, cargv)
char *cargv;
int  *cargc;
{
	extern aggreg_ ();

	int maxcol, maxrow, i, j, umgebung;
	
	int *matrix1, *matrix2;
	char *cmatrix;

	char *mapset, *filename, *outfile, *ofile2;
	char msg[100];
        struct Cell_head window;

	Filenames parse_answer;

	char **gargv;
	char *space;

	/* Kommandozeile nach Parametern abfragen und GIS Library */
	/* Initialisieren */

	gargv = (char**) calloc(*cargc, sizeof(char*) );
	for (i=0; i<(*cargc); i++) {
	  *(cargv+( (i+1)*200 )-1) = '\0';
	  gargv[i] = cargv + (i*200);
	  space = (char*) index( gargv[i], ' ');
	  *space = '\0';
	  
	}

	parse_answer = init_and_parse( *cargc, gargv);
        filename = parse_answer.input;
	outfile = parse_answer.output;
	ofile2 = parse_answer.neibordat;
	umgebung = parse_answer.neighborhood;

	/* Check for existence of raster map */
	if ( (mapset = G_find_cell2 (filename, "")) == NULL) {
		sprintf (msg,"%s: [%s] not found", G_program_name(), filename);
		G_fatal_error (msg);
		exit(1);
	};


/* Get file header information and set region */

/*	if (G_get_cellhd (filename, mapset, &window) < 0)
	{
	    sprintf (msg, "can't get cell header for [%s] in [%s]",filename,mapset);
	    G_fatal_error (msg);
	}
	G_set_window (&window); */

	G_get_window (&window); 

	maxrow = window.rows;
	maxcol = window.cols;


	/* Anfordern des Speichers fuer die Felder AGGMAT, KLAMAT und TESTED */

	matrix1 = (int*) calloc ( (maxrow+2) * (maxcol+2), sizeof(int) );
	matrix2 = (int*) calloc ( (maxrow+2) * (maxcol+2), sizeof(int) );
	cmatrix = (char*) calloc ( (maxrow+2)* (maxcol+2), 1 );


	/* Initialisieren der Felder */
	/* KLAMAT WIRD AUF -888 GESETZT UM ZU VERHINDERN DAS RAHMEN-PIXELS */
        /* EINEM AGG_AREA ZUGEORDNET WERDEN */

	for (i=0; i < ((maxrow+2)*(maxcol+2)); i++)  matrix2[i] = -888;


	aggreg_ ( &maxcol, &maxrow, filename, mapset, outfile, ofile2,
		  matrix1, matrix2, cmatrix, &umgebung );

	return 0;
}

