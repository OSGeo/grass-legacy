/****************************************************************************
Reading milgeo elevation data 16-bit, output can be  according to the input line 
of the user SRF or DATA block elevation date. If the user wants, the data 
was reduced to 8 bit and rotated about 270 degrees to get the right location.
Also 2 files with information about the data will be created:
a file called input".dsi" with the geographic coordinates of these datas and
a file input".acc" with the accuracy of the elevation data listed in headers 
files on the milgeo-tape.

date of creation: 25.05.1993
update: 21.07.1993

responsible person: Thomas Huegel UNI/BW - BauV6.2.
                    Tel.: 089/6004-3491


****************************************************************************/

#include "dev.h"
#include "gis.h"

FILE *infile;
int outfile;

int	compressflag, rotationflag;
int	srfflag, lanflag, dataflag;
int	pixheader[8];

/* called subroutines */
int	dsiread ();
int	accread ();
int	dataread ();
void getcornercoor();
int	atoint();
void err();

main (argc, argv)
int	argc;
char	*argv[];
{
	int	error;
	char	c = 'Y';
	char	eindat[MAXLENGTH];
	struct Cell_head region;

	struct header lanhead;

	if (argc != 2) {
		printf ("WRONG USAGE\n");
		err ("Try it with dhmread infile [options]\n");
	}
        
        G_gisinit(argv[0]);

	compressflag = FALSE;
	rotationflag = FALSE;
	srfflag = FALSE;
	lanflag = FALSE;
	dataflag = FALSE;

	strcpy (eindat, argv[1]);

	if ((infile = fopen(eindat, "r")) == NULL)
		err ("Can't open input data file\n");

        
	if ((error = dsiread(infile, eindat, &lanhead)) == -1) {
		printf ("ERROR reading DSI-Header!!  Continue ? (Y/N):   ");
		c = getchar(); 					     
	} /* end of if */

	if (toupper(c) != 'Y') {
		printf ("OK break program run\n");
		err ("Call TOM to check the error. TEL. 089 / 6004-3491\n");
	}

	if ((error = accread(infile, eindat)) == -1) {
		printf ("ERROR reading ACC-Header!! Continue ? (Y/N):   ");
		c = getchar(); 					     
	} /* end of if */

	if (toupper(c) != 'Y') {
		printf ("OK break program run\n");
		err ("Call TOM to check the error. TEL. 089 / 6004-3491\n");
	}

        G_get_set_window(&region);

	fprintf(stderr,"N: %lf\nS: %lf\nW: %lf\nE: %lf\n",region.north,
	        	region.south,region.west,region.east);
        fprintf(stderr,"G_rows: %d\nG_cols: %d\n",region.rows,region.cols);


	if ((outfile=G_open_cell_new("tmp.dem")) < 1)
		err("Error open GRASS file\n");
	
	if ((error = dataread(infile, outfile, eindat, &lanhead)) == -1)
		err ("ERROR reading data\n");
	else
		printf ("DATA read well done\n");

	if (error == -1)  {
		printf ("OK break program run\n");
		err ("Call TOM to check the error. TEL. 089 / 6004-3491\n");
	}  /* 17 end of if */

	fclose(infile);
        fprintf(stderr,"Closing cell file...\n"); 
        G_close_cell(outfile);
        fprintf(stderr,"Closing cell file done\n");
        if (error == 0)
		printf ("\n\nProgram runs ok\n");
} /* end of main */


