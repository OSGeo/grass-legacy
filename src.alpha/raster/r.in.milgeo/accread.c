/*****************************************************************************
Read the ACC-Header
*****************************************************************************/
#include "dev.h"

accread (infile, eindat)
char	eindat[];
FILE *infile;

{
	FILE * accfile;
	int	ii;
	char	acc[ACCHEAD], help[MAXLENGTH];
	char	aha[5], ava[5], ppv[5], pph[5];
	char	*po;

	printf("Reading Accuracy description record of %s\n\n", eindat);
	printf ("Creating output file %s.acc\n\n", eindat);

	strcpy(help, eindat);
	strcat(help, ".acc");

	for (ii = 0; ii < ACCHEAD ; ii++)
		fscanf (infile, "%c", &acc[ii]);

	if ((accfile = fopen(help, "w+")) != NULL) {
		fprintf (accfile, "Accuracy information about the elevation data set: %s\n\n", eindat);
		po = &acc[3];
		for (ii = 0; ii < 4; ii++)
			aha[ii] = *(po + ii);
		po += ii;
		for (ii = 0; ii < 4; ii++)
			ava[ii] = *(po + ii);
		po += ii;
		for (ii = 0; ii < 4; ii++)
			ppv[ii] = *(po + ii);
		po += ii;
		for (ii = 0; ii < 4; ii++)
			pph[ii] = *(po + ii);
		fprintf(accfile, "Absolute horizontal accuracy: %s [m]\n", aha);
		fprintf(accfile, "Absolute vertical accuracy: %s [m]\n", ava);
		fprintf(accfile, "Point to point vertical accuracy: %s [m]\n", ppv);
		fprintf(accfile, "Point to point horizontal accuracy: %s [m]\n", pph);
		fclose (accfile); 
	} else
		err ("ERROR opening ACC-Header file");

}


