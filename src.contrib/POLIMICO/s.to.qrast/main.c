/*
 * main.c
 * 
 * s.to.qrast
 * 
 * V 1.1 - 29/05/02
 * 
 * Politecnico di Milano - Facolta' di Como
 * U. Longoni - M. Cannata
 */

#include <stdio.h>
#include <gis.h>
#include <string.h>
#include <math.h>

#include <unistd.h>
#include <stdlib.h>
#include "gis.h"

#include "site.h"
#include "nrutil0.h"

/* definizione dei parametri per dimensionare la sottoarea da eleborare */
#define true 1
#define false 0
#define LUNG 50
#define lato 2000

#define NR_END 1
#define FREE_ARG char*

FILE *finput;
Site *orig;

struct Option *input, *raster, *type, *findex;
struct Cell_head reg_orig, reg_elab;

struct elemento
{
    double valore;
    double scarto;
    int freq;
};

struct elemento **rast_matrix;

char *output;
char num_char[10], nomesite[LUNG], nomemapset[LUNG], tt[LUNG], in_patch[1024];
char pippo[LUNG], pluto[LUNG];
int dims, cat, strs, dbls;	//variabili per descrizione dei sites
int lung, j, i, s;		//variabili per ricerca del nome site file
int num = 0, num_index, scan_int;
int x, y, row, col, last_row, last_col;
double v, sqm_tmp;
void *rast, *raster_buf;
int cf;

struct elemento **structMatrix(long nrl, long nrh, long ncl, long nch);
void free_structMatrix(struct elemento **m, long nrl, long nrh, long ncl,
		       long nch);

int main(int argc, char *argv[])
{

    G_gisinit(argv[0]);

/*********************/
    /* ACQUISIZIONE DATI */

/*********************/

    input = G_define_option();
    input->key = "input";
    input->type = TYPE_STRING;
    input->required = YES;
    input->description = "input observation sites list file name";
    input->gisprompt = "old,site_lists,sites";

    raster = G_define_option();
    raster->key = "rast";
    raster->type = TYPE_STRING;
    raster->required = YES;
    raster->description = "output raster file name";
    raster->gisprompt = "new,site_lists,sites";

    type = G_define_option();
    type->key = "type";
    type->type = TYPE_STRING;
    type->required = NO;
    type->description = "raster type";
    type->options = "min,max,mean,sqm";
    type->answer = "mean";

    findex = G_define_option();
    findex->key = "findex";
    findex->type = TYPE_INTEGER;
    findex->required = NO;
    findex->description = "Attribute field number to use for operations";
    findex->answer = "0";

    if (G_parser(argc, argv))
	exit(1);

    scan_int = sscanf(findex->answer, "%d", &num_index);

    if ((scan_int <= 0) || num_index < 0) {

	char msg[256];
	sprintf(msg,
		"%s: \"%s\" is an incorrect value for attribute field number.\n",
		G_program_name(), findex->answer);
	G_fatal_error(msg);
	exit(1);
    }


/********************************************************************************************************************/
    /* APERTURA DEL SITES FILE DI INPUT E DI OUTPUT */


    /* se il sitefile è in un altro mapset diverso da quello corrente questo è indicato nel nome (sitename@mapset) */
    lung = strlen(input->answer);
    j = 0;

    /* estraggo il nome del sitefile */
    while ((j < lung) && (input->answer[j] != '@')) {
	nomesite[j] = input->answer[j];
	j++;
    }
    nomesite[j] = '\0';

    /* estraggo il mapset di appartenenza */
    if (input->answer[j] == '@') {
	j++;
	i = 0;
	while (j < lung) {
	    nomemapset[i] = input->answer[j];
	    j++;
	    i++;
	}
	nomemapset[i] = '\0';
    }

    /* assegno mapset corrente */
    else
	strcpy(nomemapset, G_mapset());

    /* apertura del file sites dei punti da interpolare(INPUT) */
    if ((finput = G_sites_open_old(nomesite, nomemapset)) == NULL) {
	printf("\nIl file indicato non è stato trovato.\n");
	exit(1);
    }

    /* controlla esattezza del file site: prima se il formato è tipico di grass e poi se il formato  */
    if (G_site_describe(finput, &dims, &cat, &strs, &dbls) != 0) {
	printf("\nErrore nelle lettura del file di ingresso\n");
	exit(1);
    }

    //fprintf(stdout,"dims=%d strs=%d dbls=%d cat=%d\n",dims,strs,dbls,cat); //CCCCCC

    orig = G_site_new_struct(cat, dims, strs, dbls);

/*********************************************************************************************************/
    /* ACQUISIZIONE REGIONE CORRENTE */
    G_get_window(&reg_orig);

/*********************************************************************************************************/

    /*assegna i parametri fissi delle sottoregioni da utilizzzare per elaborazione */
    reg_elab.ew_res = reg_orig.ew_res;
    reg_elab.ns_res = reg_orig.ns_res;
    reg_elab.format = reg_orig.format;
    reg_elab.compressed = reg_orig.compressed;
    reg_elab.proj = reg_orig.proj;
    reg_elab.zone = reg_orig.zone;

    reg_elab.rows = reg_orig.rows;
    reg_elab.cols = reg_orig.cols;

/***********************************************************************************************************/
    /* SELEZIONE DELLE SOTTOAREE */
    reg_elab.south = reg_orig.north;
    last_row = false;

    while (last_row == false) {

	reg_elab.east = reg_orig.west;

	reg_elab.north = reg_elab.south;
	reg_elab.south = reg_elab.north - lato * reg_elab.ns_res;

	if (reg_elab.north > reg_orig.north) {
	    reg_elab.north = reg_orig.north;
	    reg_elab.south = reg_elab.north - lato * reg_elab.ns_res;
	}
	if (reg_elab.south <= reg_orig.south) {
	    reg_elab.south = reg_orig.south;
	    last_row = true;
	}

	last_col = false;
	while (last_col == false) {
	    reg_elab.west = reg_elab.east;
	    reg_elab.east = reg_elab.west + lato * reg_elab.ew_res;

	    if (reg_elab.west < reg_orig.west) {
		reg_elab.west = reg_orig.west;
		reg_elab.east = reg_elab.west + lato * reg_elab.ew_res;
	    }

	    if (reg_elab.east >= reg_orig.east) {
		reg_elab.east = reg_orig.east;
		last_col = true;
	    }

		/************************************************************************************************/
	    /* TRASFERIMENTO SITE NELLA MATRICE */

	    reg_elab.rows =
		(int)((reg_elab.north - reg_elab.south) / reg_elab.ns_res);
	    reg_elab.cols =
		(int)((reg_elab.east - reg_elab.west) / reg_elab.ew_res);

	    rast_matrix = structMatrix(0, reg_elab.rows, 0, reg_elab.cols);

	    for (row = 0; row < reg_elab.rows; row++) {
		for (col = 0; col < reg_elab.cols; col++) {

		    rast_matrix[row][col].valore = 0;
		    rast_matrix[row][col].scarto = 0;
		    rast_matrix[row][col].freq = 0;
		}
	    }

	    //lettura site in region
	    fseek(finput, 0L, SEEK_SET);

	    while (G_site_get(finput, orig) != -1) {

		if (G_site_in_region(orig, &reg_elab) == 1) {

		    y = (int)(G_northing_to_row(orig->north, &reg_elab));
		    x = (int)(G_easting_to_col(orig->east, &reg_elab));

		    if (strcmp(type->answer, "mean") == 0) {

			rast_matrix[y][x].valore += orig->dbl_att[num_index];
			rast_matrix[y][x].freq++;

		    }

		    if (strcmp(type->answer, "max") == 0) {

			if (orig->dbl_att[num_index] >=
			    rast_matrix[y][x].valore) {
			    rast_matrix[y][x].valore = orig->dbl_att[num_index];
			    rast_matrix[y][x].freq++;
			}
		    }

		    if (strcmp(type->answer, "min") == 0) {

			if (orig->dbl_att[num_index] <=
			    rast_matrix[y][x].valore) {

			    rast_matrix[y][x].valore = orig->dbl_att[num_index];
			    rast_matrix[y][x].freq++;
			}
		    }

		    if (strcmp(type->answer, "sqm") == 0) {

			rast_matrix[y][x].valore += orig->dbl_att[num_index];
			rast_matrix[y][x].scarto +=
			    orig->dbl_att[num_index] * orig->dbl_att[num_index];
			rast_matrix[y][x].freq++;
		    }

		}		//end if

	    }			//end while

		 /************************************************************************************************/
	    /* CREAZIONE SITE */

	    num++;
	    strcpy(pippo, "");
	    sprintf(pippo, "%d", num);
	    strcpy(pluto, "");
	    strcpy(pluto, "tmp");

	    G_strcat(pluto, pippo);

	    if (G_set_window(&reg_elab) < 0)
		exit(3);

	    raster_buf = G_allocate_raster_buf(DCELL_TYPE);	/*riga di numero di celle della regione corrente */
	    rast = raster_buf;

	    cf = G_open_raster_new(pluto, DCELL_TYPE);

	    if (cf < 0) {

		char msg[100];
		sprintf(msg, "unable to create raster map %s", output);
		G_fatal_error(msg);
		exit(1);
	    }

	    for (row = 0; row < reg_elab.rows; row++) {
		for (col = 0; col < reg_elab.cols; col++) {

		    if (rast_matrix[row][col].freq != 0) {

			if (strcmp(type->answer, "mean") == 0) {

			    G_set_raster_value_d(raster_buf,
						 (DCELL) (rast_matrix[row][col].
							  valore /
							  rast_matrix[row][col].
							  freq), DCELL_TYPE);
			}

			if (strcmp(type->answer, "max") == 0) {

			    G_set_raster_value_d(raster_buf,
						 (DCELL) (rast_matrix[row][col].
							  valore), DCELL_TYPE);
			}

			if (strcmp(type->answer, "min") == 0) {

			    G_set_raster_value_d(raster_buf,
						 (DCELL) (rast_matrix[row][col].
							  valore), DCELL_TYPE);
			}

			if (strcmp(type->answer, "sqm") == 0) {

			    sqm_tmp =
				(rast_matrix[row][col].scarto /
				 rast_matrix[row][col].freq) -
				(rast_matrix[row][col].valore *
				 rast_matrix[row][col].valore);
			    G_set_raster_value_d(raster_buf, (DCELL) (sqm_tmp),
						 DCELL_TYPE);
			    sqm_tmp = 0;
			}
		    }
		    else {
			G_set_null_value(raster_buf, 1, DCELL_TYPE);	//¿???????? perchè 1?
		    }
		    raster_buf =
			G_incr_void_ptr(raster_buf, G_raster_size(DCELL_TYPE));
		}
		G_put_raster_row(cf, rast, DCELL_TYPE);
		raster_buf = rast;
	    }

	    G_close_cell(cf);

	}			//end while last_co0l=false
    }				// end while last_row=false


    strcpy(pippo, "");
    if (num > 1) {

	strcpy(in_patch, "");

	for (row = 1; row < num; row++) {

	    sprintf(tt, "tmp%d,", row);
	    G_strcat(in_patch, tt);
	    strcpy(tt, "");
	}
	sprintf(tt, "tmp%d,", num);
	G_strcat(in_patch, tt);

	sprintf(pippo, " r.patch input=%s output=%s", in_patch, raster->answer);
	G_system(pippo);

	strcpy(pippo, "");
	sprintf(pippo, " g.remove rast=%s", in_patch);
	G_system(pippo);
    }
    else {
	sprintf(pippo, "g.rename -o rast=tmp1,%s", raster->answer);
	G_system(pippo);
    }

}				//END MAIN


/******************************************************************************/
struct elemento **structMatrix(long nrl, long nrh, long ncl, long nch)
{

    long i, nrow = nrh - nrl + 1, ncol = nch - ncl + 1;
    struct elemento **m;

    /* allocate pointers to rows */
    m = (struct elemento **)calloc((size_t) (nrow + NR_END),
				   sizeof(struct elemento *));
    if (!m)
	nrerror("allocation failure 1 in matrix()");
    m += NR_END;
    m -= nrl;

    /* allocate rows and set pointers to them */
    m[nrl] =
	(struct elemento *)calloc((size_t) (nrow * ncol + NR_END),
				  sizeof(struct elemento));
    if (!m[nrl])
	nrerror("allocation failure 2 in matrix()");
    m[nrl] += NR_END;
    m[nrl] -= ncl;

    for (i = nrl + 1; i <= nrh; i++)
	m[i] = m[i - 1] + ncol;

    /* return pointer to array of pointers to rows */
    return m;
}

/**********************/
void free_structMatrix(struct elemento **m, long nrl, long nrh, long ncl,
		       long nch)
{

    free((FREE_ARG) (m[nrl] + ncl - NR_END));
    free((FREE_ARG) (m + nrl - NR_END));
}
