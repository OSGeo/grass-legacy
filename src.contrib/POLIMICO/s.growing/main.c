/*
 * GROWmain.c
 * s.growing
 * 
 * V 1.1 - 29/05/02     
 * 
 * Politecnico di Milano - Facolta' di Como
 * U. LOngoni - M. Cannata
 */

#include <stdio.h>
#include <gis.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include <unistd.h>
#include <stdlib.h>
#include "gis.h"

#include "site.h"
#include "nrutil0.h"

/* definizione dei parametri per dimensionare la sottoarea da eleborare */
#define true 1
#define false 0
#define LUNG 50
#define lato 1000

#define NR_END 1
#define FREE_ARG char*

#define CMPM(c,A,B) \
v = (*(double**)A)[c] - (*(double**)B)[c];\
if (v>0) return 1;\
if (v<0) return -1;

FILE *finput_last, *finput_first, *f_terreno;
Site *orig_last, *orig_first, *terreno;

struct Option *last, *first, *type, *findex, *growed, *soglia_O, *soglia_2P;
struct Flag *reg;

struct Cell_head reg_orig, reg_elab;

struct elemento
{

    double interp;
    int fi;

    int bordo;

    int dueImp;

    double orig;
    int fo;

    double clas;
    int fc;
};

struct elemento **rast_matrix;

double S_Ogg, sogliaDI;

char *output;
char num_char[10], nomesite_last[LUNG], nomemapset_last[LUNG],
    nomesite_first[LUNG], nomemapset_first[LUNG], tt[LUNG], in_patch[1024];
char pippo[LUNG], pluto[LUNG];
int dims, cat, strs, dbls;
int dims1, cat1, strs1, dbls1;	//variabili per descrizione dei sites
int lung, j, i, s;		//variabili per ricerca del nome site file
int num = 0, num_index, scan_int;
int x, y, row, col, last_row, last_col, c1, c2, conta;
double v, sqm_tmp;
void *rast, *raster_buf;
int cf, xi;

double **points, **P;
int lungPoints, MAXPOINTS;
double minNS, minEW, maxNS, maxEW;

double **pianoInt, altPiano;
int dimPianoR, dimPianoC;

double **cvxHull;
int lungHull;

int RIPIENO, COLORBORDO;

/************************************************************************************/
struct elemento **structMatrix(long nrl, long nrh, long ncl, long nch);
void free_structMatrix(struct elemento **m, long nrl, long nrh, long ncl,
		       long nch);

double **Pvector(long nl, long nh);
void free_Pvector(double **v, long nl, long nh);

void regGrow4(struct elemento **mat, double **punti, int *lung, int r, int c,
	      int v);
void regGrow8(struct elemento **mat, double **punti, int *lung, int r, int c,
	      int v);
int convexHull(double **Q, double **punti, int num);
int checkHull(int cR, int cC, double **oldHull, int lungOld);
double pianOriz(double **punti, int obsNum, double *minNS, double *minEW,
		double *maxNS, double *maxEW, struct elemento **mat);

/************************************************************************************/

int main(int argc, char *argv[])
{

    G_gisinit(argv[0]);

/*********************/
    /* ACQUISIZIONE DATI */

/*********************/

    last = G_define_option();
    last->key = "input";
    last->type = TYPE_STRING;
    last->required = YES;
    last->description = "input last pulse observation sites list file name";
    last->gisprompt = "old,site_lists,sites";

    first = G_define_option();
    first->key = "first";
    first->type = TYPE_STRING;
    first->required = YES;
    first->description =
	"input first pulse sites list file name (s.bordilaser output)";
    first->gisprompt = "old,site_lists,sites";

    growed = G_define_option();
    growed->key = "output";
    growed->type = TYPE_STRING;
    growed->required = YES;
    growed->description = "output classificated sites list file name";
    growed->gisprompt = "new,site_lists,sites";

    soglia_O = G_define_option();
    soglia_O->key = "Tj";
    soglia_O->type = TYPE_DOUBLE;
    soglia_O->required = NO;
    soglia_O->description =
	"threshold for cell object frequency in region growing";
    soglia_O->answer = "0.2";

    soglia_2P = G_define_option();
    soglia_2P->key = "Td";
    soglia_2P->type = TYPE_DOUBLE;
    soglia_2P->required = NO;
    soglia_2P->description = "threshold for double pulse in region growing";
    soglia_2P->answer = "0.6";

    reg = G_define_flag();
    reg->key = 'o';
    reg->description = "without region growing procedure?";

    sscanf(soglia_2P->answer, "%lf", &sogliaDI);
    sscanf(soglia_O->answer, "%lf", &S_Ogg);

    if (G_parser(argc, argv))
	exit(1);

/********************************************************************************************************************/
    /* APERTURA DEL SITES FILE DI INPUT E DI OUTPUT */

    /* se il sitefile è in un altro mapset diverso da quello corrente questo è indicato nel nome (sitename@mapset) */
    lung = strlen(last->answer);
    j = 0;

    /* estraggo il nome del sitefile */
    while ((j < lung) && (last->answer[j] != '@')) {
	nomesite_last[j] = last->answer[j];
	j++;
    }
    nomesite_last[j] = '\0';

    /* estraggo il mapset di appartenenza */
    if (last->answer[j] == '@') {
	j++;
	i = 0;
	while (j < lung) {
	    nomemapset_last[i] = last->answer[j];
	    j++;
	    i++;
	}
	nomemapset_last[i] = '\0';
    }

    /* assegno mapset corrente */
    else
	strcpy(nomemapset_last, G_mapset());

    /* se il sitefile è in un altro mapset diverso da quello corrente questo è indicato nel nome (sitename@mapset) */
    lung = strlen(first->answer);
    j = 0;

    /* estraggo il nome del sitefile */
    while ((j < lung) && (first->answer[j] != '@')) {

	nomesite_first[j] = first->answer[j];
	j++;
    }
    nomesite_first[j] = '\0';

    /* estraggo il mapset di appartenenza */
    if (first->answer[j] == '@') {

	j++;
	i = 0;

	while (j < lung) {
	    nomemapset_first[i] = first->answer[j];
	    j++;
	    i++;
	}
	nomemapset_first[i] = '\0';
    }
    /* assegno mapset corrente */
    else
	strcpy(nomemapset_first, G_mapset());

    /* apertura del file sites dei punti da interpolare(INPUT) */
    if ((finput_last =
	 G_sites_open_old(nomesite_last, nomemapset_last)) == NULL) {

	printf("\nIl file %s non è stato trovato.\n", nomesite_last);
	exit(1);
    }

    if ((finput_first =
	 G_sites_open_old(nomesite_first, nomemapset_first)) == NULL) {

	printf("\nIl file %s non è stato trovato.\n", nomesite_first);
	exit(1);
    }

    /* controlla esattezza del file site: prima se il formato è tipico di grass */
    if (G_site_describe(finput_last, &dims, &cat, &strs, &dbls) != 0) {

	printf("\nErrore nelle lettura del file %s\n", nomesite_last);
	exit(1);
    }

    /* controlla esattezza del file site: prima se il formato è tipico di grass */
    if (G_site_describe(finput_first, &dims1, &cat1, &strs1, &dbls1) != 0) {

	printf("\nErrore nelle lettura del file %s\n", nomesite_first);
	exit(1);
    }

    orig_last = G_site_new_struct(cat, dims, strs, dbls);
    orig_first = G_site_new_struct(cat1, dims1, strs1, dbls1);

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
    reg_elab.rows = G_window_rows();
    reg_elab.cols = G_window_cols();

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

	    MAXPOINTS = reg_elab.rows * reg_elab.cols;

	    // Inizializzazione matrice
	    for (row = 0; row < reg_elab.rows; row++) {
		for (col = 0; col < reg_elab.cols; col++) {

		    rast_matrix[row][col].interp = 0;
		    rast_matrix[row][col].fi = 0;

		    rast_matrix[row][col].bordo = 0;

		    rast_matrix[row][col].dueImp = 0;

		    rast_matrix[row][col].orig = 0;
		    rast_matrix[row][col].fo = 0;

		    rast_matrix[row][col].clas = 0;
		    rast_matrix[row][col].fc = 0;
		}
	    }

	    //lettura site in region
	    fseek(finput_last, 0L, SEEK_SET);

	    while (G_site_get(finput_last, orig_last) != -1) {

		if (G_site_in_region(orig_last, &reg_elab) == 1) {

		    y = (int)(G_northing_to_row(orig_last->north, &reg_elab));
		    x = (int)(G_easting_to_col(orig_last->east, &reg_elab));

		    /*Versione con altezza celle miste calcolata con il solo contributo di quelle bordo*
		     * if ((orig_last->dbl_att[1]==1)&&(rast_matrix[y][x].bordo==0)) {
		     * 
		     * rast_matrix[y][x].bordo  = 1;
		     * rast_matrix[y][x].interp = 0;
		     * rast_matrix[y][x].fi     = 0;
		     * }
		     * 
		     * if ((rast_matrix[y][x].bordo==0)&&(orig_last->dbl_att[1]!=1)) {
		     * 
		     * rast_matrix[y][x].interp += orig_last->dbl_att[2];
		     * rast_matrix[y][x].fi++;
		     * }
		     * 
		     * if ((rast_matrix[y][x].bordo==1)&&(orig_last->dbl_att[1]==1)) {
		     * 
		     * rast_matrix[y][x].interp += orig_last->dbl_att[2];
		     * rast_matrix[y][x].fi++;
		     * }
		     * 
		     * /*Versione media semplice */
		    rast_matrix[y][x].interp += orig_last->dbl_att[2];
		    rast_matrix[y][x].fi++;

		    if (orig_last->dbl_att[1] != 2) {
			rast_matrix[y][x].clas += orig_last->dbl_att[1];
			rast_matrix[y][x].fc++;
		    }

		    rast_matrix[y][x].orig += orig_last->dbl_att[0];
		    rast_matrix[y][x].fo++;

		}		//end if
	    }			//end while

	    for (row = 0; row < reg_elab.rows; row++) {
		for (col = 0; col < reg_elab.cols; col++) {

		    if (rast_matrix[row][col].fc == 0)
			rast_matrix[row][col].clas = 0;
		    else
			rast_matrix[row][col].clas =
			    rast_matrix[row][col].clas /
			    rast_matrix[row][col].fc;

		    if (rast_matrix[row][col].fi != 0)
			rast_matrix[row][col].interp =
			    rast_matrix[row][col].interp /
			    rast_matrix[row][col].fi;

		    if (rast_matrix[row][col].fo != 0)
			rast_matrix[row][col].orig =
			    rast_matrix[row][col].orig /
			    rast_matrix[row][col].fo;
		}
	    }

		/************************************************************************************************/
	    /*  Doppio Impulso */

	    //lettura site in region
	    fseek(finput_first, 0L, SEEK_SET);

	    while (G_site_get(finput_first, orig_first) != -1) {

		if (G_site_in_region(orig_first, &reg_elab) == 1) {

		    y = (int)(G_northing_to_row(orig_first->north, &reg_elab));
		    x = (int)(G_easting_to_col(orig_first->east, &reg_elab));

		    if ((x < 0) || (y < 0) || (x >= reg_elab.cols) ||
			(y >= reg_elab.rows)) {
			printf("Dati fuori zona!\n");
		    }
		    else {
			if (abs(orig_first->dbl_att[0] - rast_matrix[y][x].orig)
			    >= sogliaDI) {
			    rast_matrix[y][x].dueImp = 1;
			}
		    }
		}
	    }

		/************************************************************************************************/
	    /*  Region Growing */
	    if (!reg->answer) {

		printf("RegGrowing\n");

		points = dmatrix(0, MAXPOINTS - 1, 0, 2);
		P = Pvector(0, MAXPOINTS);

		COLORBORDO = 5;
		RIPIENO = 6;

		for (row = 0; row < reg_elab.rows; row++) {
		    for (col = 0; col < reg_elab.cols; col++) {

			if ((rast_matrix[row][col].clas >= S_Ogg) &&
			    (rast_matrix[row][col].clas < COLORBORDO)
			    && (rast_matrix[row][col].fi != 0) &&
			    (rast_matrix[row][col].dueImp == 0)) {

			    /* Seleziona una zona Oggetto connessa */

			    RIPIENO++;
			    if (RIPIENO > 10)
				RIPIENO = 6;

			    /*Selezione i punti appartenenti ad un bordo connesso */
			    for (conta = 0; conta < MAXPOINTS; conta++) {
				points[conta][0] = 0;
				points[conta][1] = 0;
				points[conta][2] = 0;

				P[conta] = points[conta];
			    }

			    lungPoints = 0;
			    lungHull = 0;

			    //regGrow4(rast_matrix,points,&lungPoints,row,col,COLORBORDO);
			    regGrow8(rast_matrix, points, &lungPoints, row, col,
				     COLORBORDO);

			    /*Calcolo la convex hull */
			    lungHull = ch2d(P, lungPoints);

			    cvxHull = dmatrix(0, lungHull - 1, 0, 2);

			    for (xi = 0; xi < lungHull; xi++) {

				cvxHull[xi][0] = P[xi][0];
				cvxHull[xi][1] = P[xi][1];
				cvxHull[xi][2] = P[xi][2];
			    }

			    /*Calcola piano intepolante basandosi solamente sui punti classificati Oggetto */
			    altPiano =
				pianOriz(points, lungPoints, &minNS, &minEW,
					 &maxNS, &maxEW, rast_matrix);

			    for (c1 = minNS; c1 <= maxNS; c1++) {
				for (c2 = minEW; c2 <= maxEW; c2++) {

				    if ((checkHull(c1, c2, cvxHull, lungHull) ==
					 1) && (rast_matrix[c1][c2].clas == 0)
					&& (rast_matrix[c1][c2].orig >=
					    altPiano)) {

					rast_matrix[c1][c2].clas = RIPIENO;
				    }
				}
			    }

			    free_dmatrix(cvxHull, 0, lungHull - 1, 0, 2);
			}
		    }
		}
	    }

		/************************************************************************************************/
	    /* scrittura sites solo terreno */

	    terreno = G_site_new_struct(0, 2, 0, 2);

	    f_terreno = G_sites_open_new(growed->answer);
	    if (growed->answer == NULL) {

		fprintf(stderr, "can't create sites file [%s]\n",
			growed->answer);
		exit(1);
	    }

	    fseek(finput_last, 0L, SEEK_SET);

	    while (G_site_get(finput_last, orig_last) != -1) {

		if (G_site_in_region(orig_last, &reg_elab) == 1) {

		    terreno->east = orig_last->east;
		    terreno->north = orig_last->north;
		    terreno->dbl_att[0] = orig_last->dbl_att[0];

		    y = (int)(G_northing_to_row(orig_last->north, &reg_elab));
		    x = (int)(G_easting_to_col(orig_last->east, &reg_elab));

		    if (rast_matrix[y][x].clas == 0) {

			if (rast_matrix[y][x].dueImp == 0) {
			    terreno->dbl_att[1] = 0;
			}
			else {
			    terreno->dbl_att[1] = 1;
			}

		    }
		    else {

			if (rast_matrix[y][x].dueImp == 0) {
			    terreno->dbl_att[1] = 3;
			}
			else {
			    terreno->dbl_att[1] = 2;
			}
		    }
		    G_site_put(f_terreno, terreno);
		}
	    }
	}			//end while last_co0l=false
    }				// end while last_row=false

    G_site_free_struct(orig_first);
    G_site_free_struct(orig_last);
    G_site_free_struct(terreno);

    free_structMatrix(rast_matrix, 0, reg_elab.rows - 1, 0, reg_elab.cols - 1);

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

/**********************/
void regGrow8(struct elemento **mat, double **punti, int *lung, int r, int c,
	      int v)
{

    mat[r][c].clas = v;

    punti[*lung][0] = c;
    punti[*lung][1] = r;
    punti[*lung][2] = mat[r][c].interp;

    assert((*lung)++ < MAXPOINTS - 1);

    if (r - 1 >= 0) {
	if ((mat[r - 1][c].clas > S_Ogg) && (mat[r - 1][c].clas < v) &&
	    (mat[r - 1][c].fi != 0)) {
	    regGrow8(mat, punti, lung, r - 1, c, v);
	}
    }

    if (c - 1 >= 0) {
	if ((mat[r][c - 1].clas > S_Ogg) && (mat[r][c - 1].clas < v) &&
	    (mat[r][c - 1].fi != 0)) {
	    regGrow8(mat, punti, lung, r, c - 1, v);
	}
    }

    if (c + 1 < reg_elab.cols) {
	if ((mat[r][c + 1].clas > S_Ogg) && (mat[r][c + 1].clas < v) &&
	    (mat[r][c + 1].fi != 0)) {
	    regGrow8(mat, punti, lung, r, c + 1, v);
	}
    }

    if (r + 1 < reg_elab.rows) {
	if ((mat[r + 1][c].clas > S_Ogg) && (mat[r + 1][c].clas < v) &&
	    (mat[r + 1][c].fi != 0)) {
	    regGrow8(mat, punti, lung, r + 1, c, v);
	}
    }

    if ((r - 1 >= 0) && (c - 1 >= 0)) {
	if ((mat[r - 1][c - 1].clas > S_Ogg) && (mat[r - 1][c - 1].clas < v) &&
	    (mat[r - 1][c - 1].fi != 0)) {
	    regGrow8(mat, punti, lung, r - 1, c - 1, v);
	}
    }

    if ((r - 1 >= 0) && (c + 1 < reg_elab.cols)) {
	if ((mat[r - 1][c + 1].clas > S_Ogg) && (mat[r - 1][c + 1].clas < v) &&
	    (mat[r - 1][c + 1].fi != 0)) {
	    regGrow8(mat, punti, lung, r - 1, c + 1, v);
	}
    }

    if ((r + 1 < reg_elab.rows) && (c - 1 >= 0)) {
	if ((mat[r + 1][c - 1].clas > S_Ogg) && (mat[r + 1][c - 1].clas < v) &&
	    (mat[r + 1][c - 1].fi != 0)) {
	    regGrow8(mat, punti, lung, r + 1, c - 1, v);
	}
    }

    if ((r + 1 < reg_elab.rows) && (c + 1 < reg_elab.cols)) {
	if ((mat[r + 1][c + 1].clas > S_Ogg) && (mat[r + 1][c + 1].clas < v) &&
	    (mat[r + 1][c + 1].fi != 0)) {
	    regGrow8(mat, punti, lung, r + 1, c + 1, v);
	}
    }
}

/* true if points i, j, k counterclockwise */
int ccw(double **P, int i, int j, int k)
{

    double a, b, c, d;

    a = P[i][0] - P[j][0];
    b = P[i][1] - P[j][1];
    c = P[k][0] - P[j][0];
    d = P[k][1] - P[j][1];
    return a * d - b * c <= 0;
}

int cmpl(const void *a, const void *b)
{
    double v;
    CMPM(0, a, b);
    CMPM(1, b, a);
    return 0;
}

int cmph(const void *a, const void *b)
{
    return cmpl(b, a);
}

int make_chain(double **V, int n, int (*cmp) (const void *, const void *))
{
    int i, j, s = 1;
    double *t;

    qsort(V, n, sizeof(double *), cmp);

    for (i = 2; i < n; i++) {
	for (j = s; j >= 1 && ccw(V, i, j, j - 1); j--) {
	}

	s = j + 1;
	t = V[s];
	V[s] = V[i];
	V[i] = t;
    }

    return s;
}

int ch2d(double **P, int n)
{
    int u = make_chain(P, n, cmpl);	/* make lower hull */

    if (!n)
	return 0;
    P[n] = P[0];

    return u + make_chain(P + u, n - u + 1, cmph);	/* make upper hull */
}

void print_hull(double **P, double **pti, int m, double **h)
{

    int i;

    for (i = 0; i < m; i++) {
	h[i][0] = pti[(P[i] - pti[0]) / 2][0];
	h[i][1] = pti[(P[i] - pti[0]) / 2][1];
	h[i][2] = pti[(P[i] - pti[0]) / 2][2];
    }
}

int checkHull(int cR, int cC, double **oldHull, int lungOld)
{

    double **newP;
    double **newPoint;
    int count, lungHullNew;

    newP = Pvector(0, lungOld + 1);
    newPoint = dmatrix(0, lungOld, 0, 1);

    for (count = 0; count < lungOld; count++) {
	newPoint[count][0] = oldHull[count][0];
	newPoint[count][1] = oldHull[count][1];

	newP[count] = newPoint[count];
    }

    newPoint[lungOld][0] = cC;
    newPoint[lungOld][1] = cR;

    newP[lungOld] = newPoint[lungOld];

    lungHullNew = ch2d(newP, lungOld + 1);

    if (lungOld != lungHullNew) {
	free_dmatrix(newPoint, 0, lungOld, 0, 1);
	free_Pvector(newP, 0, lungOld + 1);

	return 0;
    }
    else {
	for (count = 0; count < lungOld; count++) {
	    if ((oldHull[count][0] != newP[count][0]) ||
		(oldHull[count][1] != newP[count][1])) {
		free_dmatrix(newPoint, 0, lungOld, 0, 1);
		free_Pvector(newP, 0, lungOld + 1);

		return 0;
	    }
	}
    }
    free_dmatrix(newPoint, 0, lungOld, 0, 1);
    free_Pvector(newP, 0, lungOld + 1);

    return 1;
}

/* Calcola piano interpolante *
 * void piano(double **piano,double **punti,int obsNum,int dimR,int dimC,double minNS, double minEW) {
 * 
 * double **A           = dmatrix (0, obsNum - 1, 0, 2);
 * double  *N           = dvector (0, 2);
 * double  *p           = dvector (0, 2);
 * double  *pEst        = dvector (0, 2);
 * double **ground      = dmatrix (0, obsNum - 1, 0, 2);
 * 
 * double mediaR,mediaC,maxNS,maxEW;
 * 
 * 
 * minNS = punti[0][1];
 * maxNS = punti[0][1];
 * minEW = punti[0][0];
 * maxEW = punti[0][0];
 * 
 * mediaR = 0;
 * mediaC = 0;
 * 
 * for (c1=1;c1<obsNum;c1++) {
 * 
 * if (punti[c1][0]>maxEW) maxEW=punti[c1][0];
 * if (punti[c1][0]<minEW) minEW=punti[c1][0];
 * if (punti[c1][1]>maxNS) maxNS=punti[c1][1];                                          
 * if (punti[c1][1]<minNS) minNS=punti[c1][1];                                          
 * 
 * mediaR += punti[c1][1];
 * mediaC += punti[c1][0];
 * }
 * 
 * mediaR /= obsNum;
 * mediaC /= obsNum;
 * 
 * dimR = maxNS-minNS+1;
 * dimC = maxEW-minEW+1;
 * 
 * pianoInt = dmatrix (0, dimR-1, 0, dimC-1);
 * 
 * for (i = 0; i < obsNum; i++) {
 * ground[i][0] = punti[i][0]-mediaC;
 * ground[i][1] = punti[i][1]-mediaR;
 * ground[i][2] = punti[i][2];
 * }
 * 
 * for (i = 0; i < obsNum; i++){
 * A[i][0] = 1;
 * A[i][1] = ground[i][0];
 * A[i][2] = ground[i][1];
 * }
 * 
 * N[0] = obsNum;
 * for (i = 0; i < obsNum; i++){
 * N[1] += (ground[i][0] * ground[i][0]);
 * N[2] +=      (ground[i][1] * ground[i][1]);
 * }
 * 
 * for (j = 0 ; j < 3 ; j++){
 * for (i = 0 ; i < obsNum ; i++){
 * p[j] += A[i][j] * ground[i][2] ;
 * }
 * pEst[j]= (p[j] / N[j]);
 * }
 * 
 * for (j = 0; j < dimR; j++) {
 * for (i = 0; i < dimC; i++) {
 * piano[j][i] = (pEst[0]+pEst[1]*(i-mediaC)+pEst[2]*(j-mediaR));
 * }
 * }
 * 
 * free_dmatrix(A, 0, obsNum-1, 0,2);
 * free_dvector(N, 0, 2);
 * free_dvector(p, 0, 2);
 * free_dvector(pEst, 0, 2);
 * free_dmatrix(ground, 0, obsNum-1, 0, 2);
 * }
 * /* */


double pianOriz(double **punti, int obsNum, double *minNS, double *minEW,
		double *maxNS, double *maxEW, struct elemento **mat)
{

    double minBordo, minBordo1, medioBordo;

    /*Calcola coordinate min e max della zona e media delle righe e delle colonne */
    *minNS = punti[0][1];
    *maxNS = punti[0][1];
    *minEW = punti[0][0];
    *maxEW = punti[0][0];

    medioBordo = 0;

    minBordo = punti[0][2];
    minBordo1 = punti[0][2];

    for (c1 = 0; c1 < obsNum; c1++) {

	if (punti[c1][0] > *maxEW)
	    *maxEW = punti[c1][0];
	if (punti[c1][0] < *minEW)
	    *minEW = punti[c1][0];
	if (punti[c1][1] > *maxNS)
	    *maxNS = punti[c1][1];
	if (punti[c1][1] < *minNS)
	    *minNS = punti[c1][1];

	if ((punti[c1][2] < minBordo) &&
	    (mat[(int)(punti[c1][1])][(int)(punti[c1][0])].clas >= 1)
	    && (mat[(int)(punti[c1][1])][(int)(punti[c1][0])].clas <
		COLORBORDO)) {

	    minBordo1 = punti[c1][2];
	}

	if (punti[c1][2] < minBordo)
	    minBordo = punti[c1][2];

	medioBordo += punti[c1][2];
    }

    medioBordo /= obsNum;

    //      return minBordo;
    //      return minBordo1;
    return medioBordo;
}

double **Pvector(long nl, long nh)
{

    double **v;

    v = (double **)calloc((size_t) (nh - nl + 1 + NR_END), sizeof(double *));
    if (!v)
	nrerror("allocation failure in dvector()");
    return v - nl + NR_END;
}

void free_Pvector(double **v, long nl, long nh)
{

    free((FREE_ARG) (v + nl - NR_END));
}
