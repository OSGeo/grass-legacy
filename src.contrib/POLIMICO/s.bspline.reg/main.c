/*
 * main.c
 * s.bspline.reg
 * 
 * V 1.1 - 29/05/02     
 * 
 * Politecnico di Milano - Facolta' di Como
 * U. Longoni - M. Cannata
 *
 * Modified - 02/09/05 
 * Politecnico di Milano - Facolta' di Como
 * R. Antolin - M. Reguzzoni
 *
 */

#include <stdio.h>
#include <gis.h>
#include <string.h>
#include <math.h>

#include "site.h"
#include "nrutil0.h"
#include "tcholband.h"
#include "interpspline.h"

/* definizione dei parametri per dimensionare la sottoarea da eleborare */

#define nsplx_max 280		//max numero di spline da utilizzare per sottoelaborazioni lungo E
#define nsply_max 200		//max numero di spline da utilizzare per sottoelaborazioni lungo N
#define LUNG 50

#define true 1
#define false 0

#define SWAP(a,b) temp=(a); (a)=(b); (b)=temp;
#define NSTACK 50
#define M 7
#define contur 15		//contorno della sottozona su cui calcolare la media

FILE *finput, *foutput;
Site *interpolate, *orig;

double *TN, *Q, *parVect, *x, *y, *z, interp, peso, csi, eta;	//vettori da passare alle procedure
double **N, **obsVect;		//matrici da passare alle procedure

int num_spline, num_spline_E, num_spline_N, num_obs, BW, reg, nr, nc, kk, jj,
    parNum;
int reg_c, reg_r, ss, nsplx, nsply;

double passoE, passoN, latoE, latoN, lambda, media, sovr;

char *me;

double coordX, coordY, orlo_h, orlo_v;

double reg_orlo_n, reg_orlo_s, reg_orlo_e, reg_orlo_w;
double reg_bordo_n, reg_bordo_s, reg_bordo_e, reg_bordo_w;

int last_col, last_row, row, col;
int tt, conta, mm, nn, num = 0;
char nomesite[LUNG], nomemapset[LUNG];
int dim_vect, dim_vect_max;	//num di osservazioni a volta tenute in memoria
int col_beg, col_end, row_beg, row_end;

char pippo[LUNG], pluto[LUNG], in_patch[1024], set[LUNG];
void *rast, *raster_buf;
int cf;

struct Option *input, *output, *passo_E, *passo_N, *lambda_f, *type, *raster;
struct Flag *grid;
struct Cell_head reg_orig, reg_elab, reg_buf;

struct svr
{
    double coordX;
    double coordY;
    double quota;
};

struct svr *rigaPrec, *rigaSuc, *colPrec, *colSuc, *trePrec, *treSuc, *novePrec,
    *noveSuc;

long C_rigaPrec, C_rigaSuc, C_colPrec, C_colSuc, C_trePrec, C_treSuc,
    C_novePrec, C_noveSuc, pos, pos1;
long gt1, gt2, gt3, gt4, gt5, gt6, gt7, gt8;

int dims, cat, strs, dbls;	//variabili per descrizione dei sites
int lung, j, i, s;		//variabili per ricerca del nome site file

/***************************************************************************************/
unsigned long ricercaBinaria(double cX, double cY, struct svr *vett,
			     unsigned long lungList);
void quickSortList(unsigned long n, struct svr *arr);
void puntisparsi(void);
void puntigrigliati(void);

/***************************************************************************************/

int main(int argc, char *argv[])
{

    G_gisinit(me = argv[0]);

/*********************/
    /* ACQUISIZIONE DATI */

/*********************/

    input = G_define_option();
    input->key = "input";
    input->type = TYPE_STRING;
    input->required = YES;
    input->description = "input observation sites list file name";
    input->gisprompt = "old,site_lists,sites";

    output = G_define_option();
    output->key = "output";
    output->type = TYPE_STRING;
    output->required = YES;
    output->description =
	"output interpolated file name (default is a sites list)";

    grid = G_define_flag();
    grid->key = 'g';
    grid->description = "rast output instead sites list file";

    passo_E = G_define_option();
    passo_E->key = "Sie";
    passo_E->type = TYPE_DOUBLE;
    passo_E->required = YES;
    passo_E->description = "interpolation spline step value in east direction";

    passo_N = G_define_option();
    passo_N->key = "Sin";
    passo_N->type = TYPE_DOUBLE;
    passo_N->required = YES;
    passo_N->description = "interpolation spline step value in north direction";

    type = G_define_option();
    type->key = "type";
    type->type = TYPE_STRING;
    type->required = NO;
    type->description = "spline type";
    type->options = "bilinear,bicubic";
    type->answer = "bilinear";


    lambda_f = G_define_option();
    lambda_f->key = "lambda_i";
    lambda_f->type = TYPE_DOUBLE;
    lambda_f->required = NO;
    lambda_f->description = "Thchonov regularization weigth";
    lambda_f->answer = "1";

    if (G_parser(argc, argv))
	exit(1);

    sscanf(lambda_f->answer, "%lf", &lambda);
    sscanf(passo_E->answer, "%lf", &passoE);
    sscanf(passo_N->answer, "%lf", &passoN);

    dim_vect_max = nsplx_max * nsply_max;

/************************************************/
    /* APERTURA DEL SITES FILE DI INPUT E DI OUTPUT */

/************************************************/

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

    /* controlla esattezza del file site: prima se il formato è tipico di grass e poi se il formato è -- x|y|#value(strs)%value(dbls) -- */
    if (G_site_describe(finput, &dims, &cat, &strs, &dbls) != 0) {
	printf("\nErrore nelle lettura del file di ingresso\n");
	exit(1);
    }

    if ((cat != 0) || (dims != 2) || (strs != 0) || (dbls != 1)) {
	fprintf(stdout,
		"\nWRONG input list sites file format\nGuessed dims=%d strs=%d dbls=%d cat=%d\n END of program.\n",
		dims, strs, dbls, cat);
	exit(1);
    }

    /* allocazione per la struttura ed apertura del file sites dei punti interpolati(output) */
    interpolate = G_site_new_struct(cat, dims, strs, dbls);
    orig = G_site_new_struct(cat, dims, strs, dbls);

    foutput = G_sites_open_new(output->answer);

    if (output->answer == NULL) {
	fprintf(stderr, " %s - can't create sites file [%s]", me,
		output->answer);
	exit(1);
    }

/******************************************/
    /* SELEZIONE DELLE SOTTOZONE DA ELABORARE */

/******************************************/

    /*inizializzo le variabili contatori delle strutture svr */
    C_rigaPrec = 0;
    C_rigaSuc = 0;
    C_colPrec = 0;
    C_colSuc = 0;
    C_trePrec = 0;
    C_treSuc = 0;
    C_novePrec = 0;
    C_noveSuc = 0;

    gt1 = 1000;
    gt2 = 1000;
    gt3 = 1000;
    gt4 = 1000;
    gt5 = 1000;
    gt6 = 1000;
    gt7 = 1000;
    gt8 = 1000;

    /*allocazione memoria strutture svr */
    rigaPrec = (struct svr *)calloc(gt1, sizeof(struct svr));
    rigaSuc = (struct svr *)calloc(gt2, sizeof(struct svr));
    colPrec = (struct svr *)calloc(gt3, sizeof(struct svr));
    colSuc = (struct svr *)calloc(gt4, sizeof(struct svr));
    trePrec = (struct svr *)calloc(gt5, sizeof(struct svr));
    treSuc = (struct svr *)calloc(gt6, sizeof(struct svr));
    novePrec = (struct svr *)calloc(gt7, sizeof(struct svr));
    noveSuc = (struct svr *)calloc(gt8, sizeof(struct svr));

    /*legge la regione corrente */
    G_get_window(&reg_orig);
    nr = G_window_rows();
    nc = G_window_cols();

    /*assegna i parametri fissi delle sottoregioni da utilizzzare per elaborazione */
    reg_elab.ew_res = (reg_orig.ew_res);
    reg_elab.ns_res = (reg_orig.ns_res);
    reg_elab.format = reg_orig.format;
    reg_elab.compressed = reg_orig.compressed;
    reg_elab.proj = reg_orig.proj;
    reg_elab.zone = reg_orig.zone;

    sovr = reg_elab.ew_res * 10;

    /*valuta i lati delle sottozone massime da elaborare */
    latoE = nsplx_max * passoE;
    latoN = nsply_max * passoN;

    /*calcolo dell'orlo delle sottozone */
    if (strcmp(type->answer, "bilinear") == 0) {
	orlo_v = 4 * passoE;
	orlo_h = 4 * passoN;
    }
    else {
	orlo_v = 3 * passoE;
	orlo_h = 3 * passoN;
    }

    /*controllo sulla dimensione minima della regione selezionata */
    if ((reg_orig.east - reg_orig.west) < (2 * orlo_v) ||
	(reg_orig.north - reg_orig.south) < (2 * orlo_h)) {
	fprintf(stderr,
		" %s - selected region too small, consider reduce interpolation steps",
		me);
	exit(1);
    }

    /*ciclo per la selezione delle sottozone che comprendono l'orlo e la sovrapposizione */
    reg_r = 0;
    reg_elab.south = reg_orig.north;

    last_row = false;
    while (last_row == false) {

	reg_c = 0;
	reg_elab.east = reg_orig.west;

	reg_elab.north = reg_elab.south + sovr + (2 * orlo_h);
	reg_elab.south = reg_elab.north - latoN;

	reg_orlo_n = reg_elab.north - orlo_h;
	reg_orlo_s = reg_elab.south + orlo_h;

	reg_bordo_n = reg_orlo_n - sovr;
	reg_bordo_s = reg_orlo_s + sovr;

	nsply = nsply_max;

	if (reg_elab.north > reg_orig.north) {
	    reg_elab.north = reg_orig.north;
	    reg_elab.south = reg_elab.north - latoN;
	    reg_orlo_n = reg_elab.north;
	    reg_orlo_s = reg_elab.south + orlo_h;
	    reg_bordo_n = reg_elab.north;
	    reg_bordo_s = reg_orlo_s + sovr;

	    nsply = ceil((reg_elab.north - reg_elab.south) / passoN) + 1;

	     /**/ if (nsply > nsply_max) {
		printf("\nnsply=%d nsply_max=%d\n", nsply, nsply_max);
	    }

	}
	if (reg_elab.south <= reg_orig.south) {
	    reg_elab.south = reg_orig.south;
	    reg_orlo_s = reg_elab.south;
	    reg_bordo_s = reg_elab.south;
	    last_row = true;

	    nsply = ceil((reg_elab.north - reg_elab.south) / passoN) + 1;

	     /**/ if (nsply > nsply_max) {
		printf("\nnsply=%d nsply_max=%d\n", nsply, nsply_max);
	    }
	}

	reg_r++;

	last_col = false;
	while (last_col == false) {
	    reg_elab.west = reg_elab.east - sovr - (2 * orlo_v);
	    reg_elab.east = reg_elab.west + latoE;

	    reg_orlo_w = reg_elab.west + orlo_v;
	    reg_orlo_e = reg_elab.east - orlo_v;

	    reg_bordo_w = reg_orlo_w + sovr;
	    reg_bordo_e = reg_orlo_e - sovr;

	    nsplx = nsplx_max;

	    if (reg_elab.west < reg_orig.west) {

		reg_elab.west = reg_orig.west;
		reg_elab.east = reg_elab.west + latoE;
		reg_orlo_w = reg_elab.west;
		reg_orlo_e = reg_elab.east - orlo_v;
		reg_bordo_w = reg_elab.west;
		reg_bordo_e = reg_orlo_e - sovr;

		nsplx = ceil((reg_elab.east - reg_elab.west) / passoE) + 1;

		 /**/ if (nsplx > nsplx_max) {
		    printf("\nnsplx=%d nsplx_max=%d\n", nsplx, nsplx_max);
		}
	    }

	    if (reg_elab.east >= reg_orig.east) {
		reg_elab.east = reg_orig.east;
		reg_orlo_e = reg_elab.east;
		reg_bordo_e = reg_elab.east;

		last_col = true;

		nsplx = ceil((reg_elab.east - reg_elab.west) / passoE) + 1;

		 /**/ if (nsplx > nsplx_max) {
		    printf("\nnsplx=%d nsplx_max=%d\n", nsplx, nsplx_max);
		}
	    }

	    reg_c++;

	    dim_vect = dim_vect_max;

	    /* allocazione vettori delle osservazioni */
	    x = (double *)calloc(dim_vect, sizeof(double));
	    y = (double *)calloc(dim_vect, sizeof(double));
	    z = (double *)calloc(dim_vect, sizeof(double));

	    /*seleziona i punti del site delle osservazioni che cadono nella regione e li scrive nella matrice obsVect */
	    s = -1;

	    fseek(finput, 0L, SEEK_SET);

	    /*lettura del site */
	    while (G_site_get(finput, orig) != -1) {

		if (G_site_in_region(orig, &reg_elab) == 1) {

		    s++;

		    if (s >= dim_vect) {

			dim_vect += dim_vect_max;

			x = (double *)realloc(x, dim_vect * sizeof(double));
			y = (double *)realloc(y, dim_vect * sizeof(double));
			z = (double *)realloc(z, dim_vect * sizeof(double));

		    }		//end if                                                       

		    x[s] = orig->east;
		    y[s] = orig->north;
		    z[s] = orig->dbl_att[0];

		}		//end if
	    }

	    if (s > 0) {

		/* allocazione vettori e matrici per input procedura interpolazione */
		parNum = nsplx * nsply;

		if (strcmp(type->answer, "bilinear") == 0) {
		    BW = 2 * nsply + 1;
		}
		else {
		    BW = 4 * nsply + 3;
		}

		N = dmatrix(0, parNum - 1, 0, BW - 1);
		TN = dvector(0, parNum - 1);
		parVect = dvector(0, parNum - 1);

		obsVect = dmatrix(0, s, 0, 2);

		Q = dvector(0, s);

		mm = 0;
		media = 0;
		for (ss = 0; ss <= s; ss++) {
		    if (!
			(x[ss] > (reg_elab.west + contur) &&
			 x[ss] < (reg_elab.east - contur) &&
			 y[ss] > (reg_elab.south + contur) &&
			 y[ss] < (reg_elab.north - contur))) {
			mm++;
			media += z[ss];
		    }
		}

		if (mm != 0) {
		    media /= mm;
		}
		else {
		    media = 0;
		    printf("\nWarning!consider larger contour\n");
		}


		for (ss = 0; ss <= s; ss++) {
		    Q[ss] = 1;

		    obsVect[ss][0] = x[ss];
		    obsVect[ss][1] = y[ss];
		    obsVect[ss][2] = z[ss] - media;
		}
		free(x);
		free(y);
		free(z);

		/* calcolo parametri dell'interpolazione */
		if (strcmp(type->answer, "bilinear") == 0) {
		    normalDefBilin(N, TN, Q, obsVect, passoE, passoN, nsplx,
				   nsply, reg_elab.west, reg_elab.south, s,
				   parNum, BW);
		    nCorrectGrad(N, lambda, nsplx, nsply, passoE, passoN);
		}
		else {
		    normalDefBicubic(N, TN, Q, obsVect, passoE, passoN, nsplx,
				     nsply, reg_elab.west, reg_elab.south, s,
				     parNum, BW);
		    nCorrectLapl(N, lambda, nsplx, nsply, passoE, passoN);
		}

		tcholSolve(N, TN, parVect, parNum, BW);

			/*****************************************/
		/* ottengo parVect -> calcolo gradiente  */

			/*****************************************/

		free_dmatrix(N, 0, parNum - 1, 0, BW - 1);
		free_dvector(TN, 0, parNum - 1);
		free_dvector(Q, 0, s);

			/*--------------------------------------------*/
		/* stima e scrittura dei punti grigliati o non */

			/*--------------------------------------------*/

		if (!grid->answer) {

		    puntisparsi();
		    free_dmatrix(obsVect, 0, s, 0, 2);
		}
		else {

		    free_dmatrix(obsVect, 0, s, 0, 2);
		    puntigrigliati();
		}

		free_dvector(parVect, 0, parNum - 1);

	    }
	    else {

		free(x);
		free(y);
		free(z);
	    }

	    trePrec =
		(struct svr *)realloc(trePrec, C_treSuc * sizeof(struct svr));

	    for (conta = 0; conta < C_treSuc; conta++) {
		trePrec[conta] = treSuc[conta];
	    }

	    C_trePrec = C_treSuc;
	    C_treSuc = 0;
	    gt6 = 1000;
	    treSuc = (struct svr *)realloc(treSuc, gt6 * sizeof(struct svr));

	    colPrec =
		(struct svr *)realloc(colPrec, C_colSuc * sizeof(struct svr));

	    for (conta = 0; conta < C_colSuc; conta++) {
		colPrec[conta] = colSuc[conta];
	    }
	    C_colPrec = C_colSuc;
	    C_colSuc = 0;
	    gt4 = 1000;
	    colSuc = (struct svr *)realloc(colSuc, gt4 * sizeof(struct svr));

	    novePrec =
		(struct svr *)realloc(novePrec, C_noveSuc * sizeof(struct svr));

	    for (conta = 0; conta < C_noveSuc; conta++) {
		novePrec[conta] = noveSuc[conta];
	    }
	    C_novePrec = C_noveSuc;
	    C_noveSuc = 0;
	    gt8 = 1000;
	    noveSuc = (struct svr *)realloc(noveSuc, gt8 * sizeof(struct svr));

	    quickSortList(C_colPrec, colPrec);
	    quickSortList(C_trePrec, trePrec);
	    quickSortList(C_novePrec, novePrec);

	}			//end while col x regioni da elaborare

	rigaPrec =
	    (struct svr *)realloc(rigaPrec, C_rigaSuc * sizeof(struct svr));

	for (conta = 0; conta < C_rigaSuc; conta++) {
	    rigaPrec[conta] = rigaSuc[conta];
	}

	C_rigaPrec = C_rigaSuc;
	C_rigaSuc = 0;
	gt6 = 1000;
	rigaSuc = (struct svr *)realloc(rigaSuc, gt2 * sizeof(struct svr));

	quickSortList(C_rigaPrec, rigaPrec);
    }

    if (grid->answer) {
	strcpy(pippo, "");
	if (num > 1) {

	    strcpy(in_patch, "");

	    for (row = 1; row < num; row++) {
		sprintf(set, "tmp%d,", row);
		G_strcat(in_patch, set);
		strcpy(set, "");
	    }

	    sprintf(set, "tmp%d,", num);
	    G_strcat(in_patch, set);

	    sprintf(pippo, " r.patch input=%s output=%s", in_patch,
		    output->answer);
	    G_system(pippo);

	    strcpy(pippo, "");
	    sprintf(pippo, " g.remove rast=%s", in_patch);
	    G_system(pippo);
	}
	else {
	    sprintf(pippo, "g.rename -o rast=tmp1,%s", output->answer);
	    G_system(pippo);
	}
    }

    G_site_free_struct(interpolate);
    G_site_free_struct(orig);

}

/*--------------------------------------------------------*/
/*procedura scelta del minimo tra duo valori */
double min(double val1, double val2)
{
    if (val1 - val2 > 0)
	return val2;
    else
	return val1;
}

/*--------------------------------------------------------*/
/*procedura scelta del massimo tra duo valori */
double max(double val1, double val2)
{
    if (val1 - val2 > 0)
	return val1;
    else
	return val2;
}

/*------------------------------------------------------------------------------------*/
/*procedura per ricerca elementi su variabile ordinata di struttura svr */
unsigned long ricercaBinaria(double cX, double cY, struct svr *vett,
			     unsigned long lungList)
{

    int inizio, fine;
    int i;

    inizio = 0;
    fine = lungList - 1;

    do {
	i = (int)floor((inizio + fine) / 2);

	if (cX != vett[i].coordX) {
	    if (cX < vett[i].coordX)
		fine = i - 1;
	    else
		inizio = i + 1;
	}
	else {
	    if (cY < vett[i].coordY)
		fine = i - 1;
	    else
		inizio = i + 1;
	}

    } while (inizio <= fine);

    if ((cX != vett[fine].coordX) || (cY != vett[fine].coordY))
	return -1;
    else
	return fine;
}

/*------------------------------------------------------------------------------------*/
/* procedura di ordinamento di variabile con struttura svr */
void quickSortList(unsigned long n, struct svr arr[])
{

    int i, j, k, l = 0, ir = n - 1;
    int jstack = 0, *istack;
    struct svr a, b, temp;

    istack = ivector(1, NSTACK);

    for (;;) {

	if (ir - l < M) {
	    /* Insertion sort se l'array è piccolo */
	    for (j = l + 1; j <= ir; j++) {
		a = arr[j];
		for (i = j - 1; i >= l; i--) {
		    if (arr[i].coordX != a.coordX) {
			if (arr[i].coordX <= a.coordX)
			    break;
		    }
		    else {
			if (arr[i].coordY <= a.coordY)
			    break;
		    }
		    arr[i + 1] = arr[i];
		}
		arr[i + 1] = a;
	    }

	    if (jstack == 0)
		break;

	    ir = istack[jstack--];
	    l = istack[jstack--];
	}
	else {
	    k = (l + ir) >> 1;

	    SWAP(arr[k], arr[l + 1]);

	    if (arr[l].coordX != arr[ir].coordX) {

		if (arr[l].coordX > arr[ir].coordX) {
		    SWAP(arr[l], arr[ir]);
		}
	    }
	    else {
		if (arr[l].coordY > arr[ir].coordY) {
		    SWAP(arr[l], arr[ir]);
		}
	    }

	    if (arr[l + 1].coordX != arr[ir].coordX) {
		if (arr[l + 1].coordX > arr[ir].coordX) {
		    SWAP(arr[l + 1], arr[ir]);
		}
	    }
	    else {
		if (arr[l + 1].coordY > arr[ir].coordY) {
		    SWAP(arr[l + 1], arr[ir]);
		}
	    }

	    if (arr[l].coordX != arr[l + 1].coordX) {
		if (arr[l].coordX > arr[l + 1].coordX) {
		    SWAP(arr[l], arr[l + 1]);
		}
	    }
	    else {
		if (arr[l].coordY > arr[l + 1].coordY) {
		    SWAP(arr[l], arr[l + 1]);
		}
	    }

	    i = l + 1;
	    j = ir;
	    a = arr[l + 1];

	    for (;;) {
		for (;;) {

		    i++;

		    if (arr[i].coordX != a.coordX) {
			if (arr[i].coordX >= a.coordX)
			    break;
		    }
		    else {
			if (arr[i].coordY >= a.coordY)
			    break;
		    }
		}

		for (;;) {

		    j--;

		    if (arr[j].coordX != a.coordX) {
			if (arr[j].coordX <= a.coordX)
			    break;
		    }
		    else {
			if (arr[j].coordY <= a.coordY)
			    break;
		    }
		}

		if (j < i)
		    break;

		SWAP(arr[i], arr[j]);
	    }

	    arr[l + 1] = arr[j];
	    arr[j] = a;
	    jstack += 2;

	    if (jstack > NSTACK)
		nrerror("NSTACK too small in sort.");

	    if (ir - i + 1 >= j - l) {
		istack[jstack] = ir;
		istack[jstack - 1] = i;
		ir = j - 1;
	    }
	    else {
		istack[jstack] = j - 1;
		istack[jstack - 1] = l;
		l = i;
	    }
	}
    }

    free_ivector(istack, 1, NSTACK);
}

/*----------------------------------------------------------------------------------------------*/
/* procedura per la valutazione dei valori interpolati nelle coordinate dei punti sparsi osservati */
void puntisparsi(void)
{

    nn = 0;

    for (jj = 0; jj <= s; jj++) {

	/* Scartiamo i punti appertenenti alla cornice degli effetti di bordo */

	if (!((obsVect[jj][0] > reg_orlo_e) || (obsVect[jj][0] < reg_orlo_w) ||
	      (obsVect[jj][1] > reg_orlo_n) || (obsVect[jj][1] < reg_orlo_s))) {

	    nn++;

	    if (strcmp(type->answer, "bilinear") == 0) {
		interp =
		    dataInterpolateBilin(obsVect[jj][0], obsVect[jj][1], passoE,
					 passoN, nsplx, nsply, reg_elab.west,
					 reg_elab.south, parVect);
	    }
	    else {
		interp =
		    dataInterpolateBicubic(obsVect[jj][0], obsVect[jj][1],
					   passoE, passoN, nsplx, nsply,
					   reg_elab.west, reg_elab.south,
					   parVect);
	    }

	    interp += media;

	    /* ZONA 5 */
	    if ((obsVect[jj][0] <= reg_bordo_e) &&
		(obsVect[jj][0] >= reg_bordo_w) &&
		(obsVect[jj][1] <= reg_bordo_n) &&
		(obsVect[jj][1] >= reg_bordo_s)) {

		interpolate->east = obsVect[jj][0];
		interpolate->north = obsVect[jj][1];
		interpolate->dbl_att[0] = interp;
		G_site_put(foutput, interpolate);
	    }

	    /* ZONA 1  */
	    if ((obsVect[jj][0] > reg_orlo_w) && (obsVect[jj][0] < reg_bordo_w)
		&& (obsVect[jj][1] < reg_orlo_n) &&
		(obsVect[jj][1] > reg_bordo_n)) {

		csi = (obsVect[jj][0] - reg_orlo_w) / sovr;
		eta = (obsVect[jj][1] - reg_bordo_n) / sovr;
		peso = csi * (1 - eta);

		pos =
		    ricercaBinaria(obsVect[jj][0], obsVect[jj][1], rigaPrec,
				   C_rigaPrec);
		pos1 =
		    ricercaBinaria(obsVect[jj][0], obsVect[jj][1], trePrec,
				   C_trePrec);

		interpolate->east = obsVect[jj][0];
		interpolate->north = obsVect[jj][1];
		interpolate->dbl_att[0] =
		    ((interp * peso) + (rigaPrec[pos].quota) +
		     (trePrec[pos1].quota));

		G_site_put(foutput, interpolate);

		if ((peso > 1) || (peso < 0)) {
		    printf("Peso Sbagliato\n");
		    exit(1);
		};

		if (pos == -1) {
		    printf("Elemento non trovato\n");
		    exit(1);
		}
		if (pos1 == -1) {
		    printf("Elemento non trovato\n");
		    exit(1);
		}
	    }

	    /* ZONA 2 */
	    if ((obsVect[jj][0] >= reg_bordo_w) &&
		(obsVect[jj][0] <= reg_bordo_e) && (obsVect[jj][1] < reg_orlo_n)
		&& (obsVect[jj][1] > reg_bordo_n)) {

		peso = (reg_orlo_n - obsVect[jj][1]) / sovr;

		pos =
		    ricercaBinaria(obsVect[jj][0], obsVect[jj][1], rigaPrec,
				   C_rigaPrec);

		interpolate->east = obsVect[jj][0];
		interpolate->north = obsVect[jj][1];
		interpolate->dbl_att[0] =
		    ((interp * peso) + (rigaPrec[pos].quota));

		G_site_put(foutput, interpolate);

		if ((peso > 1) || (peso < 0)) {
		    printf("Peso Sbagliato\n");
		    exit(1);
		};

		if (pos == -1) {
		    printf("Elemento non trovato\n");
		    exit(1);
		}
	    }

	    /* ZONA 3 */
	    if ((obsVect[jj][0] > reg_bordo_e) && (obsVect[jj][0] < reg_orlo_e)
		&& (obsVect[jj][1] < reg_orlo_n) &&
		(obsVect[jj][1] > reg_bordo_n)) {

		csi = (obsVect[jj][0] - reg_bordo_e) / sovr;
		eta = (obsVect[jj][1] - reg_bordo_n) / sovr;
		peso = (1 - csi) * (1 - eta);

		if (C_treSuc >= gt6) {
		    gt6 = gt6 + 1000;
		    treSuc =
			(struct svr *)realloc(treSuc, gt6 * sizeof(struct svr));
		}

		treSuc[C_treSuc].coordX = obsVect[jj][0];
		treSuc[C_treSuc].coordY = obsVect[jj][1];
		treSuc[C_treSuc].quota = interp * peso;
		C_treSuc++;

		if ((peso > 1) || (peso < 0)) {
		    printf("Peso Sbagliato\n");
		    exit(1);
		};
	    }

	    /* ZONA 4 */
	    if ((obsVect[jj][0] > reg_orlo_w) && (obsVect[jj][0] < reg_bordo_w)
		&& (obsVect[jj][1] <= reg_bordo_n) &&
		(obsVect[jj][1] >= reg_bordo_s)) {

		peso = (obsVect[jj][0] - reg_orlo_w) / sovr;

		pos =
		    ricercaBinaria(obsVect[jj][0], obsVect[jj][1], colPrec,
				   C_colPrec);

		interpolate->east = obsVect[jj][0];
		interpolate->north = obsVect[jj][1];
		interpolate->dbl_att[0] =
		    ((interp * peso) + (colPrec[pos].quota));

		G_site_put(foutput, interpolate);

		if ((peso > 1) || (peso < 0)) {
		    printf("Peso Sbagliato\n");
		    exit(1);
		};

		if (pos == -1) {
		    printf("4_Elemento non trovato\n");
		    exit(1);
		}
	    }

	    /* ZONA 6 */
	    if ((obsVect[jj][0] > reg_bordo_e) && (obsVect[jj][0] < reg_orlo_e)
		&& (obsVect[jj][1] <= reg_bordo_n) &&
		(obsVect[jj][1] >= reg_bordo_s)) {

		peso = (reg_orlo_e - obsVect[jj][0]) / sovr;

		if (C_colSuc >= gt4) {
		    gt4 = gt4 + 1000;
		    colSuc =
			(struct svr *)realloc(colSuc, gt4 * sizeof(struct svr));
		}

		colSuc[C_colSuc].coordX = obsVect[jj][0];
		colSuc[C_colSuc].coordY = obsVect[jj][1];
		colSuc[C_colSuc].quota = interp * peso;
		C_colSuc++;

		if ((peso > 1) || (peso < 0)) {
		    printf("Peso Sbagliato\n");
		    exit(1);
		};
	    }

	    /* ZONA 7 */
	    if ((obsVect[jj][0] > reg_orlo_w) && (obsVect[jj][0] < reg_bordo_w)
		&& (obsVect[jj][1] < reg_bordo_s) &&
		(obsVect[jj][1] > reg_orlo_s)) {

		csi = (obsVect[jj][0] - reg_orlo_w) / sovr;
		eta = (obsVect[jj][1] - reg_orlo_s) / sovr;
		peso = csi * eta;

		if (C_rigaSuc >= gt2) {
		    gt2 = gt2 + 1000;
		    rigaSuc =
			(struct svr *)realloc(rigaSuc,
					      gt2 * sizeof(struct svr));
		}

		pos =
		    ricercaBinaria(obsVect[jj][0], obsVect[jj][1], novePrec,
				   C_novePrec);

		rigaSuc[C_rigaSuc].coordX = obsVect[jj][0];
		rigaSuc[C_rigaSuc].coordY = obsVect[jj][1];
		rigaSuc[C_rigaSuc].quota =
		    (interp * peso) + (novePrec[pos].quota);
		C_rigaSuc++;

		if ((peso > 1) || (peso < 0)) {
		    printf("Peso Sbagliato\n");
		    exit(1);
		}

		if (pos == -1) {
		    printf("Elemento non trovato\n");
		    exit(1);
		}

	    }

	    /* ZONA 8 */
	    if ((obsVect[jj][0] >= reg_bordo_w) &&
		(obsVect[jj][0] <= reg_bordo_e) &&
		(obsVect[jj][1] < reg_bordo_s) &&
		(obsVect[jj][1] > reg_orlo_s)) {

		peso = (obsVect[jj][1] - reg_orlo_s) / sovr;

		if (C_rigaSuc >= gt2) {
		    gt2 = gt2 + 1000;
		    rigaSuc =
			(struct svr *)realloc(rigaSuc,
					      gt2 * sizeof(struct svr));
		}

		rigaSuc[C_rigaSuc].coordX = obsVect[jj][0];
		rigaSuc[C_rigaSuc].coordY = obsVect[jj][1];
		rigaSuc[C_rigaSuc].quota = interp * peso;
		C_rigaSuc++;

		if ((peso > 1) || (peso < 0)) {
		    printf("Peso Sbagliato\n");
		    exit(1);
		}
	    }

	    /* ZONA 9 */
	    if ((obsVect[jj][0] > reg_bordo_e) && (obsVect[jj][0] < reg_orlo_e)
		&& (obsVect[jj][1] < reg_bordo_s) &&
		(obsVect[jj][1] > reg_orlo_s)) {

		csi = (obsVect[jj][0] - reg_bordo_e) / sovr;
		eta = (obsVect[jj][1] - reg_orlo_s) / sovr;
		peso = eta * (1 - csi);

		if (C_noveSuc >= gt8) {
		    gt8 = gt8 + 1000;
		    noveSuc =
			(struct svr *)realloc(noveSuc,
					      gt8 * sizeof(struct svr));
		}

		noveSuc[C_noveSuc].coordX = obsVect[jj][0];
		noveSuc[C_noveSuc].coordY = obsVect[jj][1];
		noveSuc[C_noveSuc].quota = interp * peso;
		C_noveSuc++;

		if ((peso > 1) || (peso < 0)) {
		    printf("Peso Sbagliato\n");
		    exit(1);
		}

	    }
	}			//end if obsVect       
    }				//end for
}				//end puntisparsi

/*---------------------------------------------------------------------------------------------------------*/

/*******************************************************/
		/* PUNTI GRIGLIATI */

/******************************************************/
void puntigrigliati(void)
{

    int chk;

    num++;
    strcpy(pippo, "");
    sprintf(pippo, "%d", num);
    strcpy(pluto, "");
    strcpy(pluto, "tmp");

    G_strcat(pluto, pippo);

    reg_buf.ew_res = reg_orig.ew_res;
    reg_buf.ns_res = reg_orig.ns_res;
    reg_buf.format = reg_orig.format;
    reg_buf.compressed = reg_orig.compressed;
    reg_buf.proj = reg_orig.proj;
    reg_buf.zone = reg_orig.zone;
    reg_buf.west = reg_orlo_w;
    reg_buf.east = reg_orlo_e;
    reg_buf.north = reg_orlo_n;
    reg_buf.south = reg_orlo_s;

    chk = G_set_window(&reg_buf);
    if (chk == -1) {
	printf("Region is not valid\n");
	exit(1);
    }

    reg_buf.rows = G_window_rows();
    reg_buf.cols = G_window_cols();

    raster_buf = G_allocate_raster_buf(DCELL_TYPE);	/* allocate an array based on the num of col in the current region */
    rast = raster_buf;		/* rast punta al primo elemento */

    cf = G_open_raster_new(pluto, DCELL_TYPE);

    if (cf < 0) {
	char msg[100];
	sprintf(msg, "unable to create raster map %s", output->answer);
	G_fatal_error(msg);
	exit(1);
    }

    for (row = 0; row < reg_buf.rows; row++) {

	/* Azzero raster_buf */
	for (col = 0; col < reg_buf.cols; col++) {

	    G_set_null_value(raster_buf, 1, DCELL_TYPE);
	    raster_buf = G_incr_void_ptr(raster_buf, G_raster_size(DCELL_TYPE));
	}
	raster_buf = rast;

	for (col = 0; col < reg_buf.cols; col++) {

	    coordX = G_col_to_easting((double)(col) + 0.5, &reg_buf);
	    coordY = G_row_to_northing((double)(row) + 0.5, &reg_buf);

	    if ((coordX > reg_buf.east) || (coordX < reg_buf.west) ||
		(coordY < reg_buf.south) || (coordY > reg_buf.north)) {
		printf("coordinate sbagliate >.\n");
		while (getchar() != '.') ;
	    }

	    /* Scartiamo i punti appertenenti alla cornice degli effetti di bordo */
	    if (!((coordX > reg_orlo_e) || (coordX < reg_orlo_w) ||
		  (coordY > reg_orlo_n) || (coordY < reg_orlo_s))) {

		if (strcmp(type->answer, "bilinear") == 0) {
		    interp =
			dataInterpolateBilin(coordX, coordY, passoE, passoN,
					     nsplx, nsply, reg_buf.west,
					     reg_buf.south, parVect);
		}
		else {
		    interp =
			dataInterpolateBicubic(coordX, coordY, passoE, passoN,
					       nsplx, nsply, reg_buf.west,
					       reg_buf.south, parVect);
		}

		interp += media;

		/* ZONA 5 */
		if ((coordX <= reg_bordo_e) && (coordX >= reg_bordo_w) &&
		    (coordY <= reg_bordo_n) && (coordY >= reg_bordo_s)) {

		    G_set_raster_value_d(raster_buf, (DCELL) (interp),
					 DCELL_TYPE);
		    raster_buf =
			G_incr_void_ptr(raster_buf, G_raster_size(DCELL_TYPE));
		}

		/* ZONA 1  */
		if ((coordX > reg_orlo_w) && (coordX < reg_bordo_w) &&
		    (coordY < reg_orlo_n) && (coordY > reg_bordo_n)) {

		    csi = (coordX - reg_orlo_w) / sovr;
		    eta = (coordY - reg_bordo_n) / sovr;
		    peso = csi * (1 - eta);

		    pos = ricercaBinaria(coordX, coordY, rigaPrec, C_rigaPrec);

		    if (pos == -1) {
			printf("ERRORE!\n");
			exit(1);
		    }

		    pos1 = ricercaBinaria(coordX, coordY, trePrec, C_trePrec);

		    if (pos1 == -1) {
			printf("ERRORE!\n");
			exit(1);
		    }

		    interp =
			(interp * peso) + (rigaPrec[pos].quota) +
			(trePrec[pos1].quota);

		    G_set_raster_value_d(raster_buf, (DCELL) (interp),
					 DCELL_TYPE);
		    raster_buf =
			G_incr_void_ptr(raster_buf, G_raster_size(DCELL_TYPE));

		    if ((peso > 1) || (peso < 0)) {
			printf("Peso Sbagliato\n");
			exit(1);
		    }
		}

		/* ZONA 2 */
		if ((coordX >= reg_bordo_w) && (coordX <= reg_bordo_e) &&
		    (coordY < reg_orlo_n) && (coordY > reg_bordo_n)) {

		    peso = (reg_orlo_n - coordY) / sovr;

		    pos = ricercaBinaria(coordX, coordY, rigaPrec, C_rigaPrec);

		    if (pos == -1) {
			printf("ERRORE!\n");
			exit(1);
		    }

		    interp = ((interp * peso) + (rigaPrec[pos].quota));

		    G_set_raster_value_d(raster_buf, (DCELL) (interp),
					 DCELL_TYPE);
		    raster_buf =
			G_incr_void_ptr(raster_buf, G_raster_size(DCELL_TYPE));

		    if ((peso > 1) || (peso < 0)) {
			printf("Peso Sbagliato\n");
			exit(1);
		    }
		}

		/* ZONA 3 */
		if ((coordX > reg_bordo_e) && (coordX < reg_orlo_e) &&
		    (coordY < reg_orlo_n) && (coordY > reg_bordo_n)) {

		    csi = (coordX - reg_bordo_e) / sovr;
		    eta = (coordY - reg_bordo_n) / sovr;
		    peso = (1 - csi) * (1 - eta);

		    if (C_treSuc >= gt6) {
			gt6 = gt6 + 1000;
			treSuc =
			    (struct svr *)realloc(treSuc,
						  gt6 * sizeof(struct svr));
		    }

		    treSuc[C_treSuc].coordX = coordX;
		    treSuc[C_treSuc].coordY = coordY;
		    treSuc[C_treSuc].quota = interp * peso;
		    C_treSuc++;

		    if ((peso > 1) || (peso < 0)) {
			printf("Peso Sbagliato\n");
			exit(1);
		    }
		}

		/* ZONA 4 */
		if ((coordX > reg_orlo_w) && (coordX < reg_bordo_w) &&
		    (coordY <= reg_bordo_n) && (coordY >= reg_bordo_s)) {

		    peso = (coordX - reg_orlo_w) / sovr;

		    pos = ricercaBinaria(coordX, coordY, colPrec, C_colPrec);

		    if (pos == -1) {
			printf("ERRORE!\n");
			exit(1);
		    }

		    interp = ((interp * peso) + (colPrec[pos].quota));

		    G_set_raster_value_d(raster_buf, (DCELL) (interp),
					 DCELL_TYPE);
		    raster_buf =
			G_incr_void_ptr(raster_buf, G_raster_size(DCELL_TYPE));

		    if ((peso > 1) || (peso < 0)) {
			printf("Peso Sbagliato\n");
			exit(1);
		    }
		}

		/* ZONA 6 */
		if ((coordX > reg_bordo_e) && (coordX < reg_orlo_e) &&
		    (coordY <= reg_bordo_n) && (coordY >= reg_bordo_s)) {

		    peso = (reg_orlo_e - coordX) / sovr;

		    if (C_colSuc >= gt4) {
			gt4 = gt4 + 1000;
			colSuc =
			    (struct svr *)realloc(colSuc,
						  gt4 * sizeof(struct svr));
		    }

		    colSuc[C_colSuc].coordX = coordX;
		    colSuc[C_colSuc].coordY = coordY;
		    colSuc[C_colSuc].quota = interp * peso;
		    C_colSuc++;

		    if ((peso > 1) || (peso < 0)) {
			printf("Peso Sbagliato\n");
			exit(1);
		    }
		}

		/* ZONA 7 */
		if ((coordX > reg_orlo_w) && (coordX < reg_bordo_w) &&
		    (coordY < reg_bordo_s) && (coordY > reg_orlo_s)) {

		    csi = (coordX - reg_orlo_w) / sovr;
		    eta = (coordY - reg_orlo_s) / sovr;
		    peso = csi * eta;

		    if (C_rigaSuc >= gt2) {
			gt2 = gt2 + 1000;
			rigaSuc =
			    (struct svr *)realloc(rigaSuc,
						  gt2 * sizeof(struct svr));
		    }

		    pos = ricercaBinaria(coordX, coordY, novePrec, C_novePrec);

		    if (pos == -1) {
			printf("ERRORE!\n");
			exit(1);
		    }

		    rigaSuc[C_rigaSuc].coordX = coordX;
		    rigaSuc[C_rigaSuc].coordY = coordY;
		    rigaSuc[C_rigaSuc].quota =
			(interp * peso) + (novePrec[pos].quota);
		    C_rigaSuc++;

		    if ((peso > 1) || (peso < 0)) {
			printf("7_Peso Sbagliato\n");
			exit(1);
		    }
		}

		/* ZONA 8 */
		if ((coordX >= reg_bordo_w) && (coordX <= reg_bordo_e) &&
		    (coordY < reg_bordo_s) && (coordY > reg_orlo_s)) {

		    peso = (coordY - reg_orlo_s) / sovr;

		    if (C_rigaSuc >= gt2) {
			gt2 = gt2 + 1000;
			rigaSuc =
			    (struct svr *)realloc(rigaSuc,
						  gt2 * sizeof(struct svr));
		    }

		    rigaSuc[C_rigaSuc].coordX = coordX;
		    rigaSuc[C_rigaSuc].coordY = coordY;
		    rigaSuc[C_rigaSuc].quota = interp * peso;
		    C_rigaSuc++;

		    if ((peso > 1) || (peso < 0)) {
			printf("Peso Sbagliato\n");
			exit(1);
		    }
		}

		/* ZONA 9 */
		if ((coordX > reg_bordo_e) && (coordX < reg_orlo_e) &&
		    (coordY < reg_bordo_s) && (coordY > reg_orlo_s)) {

		    csi = (coordX - reg_bordo_e) / sovr;
		    eta = (coordY - reg_orlo_s) / sovr;
		    peso = eta * (1 - csi);

		    if (C_noveSuc >= gt8) {
			gt8 = gt8 + 1000;
			noveSuc =
			    (struct svr *)realloc(noveSuc,
						  gt8 * sizeof(struct svr));
		    }

		    noveSuc[C_noveSuc].coordX = coordX;
		    noveSuc[C_noveSuc].coordY = coordY;
		    noveSuc[C_noveSuc].quota = interp * peso;
		    C_noveSuc++;

		    if ((peso > 1) || (peso < 0)) {
			printf("Peso Sbagliato\n");
			exit(1);
		    }
		}
	    }			//end if coordX
	}			//end for col
	G_put_raster_row(cf, rast, DCELL_TYPE);
	raster_buf = rast;
    }				//end for row
    G_close_cell(cf);
    G_free(raster_buf);
}
