/*
 * main.c
 * s.bordilaser
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

#include "site.h"
#include "nrutil0.h"
#include "tcholband.h"
#include "interpspline.h"

/* definizione dei parametri per dimensionare la sottoarea da eleborare */
#define nsplx_max 200		//max numero di spline da utilizzare per sottoelaborazioni lungo E
#define nsply_max 200		//max numero di spline da utilizzare per sottoelaborazioni lungo N
#define LUNG 50

#define pi 3.141592

#define true 1
#define false 0

#define SWAP(a,b) temp=(a); (a)=(b); (b)=temp;
#define NSTACK 50
#define M 7
#define contur 15		//contorno della sottozona su cui calcolare la media

FILE *finput, *fout_clas;
Site *interpolate, *orig, *clas;
double *TN, *Q, *parVect_bil, *parVect_bic, *x, *y, *z, interp, peso, csi, eta;
double **N, **obsVect;
int num_spline, num_spline_E, num_spline_N, num_obs, BW, reg, nr, nc, kk, jj,
    parNum, col_reg, row_reg, c1, c2;
int reg_c, reg_r, ss, nsplx, nsply;
double passoE, passoN, latoE, latoN, lambda, media, sovr;
char *me;
double coordX, coordY, orlo_h, orlo_v;
double reg_orlo_n, reg_orlo_s, reg_orlo_e, reg_orlo_w;
double reg_bordo_n, reg_bordo_s, reg_bordo_e, reg_bordo_w;

double gradPto, scarto, edge;

double lambda_bil, gradHigh, gradLow;

int last_col, last_row;

char nomesite[LUNG], nomemapset[LUNG];
int dim_vect, dim_vect_max;	//num di osservazioni a volta tenute in memoria

double mediaOrig, sqmOrig, mediaInterp, sqmInterp, mediaDiff, sqmDiff;

struct Option *input, *out_clas, *passo_E, *passo_N, *lambda_f, *type,
    *lambda_B, *grad_H, *grad_L, *alfa;
struct Flag *grid;
struct Cell_head reg_orig, reg_elab;

struct svr
{
    double coordX;
    double coordY;
    double quota;

    double grad[2];
};

struct svr *rigaPrec, *rigaSuc, *colPrec, *colSuc, *trePrec, *treSuc, *novePrec,
    *noveSuc;
long C_rigaPrec, C_rigaSuc, C_colPrec, C_colSuc, C_trePrec, C_treSuc,
    C_novePrec, C_noveSuc, pos, pos1;
long gt1, gt2, gt3, gt4, gt5, gt6, gt7, gt8;

int dims, cat, strs, dbls;	//variabili per descrizione dei sites
int lung, j, i, s;		//variabili per ricerca del nome site file

double grad[2], dirPto;

double g[9][3];
double r, alpha;

/* Contatori */
int conta, mm, c1, c2;

/***************************************************************************************/
unsigned long ricercaBinaria(double cX, double cY, struct svr *vett,
			     unsigned long lungList);
void quickSortList(unsigned long n, struct svr *arr);
void puntisparsi(void);
void puntigrigliati(void);
void classification(void);
void gradient(double grad[], double X, double Y, double *parVect);
void edge_detection(void);

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

    out_clas = G_define_option();
    out_clas->key = "outClas";
    out_clas->type = TYPE_STRING;
    out_clas->required = YES;
    out_clas->description = "output classify sites list file name";
    out_clas->gisprompt = "new,site_lists,sites";

    passo_E = G_define_option();
    passo_E->key = "Sge";
    passo_E->type = TYPE_DOUBLE;
    passo_E->required = YES;
    passo_E->description = "interpolation spline step value in east direction";

    passo_N = G_define_option();
    passo_N->key = "Sgn";
    passo_N->type = TYPE_DOUBLE;
    passo_N->required = YES;
    passo_N->description = "interpolation spline step value in north direction";

    lambda_B = G_define_option();
    lambda_B->key = "lambda_g";
    lambda_B->type = TYPE_DOUBLE;
    lambda_B->required = NO;
    lambda_B->description = "regualarization weight in gradient evaluation";
    lambda_B->answer = "0.01";

    grad_H = G_define_option();
    grad_H->key = "Tg";
    grad_H->type = TYPE_DOUBLE;
    grad_H->required = NO;
    grad_H->description = "high gradient threshold for edge classification";
    grad_H->answer = "6";

    grad_L = G_define_option();
    grad_L->key = "tg";
    grad_L->type = TYPE_DOUBLE;
    grad_L->required = NO;
    grad_L->description = "low gradient threshold for edge classification";
    grad_L->answer = "3";

    alfa = G_define_option();
    alfa->key = "theta_g";
    alfa->type = TYPE_DOUBLE;
    alfa->required = NO;
    alfa->description = "angle range for same direction detection";
    alfa->answer = "0.26";

    lambda_f = G_define_option();
    lambda_f->key = "lambda_r";
    lambda_f->type = TYPE_DOUBLE;
    lambda_f->required = NO;
    lambda_f->description = "regualarization weight in residual evaluation";
    lambda_f->answer = "2";

    if (G_parser(argc, argv))
	exit(1);

    sscanf(lambda_f->answer, "%lf", &lambda);
    sscanf(passo_E->answer, "%lf", &passoE);
    sscanf(passo_N->answer, "%lf", &passoN);
    sscanf(lambda_B->answer, "%lf", &lambda_bil);
    sscanf(grad_H->answer, "%lf", &gradHigh);
    sscanf(grad_L->answer, "%lf", &gradLow);

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

    clas = G_site_new_struct(cat, dims, strs, 3);
    orig = G_site_new_struct(cat, dims, strs, dbls);

    fout_clas = G_sites_open_new(out_clas->answer);

    if (out_clas->answer == NULL) {
	fprintf(stderr, " %s - can't create sites file [%s]", me,
		out_clas->answer);
	exit(1);
    }

/******************************************/
    /* SELEZIONE DELLE SOTTOZONE DA ELABORARE */

/******************************************/

    sovr = passoE * 10;
    r = passoE;

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

    /*assegna i parametri fissi delle sottoregioni da utilizzare per elaborazione */
    reg_elab.ew_res = (reg_orig.ew_res);
    reg_elab.ns_res = (reg_orig.ns_res);
    reg_elab.format = reg_orig.format;
    reg_elab.compressed = reg_orig.compressed;
    reg_elab.proj = reg_orig.proj;
    reg_elab.zone = reg_orig.zone;

    /*valuta i lati delle sottozone massime da elaborare */
    latoE = nsplx_max * passoE;
    latoN = nsply_max * passoN;

    /* non serve!!!!calcolo numero righe e colonne di spline sotto regioni da elaborare */
    col_reg = ceil((reg_orig.east - reg_orig.west) / latoE);
    row_reg = ceil((reg_orig.north - reg_orig.south) / latoN);

    /*calcolo dell'orlo delle sottozone */
    orlo_v = 4 * passoE;
    orlo_h = 4 * passoN;

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

	}
	if (reg_elab.south <= reg_orig.south) {

	    reg_elab.south = reg_orig.south;
	    reg_orlo_s = reg_elab.south;
	    reg_bordo_s = reg_elab.south;
	    last_row = true;

	    nsply = ceil((reg_elab.north - reg_elab.south) / passoN) + 1;
	}

	reg_elab.rows = G_window_cols();
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
	    }

	    if (reg_elab.east >= reg_orig.east) {

		reg_elab.east = reg_orig.east;
		reg_orlo_e = reg_elab.east;
		reg_bordo_e = reg_elab.east;

		last_col = true;

		nsplx = ceil((reg_elab.east - reg_elab.west) / passoE) + 1;
	    }

	    reg_elab.cols = G_window_rows();
	    reg_c++;

	    dim_vect = dim_vect_max;

	    /* allocazione vettori delle osservazioni */
	    x = (double *)calloc(dim_vect, sizeof(double));
	    y = (double *)calloc(dim_vect, sizeof(double));
	    z = (double *)calloc(dim_vect, sizeof(double));

	    /*seleziona i punti del site delle osservazioni che cadono nella regione e li scrive nella matrice obsVect */
	    s = -1;

	    fseek(finput, 0L, SEEK_SET);

	    while (G_site_get(finput, orig) != -1) {
		if (G_site_in_region(orig, &reg_elab) == 1) {

		    s++;

		    if (s >= dim_vect) {
			dim_vect += dim_vect_max;

			x = (double *)realloc(x, dim_vect * sizeof(double));
			y = (double *)realloc(y, dim_vect * sizeof(double));
			z = (double *)realloc(z, dim_vect * sizeof(double));

		    }

		    x[s] = orig->east;
		    y[s] = orig->north;
		    z[s] = orig->dbl_att[0];

		}		//end if
	    }			//end while G_get_site

	    // calcolo della media del bordo da sottarre prima dell'interpolazione
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

	    if (s > 0) {

		/* allocazione vettori e matrici per input procedura interpolazione */
		parNum = nsplx * nsply;
		BW = 2 * nsply + 1;

		/* allocazione matrici e vettori */
		N = dmatrix(0, parNum - 1, 0, BW - 1);
		TN = dvector(0, parNum - 1);
		parVect_bil = dvector(0, parNum - 1);
		parVect_bic = dvector(0, parNum - 1);
		obsVect = dmatrix(0, s, 0, 2);
		Q = dvector(0, s);

		/* creazione del vettore delle osservazioni */
		for (ss = 0; ss <= s; ss++) {

		    Q[ss] = 1;

		    obsVect[ss][0] = x[ss];
		    obsVect[ss][1] = y[ss];
		    obsVect[ss][2] = z[ss] - media;
		}

		free(x);
		free(y);
		free(z);

		normalDefBilin(N, TN, Q, obsVect, passoE, passoN, nsplx, nsply,
			       reg_elab.west, reg_elab.south, s, parNum, BW);
		nCorrectGrad(N, lambda_bil, nsplx, nsply, passoE, passoN);
		tcholSolve(N, TN, parVect_bil, parNum, BW);

		/* deallocazione vettori e matrici */
		free_dmatrix(N, 0, parNum - 1, 0, BW - 1);

		for (ss = 0; ss <= parNum - 1; ss++) {
		    TN[ss] = 0;
		}

		BW = 4 * nsply + 3;
		N = dmatrix(0, parNum - 1, 0, BW - 1);

		normalDefBicubic(N, TN, Q, obsVect, passoE, passoN, nsplx,
				 nsply, reg_elab.west, reg_elab.south, s,
				 parNum, BW);
		nCorrectLapl(N, lambda, nsplx, nsply, passoE, passoN);
		tcholSolve(N, TN, parVect_bic, parNum, BW);

		free_dmatrix(N, 0, parNum - 1, 0, BW - 1);
		free_dvector(TN, 0, parNum - 1);
		free_dvector(Q, 0, s);

		classification();

		free_dvector(parVect_bic, 0, parNum - 1);
		free_dvector(parVect_bil, 0, parNum - 1);
		free_dmatrix(obsVect, 0, s, 0, 2);

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

    }				//end while row x regioni da elaborare

    G_site_free_struct(interpolate);
    G_site_free_struct(clas);
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
void classification(void)
{

    for (jj = 0; jj <= s; jj++) {	//punti sparsi

	/* Scartiamo i punti appertenenti alla cornice degli effetti di bordo */
	if (!((obsVect[jj][0] > reg_orlo_e) || (obsVect[jj][0] < reg_orlo_w) ||
	      (obsVect[jj][1] > reg_orlo_n) || (obsVect[jj][1] < reg_orlo_s))) {

	    interp =
		dataInterpolateBicubic(obsVect[jj][0], obsVect[jj][1], passoE,
				       passoN, nsplx, nsply, reg_elab.west,
				       reg_elab.south, parVect_bic);
	    interp += media;

	    gradient(grad, obsVect[jj][0], obsVect[jj][1], parVect_bil);

	    /* ZONA 5 */
	    if ((obsVect[jj][0] <= reg_bordo_e) &&
		(obsVect[jj][0] >= reg_bordo_w) &&
		(obsVect[jj][1] <= reg_bordo_n) &&
		(obsVect[jj][1] >= reg_bordo_s)) {

		clas->east = obsVect[jj][0];	// x
		clas->north = obsVect[jj][1];	// y
		clas->dbl_att[0] = obsVect[jj][2] + media;	// z

		scarto = obsVect[jj][2] + media - interp;
		edge_detection();

		clas->dbl_att[1] = edge;	// Classificazione
		clas->dbl_att[2] = interp;

		G_site_put(fout_clas, clas);
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
		if (pos == -1) {
		    printf("ERRORE!\n");
		    exit(1);
		}

		pos1 =
		    ricercaBinaria(obsVect[jj][0], obsVect[jj][1], trePrec,
				   C_trePrec);
		if (pos1 == -1) {
		    printf("ERRORE!\n");
		    exit(1);
		}

		clas->east = obsVect[jj][0];	// x
		clas->north = obsVect[jj][1];	// y
		clas->dbl_att[0] = obsVect[jj][2] + media;	// z

		interp =
		    ((interp * peso) + (rigaPrec[pos].quota) +
		     (trePrec[pos1].quota));
		grad[0] =
		    ((grad[0] * peso) + (rigaPrec[pos].grad[0]) +
		     (trePrec[pos1].grad[0]));
		grad[1] =
		    ((grad[1] * peso) + (rigaPrec[pos].grad[1]) +
		     (trePrec[pos1].grad[1]));

		scarto = obsVect[jj][2] + media - interp;
		edge_detection();

		clas->dbl_att[1] = edge;	// Classificazione
		clas->dbl_att[2] = interp;

		G_site_put(fout_clas, clas);
	    }

	    /* ZONA 2 */
	    if ((obsVect[jj][0] >= reg_bordo_w) &&
		(obsVect[jj][0] <= reg_bordo_e) && (obsVect[jj][1] < reg_orlo_n)
		&& (obsVect[jj][1] > reg_bordo_n)) {

		peso = (reg_orlo_n - obsVect[jj][1]) / sovr;

		pos =
		    ricercaBinaria(obsVect[jj][0], obsVect[jj][1], rigaPrec,
				   C_rigaPrec);

		if (pos == -1) {
		    printf("ERRORE!\n");
		    exit(1);
		}

		clas->east = obsVect[jj][0];	// x
		clas->north = obsVect[jj][1];	// y
		clas->dbl_att[0] = obsVect[jj][2] + media;	// z

		interp = ((interp * peso) + (rigaPrec[pos].quota));
		grad[0] = ((grad[0] * peso) + (rigaPrec[pos].grad[0]));
		grad[1] = ((grad[1] * peso) + (rigaPrec[pos].grad[1]));

		scarto = obsVect[jj][2] + media - interp;
		edge_detection();

		clas->dbl_att[1] = edge;	// Classificazione
		clas->dbl_att[2] = interp;

		G_site_put(fout_clas, clas);
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
		treSuc[C_treSuc].grad[0] = grad[0] * peso;
		treSuc[C_treSuc].grad[1] = grad[1] * peso;

		C_treSuc++;
	    }

	    /* ZONA 4 */
	    if ((obsVect[jj][0] > reg_orlo_w) && (obsVect[jj][0] < reg_bordo_w)
		&& (obsVect[jj][1] <= reg_bordo_n) &&
		(obsVect[jj][1] >= reg_bordo_s)) {

		peso = (obsVect[jj][0] - reg_orlo_w) / sovr;

		pos =
		    ricercaBinaria(obsVect[jj][0], obsVect[jj][1], colPrec,
				   C_colPrec);

		if (pos == -1) {
		    printf("ERRORE!\n");
		    exit(1);
		}

		clas->east = obsVect[jj][0];	// x
		clas->north = obsVect[jj][1];	// y
		clas->dbl_att[0] = obsVect[jj][2] + media;	// z

		interp = ((interp * peso) + (colPrec[pos].quota));
		grad[0] = ((grad[0] * peso) + (colPrec[pos].grad[0]));
		grad[1] = ((grad[1] * peso) + (colPrec[pos].grad[1]));

		scarto = obsVect[jj][2] + media - interp;
		edge_detection();

		clas->dbl_att[1] = edge;	// Classificazione
		clas->dbl_att[2] = interp;

		G_site_put(fout_clas, clas);
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
		colSuc[C_colSuc].grad[0] = grad[0] * peso;
		colSuc[C_colSuc].grad[1] = grad[1] * peso;
		C_colSuc++;
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

		rigaSuc[C_rigaSuc].grad[0] =
		    (grad[0] * peso) + (novePrec[pos].grad[0]);
		rigaSuc[C_rigaSuc].grad[1] =
		    (grad[1] * peso) + (novePrec[pos].grad[1]);

		C_rigaSuc++;
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

		rigaSuc[C_rigaSuc].grad[0] = grad[0] * peso;
		rigaSuc[C_rigaSuc].grad[1] = grad[1] * peso;

		C_rigaSuc++;
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

		noveSuc[C_noveSuc].grad[0] = grad[0] * peso;
		noveSuc[C_noveSuc].grad[1] = grad[1] * peso;

		C_noveSuc++;
	    }

	}			//end if obsVect

    }				//end for

}				//end puntisparsi

/**********************************************/
/*rows gradient calculation on bicubic surface */

/**********************************************/

void gradient(double grad[], double X, double Y, double *parVect)
{
    int riga, colonna, N;
    double csi, eta, d, b, a, c;

    riga = (int)((Y - reg_elab.south) / passoE);
    colonna = (int)((X - reg_elab.west) / passoN);
    N = nsply * colonna + riga;
    eta = X - (reg_elab.west + (colonna * passoE));
    csi = Y - (reg_elab.south + (riga * passoN));
    d = parVect[N];
    b = parVect[N + 1] - d;
    a = parVect[N + nsply] - d;
    c = parVect[N + 1 + nsply] - a - b - d;
    grad[0] = (a + c * eta);
    grad[1] = (b + c * csi);
}

/******************************************************/
/*             classificazione dei bordi              */

/******************************************************/
void edge_detection()
{
    // 0 = terreno
    // 1 = bordo di oggetto
    // 2 = incerto

    /* Notazione: intorno del punto 0
     * 
     * 1 2 3
     * 4 0 5
     * 6 7 8
     * 
     */

    g[0][0] = grad[0];
    g[0][1] = grad[1];

    gradPto = sqrt(g[0][0] * g[0][0] + g[0][1] * g[0][1]);
    dirPto = atan(g[0][1] / g[0][0]) + pi / 2;	// radianti

    if ((gradPto > gradHigh) && (scarto > 0)) {
	edge = 1;
    }
    else if ((gradPto > gradLow) && (scarto > 0)) {

	if ((obsVect[jj][0] < reg_bordo_e) && (obsVect[jj][0] > reg_bordo_w) &&
	    (obsVect[jj][1] < reg_bordo_n) && (obsVect[jj][1] > reg_bordo_s)) {

	    gradient(grad, obsVect[jj][0] + r * cos(dirPto),
		     obsVect[jj][1] + r * sin(dirPto), parVect_bil);
	    g[2][0] = grad[0];
	    g[2][1] = grad[1];

	    gradient(grad, obsVect[jj][0] + r * cos(dirPto + pi),
		     obsVect[jj][1] + r * sin(dirPto + pi), parVect_bil);
	    g[7][0] = grad[0];
	    g[7][1] = grad[1];

	    if ((abs(atan(g[2][1] / g[2][0]) + pi / 2 - dirPto) < alpha) &&
		(abs(atan(g[7][1] / g[7][0]) + pi / 2 - dirPto) < alpha)) {

		gradient(grad, obsVect[jj][0] + r * cos(dirPto + pi / 4),
			 obsVect[jj][1] + r * sin(dirPto + pi / 4),
			 parVect_bil);
		g[1][0] = grad[0];
		g[1][1] = grad[1];

		gradient(grad, obsVect[jj][0] + r * cos(dirPto - pi / 4),
			 obsVect[jj][1] + r * sin(dirPto - pi / 4),
			 parVect_bil);
		g[3][0] = grad[0];
		g[3][1] = grad[1];

		gradient(grad, obsVect[jj][0] + r * cos(dirPto + pi / 2),
			 obsVect[jj][1] + r * sin(dirPto + pi / 2),
			 parVect_bil);
		g[4][0] = grad[0];
		g[4][1] = grad[1];

		gradient(grad, obsVect[jj][0] + r * cos(dirPto - pi / 2),
			 obsVect[jj][1] + r * sin(dirPto - pi / 2),
			 parVect_bil);
		g[5][0] = grad[0];
		g[5][1] = grad[1];

		gradient(grad, obsVect[jj][0] + r * cos(dirPto + pi * 3 / 4),
			 obsVect[jj][1] + r * sin(dirPto + pi * 3 / 4),
			 parVect_bil);
		g[6][0] = grad[0];
		g[6][1] = grad[1];

		gradient(grad, obsVect[jj][0] + r * cos(dirPto - pi * 3 / 4),
			 obsVect[jj][1] + r * sin(dirPto - pi * 3 / 4),
			 parVect_bil);
		g[8][0] = grad[0];
		g[8][1] = grad[1];

		c2 = 0;
		for (c1 = 0; c1 < 9; c1++) {
		    if (sqrt(g[c1][0] * g[c1][0] + g[c1][1] * g[c1][1]) >
			gradHigh)
			c2++;
		}

		if (c2 > 2)
		    edge = 1;
		else
		    edge = 0;

	    }
	    else {
		edge = 0;
	    }

	}
	else {
	    edge = 2;
	}

    }
    else {
	edge = 0;
    }

}




void edge_detectionSOLOGRADIENTE()
{
    // 0 = terreno
    // 1 = bordo di oggetto
    // 2 = incerto

    /* Notazione: intorno del punto 0
     * 
     * 1 2 3
     * 4 0 5
     * 6 7 8
     * 
     */

    g[0][0] = grad[0];
    g[0][1] = grad[1];

    gradPto = sqrt(g[0][0] * g[0][0] + g[0][1] * g[0][1]);
    dirPto = atan(g[0][1] / g[0][0]) + pi / 2;	// radianti

    if ((gradPto > gradHigh)) {
	edge = 1;
    }
    else {
	edge = 0;
    }

}
