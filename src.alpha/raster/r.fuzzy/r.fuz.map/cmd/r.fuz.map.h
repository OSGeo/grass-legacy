/* r.fuz.map.h							*/

 #include "gis.h"
#include "fichiers.h"
#include "commun.h"
#include "math.h"
#include "minmax.h"
#include "var_floues.h" 
#include <fcntl.h>
#include <dirent.h>
#include "math.h"
#include "graph_grass.h"
#include "affiche_fuzzy.h"
#include <sys/stat.h>
#include "var_floues.h"


/* h par defaut pour le calcul du seuil */
#define H_DEFAUT 0.5

/* Taille totale de la chaine pour la concatenation des */
/* chaines lors d'un calcul d'interesection		*/
/* Ce systeme n'est pas terrible, il faudrait au moins	*/
/* mettre un garde fou					*/
#define MAX_CUMUL_CHAINE 1000



void OuvreFichiers(
 char *	Raster,  /*		le nom du raster en entree	*/
 char *	CellDest,/*	le nom du raster en sortie	*/
 int  *	CellIn,  /*		ident. fichier en entree	*/
 int  *	CellOut, /*		ident. fichier en sortie	*/
 classe_floue ** Floue); /* 	classe floue chargee		*/

void TrouveNomMu(
 classe_floue *	Floue,    /*	Classe floue concernee	*/
 char *		NomVarMu, /*	Fction concernee	*/
 nb_flou *	NbFlou,	 /* 	NbFlou correspondant	*/
 char *		Force);	 /*	Nom variable forcee	*/

double Mux_total(classe_floue*Floue, /* Classe floue concernee */
CELL Cell,                          /* Valeur du pixel dans le raster */
int iForce,                          /* indice de la variable forcee */
int drapeau);                       

double Mux_simple(nb_flou NbFlou,   /* Indique la variable choisie */ 
CELL Cell,                         /* Valeur du pixel dans le raster */
int drapeau);

double CalculMuTotal(
classe_floue *Floue,    /*	Classe floue pour le calcul	*/
CELL Cell,	        /*      Valeur pour le calcul		*/
int	iForce,	        /*      indice de la variable forcee	*/
int drapeau);            /* Prise en compte ou non des valeurs nulles du raster */
          
/* Renvoie : valeur de Mu calculee				*/


double CalculMuSimple(
 nb_flou	NbFlou, /*	Classe d'apartenance			*/
 CELL		Cell,   /*	Valeur de depart du calcul		*/
 int            drapeau);
/* Renvoie : valeur de Mu calculee				*/

void CarteMu(
char * Raster,  /* 	Nom du raster de depart			*/
char * Mu,      /*	Nom du raster a creer			*/
char * NomVarMu,/*	Nom de la fonction d'appartenance pour	*/
                /*	le calcul du Mu, ou NULL		*/
char * Force,   /*	Nom de la variable forcee		*/
int drapeau);   /* Prise en compte ou non des valeurs nulles du raster */

int CalculSeuilHTotal(
classe_floue *	Flou,        /* 	La classe concernee		*/
CELL	        Cell,        /* 	Valeur de depart du calcul	*/
double	     	SeuilF,      /* 	Seuil pour le calcul		*/
int		iForce,       /* 	indice de la variable forcee	*/
int drapeau);

int CalculSeuilHSimple(
nb_flou  NbFlou       ,        /*  La fonction d'appartenance concernee   */
CELL	        Cell,        /* 	Valeur de depart du calcul	*/
double	     	SeuilF,      /* 	Seuil pour le calcul		*/    
int drapeau);

void CarteClass(
char * Raster, /*	Nom du raster de depart			*/
char * Class,  /*      	Nom du raster a creer			*/
char * Set,    /*       Nom de la fonction d'appartenance forcee*/
char *  Seuil,  /* 	Valeur de H (Seuil) ou NULL		*/
char * Force,   /*	Nom de la variable forcee		*/
int drapeau);

int CalculInter(
classe_floue *	    Flou,  /*	La classe concernee		*/
CELL		    Cell,  /*	Valeur de depart du calcul	*/
struct Categories * pCats, /*	Categories a modifier		*/
int		    iForce, /*	indice de la variable forcee	*/
int drapeau,
int v);
/* Renvoie : le numero de la categorie cree			*/

void CarteInter(
char * Raster, /*	Nom du raster de depart			*/
char * Inter,  /*	Nom du raster a creer			*/
char * Force,  /*	Nom de la variable forcee		*/
int drapeau,   /*       No_data ou data pour 0                  */
int v);        /*   Visualisation des zones d'intersections     */

int IndexForce(
char *		Force, /*nom de la variable dont on	*/
                       /* cherche l'indice		*/
classe_floue *	Floue);/*	dans cette classe		*/

void inter(char * nom);
/* nom correspond au nom du raster choisi */

 
