/* r.fuz.rule.h					*/

#include <string.h>				
#include <stdlib.h>
#include <stdio.h>
#include "gis.h"
#include "fichiers.h"
#include "commun.h"
#include "E_S.h"
#include "fuzerreur.h"



#define MAX_OPEN_FILE 64

#define Const 100

void Lire_Regles( char * Fichier_Regles, /* Nom du fichier des regles */
		  classe_floue * classe, /* Liste des variables et des fonctions */
		                         /* d appartenance */
		  pile * post,           /* Pile contenant le numero de la var et */
					 /* celui associe a l operateur booleen */
		  list_regle ** regles); /* Liste des regles */

void Lire_variables(char * Fichier_Regles, /* Nom du fichier des regles */
		    char ** variables,     /* Tableau contenat les variables 
					   /* declarees dans la regle */
		    int nb_rast);          /* nombre de fichier raster traite */

void Lire_Fichier_Rasters(char ** Rasters,      /* noms des rasters */
			  char * Sortie,        /* noms du rasters a generer */
			  classe_floue * Floue, /* Liste des variables et des */
			                        /* fonctions d appartenance */
			  int nb_rast,          /* nombre de fichier raster traite */
			  char ** var);         /* Nom des variables utilisees */  

void Traitement_donnees(classe_floue * classe, /* Liste des variables et des */
					       /* fonctions d appartenance */
			pile*post,             /* Pile contenant le numero de la  */
			                       /* var et celui associe a  */
			                       /* l operateur booleen */
			list_regle ** regles,  /* Liste des regles */
			char * p_Sortie,       /* Nom du raster de sortie */
			char ** Raster,        /* noms des rasters */
			int config,            /* Choix pour la modelisation */
			int defuz,             /* Choix pour la deflouification */
			int comb,              /* Choix pour la combinaison */
			char * Mu,             /* Mon de la carte de mesure du flou */
			                       /* avec les mu max */
			char * Dist,           /* Mon de la carte de mesure du flou */
			                       /* avec la mesure de l indice du flou */
			int drapeau);          /* balise pour le traitement des zero */

sol * Calcul_DOF(pile * post,            /* Pile contenant le numero de la var */
		                         /*et celui associe a l operateur booleen */
		 list_regle * regles,    /* Liste des regles */
		 int config,             /* Choix pour la modelisation */
		 classe_floue * classe); /* Liste des variables et des fonctions */
                                         /* d appartenance */

double Eval_dof( pile * post,            /* Pile contenant le numero de la var */
		                         /* et celui associe a l operateur booleen */
		 info * reg_act,         /* Contient les info pour chaque */
		                         /* fonction d appartenance */
		 int config,             /* Choix pour la modelisation */
		 classe_floue * classe); /* Liste des variables et des fonctions */
                                         /* d appartenance */

double Indice_flou ( discret tab_dis );/* Tableau contenant les points pour */
				       /* la discretisation */
 /* sorties: int result */
 /* fonction qui retourne (1-((1/|F|)*somm(|mu(x)-(1-mu(x)))) */
