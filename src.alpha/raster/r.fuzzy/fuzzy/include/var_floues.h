/**************************************************************************/
/*          include pour les variables floues : var_floues.c              */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                                                                        */
/*                   derniere modif : 08/96                               */
/**************************************************************************/
#ifndef INT_var_floues
#define INT_var_floues

#include "nb_flous.h"
/* MODIF 02-12-97 MOREL L */
#include "maths.h" /* pr nash */

/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/

/********************** type variable floue *******************************/
typedef struct node
	{
	  nb_flou      nombre;        /* noeud d'une liste qui contient */
	  struct node * suiv;          /* un nombre flou                 */
	}node;

typedef struct varia_floue	 	 	  
	{
	  node * liste_elem;          /* tete de la liste des nb flous */
          char * name;                /* nom de la variable            */
	  /* MODIF 01-12-97 MOREL L */
	  int	 reinjecter;	      /* Permet de savoir si cette variable
					 est reinjecter rempli de 0 ou 1 */
	  double val_sortie;	      /* Stockage de la sortie de deflouification
					 pour la reinjection si necessaire */
	  nash_ * nash;		      /* nash ensemble de point pour different
					 calcul */
	  /* MODIF 18-12-97 MOREL L. */
	  double val_obs;	      /* Stockage de la valeur obs lu ds le 
					 fichier des donnees pour une var 
					 de sortie */

	  double val_sortieOld;	      /* Stockage de l'avant derniere sortie de deflouification
					 ecrire ds le fichier resultat si necessaire */

	}varia_floue;


/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/

varia_floue * cree_variable_floue(char * nom);
  /* sorties: varia_floue * result */
  /* fonction qui cree la variable floue NOM . Affiche un message d'     */
  /* erreur et renvoi NULL s'il y a un probleme d'allocation .           */


void ajout_nb_flou(varia_floue * varia,
		   nb_flou * nombre,
		   erreur * err);
  /* Fonction qui ajoute le nombre flou NOMBRE dans la liste de VARIA    */


void dessin_varia_floue( varia_floue * floue);
		/*	 int           color */
    /* procedure de tracage de la variable floue FLOUE dans la couleur   */
    /* COLOR                                                             */



#endif
