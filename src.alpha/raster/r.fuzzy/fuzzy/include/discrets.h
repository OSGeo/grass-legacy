/**************************************************************************/
/*          include pour les ensembles discrets : discrets.c              */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                                                                        */
/*                   derniere modif : 08/96                               */
/**************************************************************************/
#ifndef INT_discrets
#define INT_discrets



/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/

/********************** tableau de discretisation ************************/

typedef struct point
	{
 	  double         x;
 	  double         y;
          struct point * suiv;
 	}point;

typedef point * discret;


/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/

/* Morel L. Fonctions */
/**************************************************************************/
/************** Liberation des ptrs de la discretisation  *****************/
/**************************************************************************/
void LibereDiscret(discret Dis);
/* sorties :  rien			*/
/* Parcours la liste des points de Dis  et les liberes un par un les cases memoire */
/* par appel recursif */


void dessin_discret(discret dis,
		    int     color);
    /* procedure de tracage du tableau discret DIS dans la couleur COLOR */


void mult_sca(discret dis,
	      double  x);
  /* sorties: discret dis */
  /* fonction qui multiplie les ordonnees de dis par x */


void app_discret(double * rep,
		 discret  dis ,
		 double   x);
  /* sorties: double * rep */
  /* calcul de l'appartenance de X a DIS et place la reponse dans REP */
  

discret min_discret(discret dis,
		    discret dis1);
  /* sorties: discret result */
  /* fonction qui renvoi dans RESULT le minimum des deux entrees */


discret max_discret(discret dis,
		    discret dis1);
  /* sorties: discret result */
  /* fonction qui renvoi dans RESULT le maximum des deux entrees */


discret prod_discret(discret dis,
		     discret dis1);
  /* sorties: discret result */
  /* fonction qui renvoi dans RESULT le produit des deux entrees */



void ajout_fin(discret * dis,
	       double  x,
	       double  y);
  /* sorties: discret dis */
  /* fonction qui ajoute en fin de liste le point (X,Y) a DIS. ATTENTION: */
  /* la fonction ne verifie pas que X>xmax de DIS.                        */


discret copie(discret dis);
  /* sorties: discret result */
  /* fonction qui recopie DIS dans RESULT. La sortie est independante de */
  /* l'entree: un changement sur DIS ne change rien dans RESULT.         */


discret comb_minimum(discret dis1,
		     double  mu1,
		     discret dis2,
		     double  mu2);
  /* sorties: discret result */
  /* fonction qui combine DIS1 et DIS2 par la methode du minimum */
 
void ecrete(discret dis,
	    double  mu);
  /* sorties: discret dis */
  /* fonction qui ne conserve de DIS que ce qui est inferieur e MU */


discret comb_mini_crete(discret dis1,
			double  mu1,
			discret dis2,
			double  mu2);
  /* sorties: discret result */
  /* fonction qui combine DIS1 et DIS2 par la methode du minimum a crete */


discret comb_maximum(discret dis1,
		     double  mu1,
		     discret dis2,
		     double  mu2);
  /* sorties: discret result */
  /* fonction qui combine DIS1 et DIS2 par la methode du maximum */
 
discret comb_maxi_crete(discret dis1,
			double  mu1,
			discret dis2,
			double  mu2);
  /* sorties: discret result */
  /* fonction qui combine DIS1 et DIS2 par la methode du maximum a crete */


discret comb_somme_pond(discret dis1,
			double  mu1,
			discret dis2,
			double  mu2);
  /* sorties: discret result */
  /* fonction qui combine DIS1 et DIS2 par la methode de la somme ponderee */
 

double integre(discret dis,
	       double  min,
	       double  max );
  /* sorties: double  rep */
  /* calcule l'integrale de DIS entre MIN et MAX */

double integre_pond(discret dis,
		    double  min,
		    double  max);
  /* sorties: double  rep */
  /* calcule l'integrale de X*DIS entre MIN et MAX */


double deflou_gravite(discret dis,
		      int     dessin );
  /* sorties: double  rep    */
  /* si dessin = 1 , on affiche l'ensemble flou         */
  /* deflouifie DIS par la methode du centre de gravite */


double deflou_max(discret dis,
		  int     dessin);
  /* sorties: double  rep    */
  /* deflouifie DIS par la methode des maximums */
 /* si dessin = 1 , on affiche l'ensemble flou  */

double deflou_median(discret dis,
		     double  erreur,
		     int     dessin );
  /* sorties: double  rep    */
  /* deflouifie DIS par la methode du point median . ERREUR est l'erreur */
  /* acceptee entre les deux integrales                                  */
  /* si dessin = 1 , on affiche l'ensemble flou                          */


#endif



