/**************************************************************************/
/*          include pour les nombres floues : nb_floues.c                 */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                                                                        */
/*                   derniere modif : 08/96                               */
/**************************************************************************/
#ifndef INT_nb_flous
#define INT_nb_flous

#include "erreur.h"
#include "discrets.h"
#include <stdio.h>
#include <stdlib.h>

/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/


/********************** type nombre flou **********************************/
typedef enum class
	  {
	    trian,                /* quatre types possibles pour un nombre */
 	    trap,                 /* flou : triangulaire, trapezoidal, LR */
            lr,
	    lrg,
	  }class;                  

typedef double triangle[3];       /* tableau de points qui definit chaque */
typedef double trapeze[4];        /* type de nb flou                      */
typedef double LR[5];
typedef double LRG[6];

typedef struct nb_flou
	{
	  char * name;                 /* nom du nombre                 */
	  class  type;                 /* type du nombre flou           */
	  union clas                   /* classe de ce nombre =         */
	    {
	      triangle tri;       /* a1, a2, a3 = bornes du nombre */
	      trapeze  tra;       /* a1, a2, a3, a4 = ------------ */
	      LR       L;         /* a1, a2, a3, p, q = ---------- */
	      LRG      Lrg;       /* a1, a2, a3, a4, p, q = ---------- */
	    }classe;
	}nb_flou;


/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/

nb_flou * genere_triangle( double    a1,
			   double    a2,
			   double    a3,     
			   char    * nom);    
  /* sorties: nb_flou * result */
  /* a1,a2,a3 correspondant aux bornes du triangle . Fonction qui alloue */
  /* la place memoire necessaire au nombre flou de type triangle et de   */
  /* nom NOM . Affiche un message d'erreur et renvoi NULL s'il y a un    */
  /* probleme .                                                          */
   
nb_flou * genere_trapeze(double    a1,
			 double    a2,
			 double    a3,
			 double    a4,
			 char    * nom);
  /* sorties: nb_flou * result */
  /* a1,a2,a3,a4 correspondant aux bornes du trapeze.Fonction qui alloue */
  /* la place memoire necessaire au nombre flou de type trapeze et de    */
  /* nom NOM . Affiche un message d'erreur et renvoi NULL s'il y a un    */
  /* probleme . ATTENTION: entrer les bornes dans l'ordre .              */


nb_flou * genere_LR(double    a1,
		    double    a2,
		    double    a3,     
		    double    p,
		    double    q,
		    char    * nom);
  /* sorties: nb_flou * result */
  /* a1,a2,a3 correspondant aux bornes du triangle et p,q les coeff      */
  /* de la focntion LR .Fonction qui alloue                              */
  /* la place memoire necessaire au nombre flou de type LR et de         */
  /* nom NOM . Affiche un message d'erreur et renvoi NULL s'il y a un    */
  /* probleme . ATTENTION: entrer les bornes dans l'ordre .              */

nb_flou * genere_LRG(double    a1,
		     double    a2,
		     double    a3,
		     double    a4,
		     double    p,
		     double    q,
		     char    * nom);
  /* sorties: nb_flou * result */
  /* a1,a2,a3,a4 correspondant aux bornes du trapeze et p,q les coeff      */
  /* de la focntion LRG .Fonction qui alloue                              */
  /* la place memoire necessaire au nombre flou de type LR et de         */
  /* nom NOM . Affiche un message d'erreur et renvoi NULL s'il y a un    */
  /* probleme . ATTENTION: entrer les bornes dans l'ordre .              */

nb_flou * lecture_triangle(FILE    * fichier,
			   char    * carac);
  /* sorties: nb_flou * result  */ 
  /*          char    * carac   */
  /* fonction qui alloue la place pour un nombre flou de type triangle   */
  /* dont le descriptif est disponible dans FICHIER. En sortie CARAC     */
  /* sera place sur le 1er caractere qui ne correspond plus a ce nombre. */


nb_flou * lecture_trapeze(FILE    * fichier,
			  char    * carac);
  /* sorties: nb_flou * result  */ 
  /*          char    * carac   */
  /* fonction qui alloue la place pour un nombre flou de type trapeze    */
  /* dont le descriptif est disponible dans FICHIER. En sortie CARAC     */
  /* sera place sur le 1er caractere qui ne correspond plus a ce nombre. */


nb_flou * lecture_LR(FILE    * fichier,
		     char    * carac);
  /* sorties: nb_flou * result  */ 
  /*          char    * carac   */
  /* fonction qui alloue la place pour un nombre flou de type LR         */
  /* dont le descriptif est disponible dans FICHIER. En sortie CARAC     */
  /* sera place sur le 1er caractere qui ne correspond plus a ce nombre. */

nb_flou * lecture_LRG(FILE    * fichier,
		     char    * carac);
  /* sorties: nb_flou * result  */ 
  /*          char    * carac   */
  /* fonction qui alloue la place pour un nombre flou de type LR         */
  /* dont le descriptif est disponible dans FICHIER. En sortie CARAC     */
  /* sera place sur le 1er caractere qui ne correspond plus a ce nombre. */

void dessin_triangle(nb_flou * flou,
		     int       color);
    /* procedure de tracage du nombre flou FLOU de type triangle dans la */
    /* couleur COLOR    */

void dessin_trapeze( nb_flou * flou,
		     int       color);
    /* procedure de tracage du nombre flou FLOU de type trapeze dans la  */
    /* couleur COLOR    */

void dessin_LR(nb_flou * flou,
		 int       color); 
    /* procedure de tracage du nombre flou FLOU de type LR dans la       */
    /* couleur COLOR    */

void dessin_LRG(nb_flou * flou,
		 int       color); 
    /* procedure de tracage du nombre flou FLOU de type LR dans la       */
    /* couleur COLOR    */

double appartient_triangle(nb_flou * flou,
			   double    x);
  /* sorties: double    mux  */
  /* calcul de l'appartenance de X a FLOU ( de type triangle ) */ 


double appartient_trapeze(nb_flou * flou,
			  double    x);
  /* sorties: double    mux  */
  /* calcul de l'appartenance de X a FLOU ( de type trapeze ) */ 


double appartient_LR(nb_flou * flou,
		     double    x);
  /* sorties: double    mux  */
  /* calcul de l'appartenance de X a FLOU ( de type LR ) */ 

double appartient_LRG(nb_flou * flou,
		     double    x);
  /* sorties: double    mux  */
  /* calcul de l'appartenance de X a FLOU ( de type LRG ) */ 

discret discretise_triangle(nb_flou * flou);
  /* sorties: discret   result */
  /* fonction qui dicretise un nombre flou de type triangle . Le nombre */
  /* de points en sortie est determine par PREC defini ci-dessus.       */


discret discretise_trapeze( nb_flou * flou);
  /* sorties: discret   result */
  /* fonction qui dicretise un nombre flou de type trapeze . Le nombre  */
  /* de points en sortie est determine par PREC defini ci-dessus.       */


discret discretise_LR(nb_flou * flou);
  /* sorties: discret   result */
  /* fonction qui dicretise un nombre flou de type L-R . Le nombre      */
  /* de points en sortie est determine par PREC defini ci-dessus.       */

discret discretise_LRG(nb_flou * flou);
  /* sorties: discret   result */
  /* fonction qui dicretise un nombre flou de type LRG . Le nombre      */
  /* de points en sortie est determine par PREC defini ci-dessus.       */

#endif
