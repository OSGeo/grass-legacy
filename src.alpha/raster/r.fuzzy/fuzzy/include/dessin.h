/**************************************************************************/
/*          include pour les dessins : pas de fichier .c                  */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/**************************************************************************/
#ifndef INT_dessin
#define INT_dessin
#include "maths.h"

#define NP0 10000

/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/

void courbe_();
    /* procedure graftcl ( fortran ) pour definir une courbe             */

void go_();
    /* procedure graftcl ( fortran ) pour lancer le tracage              */

void axex_();
    /* procedure graftcl ( fortran ) pour redefinir l'axe Ox             */

void axey_();
    /* procedure graftcl ( fortran ) pour redefinir l'axe Oy             */

void dessin_sortie(nash_ *  na,
		   char * nom,
		   char * title);
  /* dessin des valeurs calculees par rapport aux valeurs experimentales */
  /* le tout contenu dans NA . NOM etant le nom de la variable de sortie */
  /* et TITLE celui de la courbe                                         */ 

#endif
