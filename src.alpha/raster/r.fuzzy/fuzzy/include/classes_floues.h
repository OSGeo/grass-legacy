/**************************************************************************/
/*          include pour les classes floues : pas de fichier .c           */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                                                                        */
/*                   derniere modif : 08/96                               */
/**************************************************************************/
#ifndef INT_classes_floues
#define INT_classes_floues

#include "var_floues.h"
#include "definitions.h"

/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/


typedef struct classe_floue
	{
	  int           nb;             /* nombre de variables floues    */
	  varia_floue * tab[10];  /* tableau des variables floues  */
	  char        * regle;          /* regle qui sera appliquee sur  */
                                        /* les variables                 */ 
          char        * var_sortie;     /* nom de la variable de sortie  */
	  /* Modif 21-11-97 MOREL L. */

	  int		nb_var_sortie;	/* nombre de variable de sortie	*/
	  int		nb_var_utilise;	/* nombre de var utilise ds la regle */

	  /* Modif 02-12-97 MOREL L. */
	  int		nb_var_entree;  /* nb var en entree */
	  int	      * ench_var;	      /* pr savoir l'ench des variables */
	  /* Modif 17-12-97 MOREL L. */
	  int		nb_var_reinjecter; /* nb variable reinjecter */
	}classe_floue;


/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/

#endif
