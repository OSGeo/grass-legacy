/**************************************************************************/
/*                 include pour les piles : piles.c                       */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                                                                        */
/*                   derniere modif : 08/96                               */
/**************************************************************************/
#ifndef INT_piles
#define INT_piles

#include "erreur.h"
#include "definitions.h"

/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/

typedef struct pilereg          /*  Pile d'entier contenant une tete      */
  {                             /* et un tableau contenant les elements.  */
    int pil[NOMBREELT];         /* La tete pointe sur le haut de la pile. */
    int tete;                 
  }pilereg;

typedef pilereg * pile;

/**************************************************************************/
/*                 declaration des fonctions et procedure                 */
/**************************************************************************/

erreur ajout_nom();

pile initialiser();
  /* sorties: pile pi */ 
  /* Initialise la pile pi avec 0 dans tete et 0 dans max.   */

int depiler(pile pi);
  /* sorties: pile pi  */
  /*          int  elt */
  /* Retourne l'entier en haut de la pile et decremente la tete.  */

pile inv_pile(pile pi);
  /* sorties: pile nouv */
  /* Inverse la pile pi qui est recopiee dans la */
  /* pile nouv qui est retournee.                */

void empiler(pile pi,
	     int  elt);
  /* sorties: pile pi  */
  /* Empile l'element en pil[tete] et incremente la tete et le max. */

void AffichePile( pile pi );
  /* Affiche la pile. */

pile inf_post(pile inf);
  /* sorties: pile post */
  /* Transforme la pile infixee en une pile postfixee pour l'execution. */

#endif
