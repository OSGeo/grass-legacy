/**************************************************************************/
/*          include pour les lecture/ecriture  : E_S.c                    */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                                                                        */
/*                   derniere modif : 08/96                               */
/**************************************************************************/
#ifndef INT_E_S
#define INT_E_S
#include "erreur.h"

/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/

char lire( FILE * fichier );
   /* sorties: char   c       */
   /* lecture d'un caractere ds Fichier, la variable Fichier est un pointeur
      sur un fichier prealablement ouvert en lecture*/


double lirenb(FILE *, char *, erreur *);
  
/* sorties: double   result  */
/*          char   * carac   */
/*          erreur * err     */
/* lecture d'un double ( place dans RESULT ) ds FICHIER , prealablement */
/* ouvert en lecture.                                                  */
/* au depart carac doit etre sur le 1er chiffre du double et a la      */
/* sortie il est positionne sur le caractere suivant le double lu.     */
/* Une erreur est generee en cas de probleme                           */


int lireint( FILE   * fichier,
	     char   * carac,
	     erreur * err);

/* sorties: int      result    */
/*          char   * carac     */
/*          erreur * err       */
/* idem que precedemment avec un entier.                               */


char lireblanc(FILE * fichier,
	       char   carac);

/* sorties: char   result  */
/* verifie que carac est un ' ' ou un '\n'. Puis passe tous les	 */
/* caracteres de ce type et donne en sortie le 1er qui n'en est pas un */


char lirespace(FILE * fichier,
	       char   carac);

/* sorties: char   result  */
/* verifie que carac est un ' ' ou un TAB . Puis passe tous les        */
/* caracteres de ce type et donne en sortie le 1er qui n'en est pas un */


char lireblanc1(FILE * fichier,
		char   carac,
		int  * nb_ligne);

/* sorties: char   result   */
/* verifie que carac est un ' ' ou un '\n'. Puis passe tous les        */
/* caracteres de ce type et donne en sortie le 1er qui n'en est pas un */
/* en comptant le nombre de lignes qui ont ete passees                 */


char lireblanc_chgtligne(FILE * fichier,
			 char   carac);

/* sorties: char   result  */
/* verifie que carac est un ' ' ou un TAB. Puis passe tous les        */
/* caracteres de ce type et donne en sortie le 1er qui n'en est pas un */


char * lirenom(FILE   * fichier,
	       char   * carac,
	       erreur * err);

/* sorties: char   * result  */
/*          char   * carac   */
/*          erreur * err     */
/* carac etant place sur un caractere alphanumerique, lit un nom et le */
/* place dans RESULT .                                                 */
/* ATTENTION : la longueur du nom ne doit pas depasser LONGMOT , sinon */
/* il faut le changer ci-dessus                                        */
/* Une erreur est generee en cas de probleme                           */


char * saisie_nom();

/* sorties: char * nom */
/* fonction qui saisie un mot au clavier , la fin du mot etant defini */
/* par un retour a la ligne                                           */ 

#endif
