/**************************************************************************/
/*          include pour les toutes les erreurs : *.c                     */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/**************************************************************************/
#ifndef INT_erreur
#define INT_erreur



/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/

typedef int erreur ; 
       /* 0 = pas d'erreur                                                 */
       /* 1 = probleme d'allocation d'une chaine de caracteres             */
       /* 2 = probleme d'allocation pour la classe floue                  */
       /* 3 = probleme d'allocation pour une variable floue               */
       /* 4 = impossible d'ouvrir le fichier */
       /* 5 = Le nombre de raster ne correspond pas au nombre de variable */
       /* 6 = Les fichiers de varibles floues associes aux rastersne doivent pas contenir les autres varaibles */
       /* 7 =  L'option n'est pas valide */

/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/
void Erreur(erreur err,
	    int    nb,
	    char  * ch_err);
     /* fonction qui affiche les erreurs qui peuvent etre generees.      */

#endif
