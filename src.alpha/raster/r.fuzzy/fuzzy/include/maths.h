/**************************************************************************/
/*         include pour les operations mathematiques : maths.c            */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                                                                        */
/*                   derniere modif : 08/96                               */
/**************************************************************************/
#ifndef INT_maths
#define INT_maths

/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/

typedef struct nash_      /* liste pour le calcul du coefficient de nash    */
  {                       /* elle contient l'ensemble des valeurs observees */
    double val_obs;       /* et des valeurs calculees                       */
    double val_calc;
    struct nash_ * suiv;
  }nash_;


/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/

double s_carre_ecarts(nash_ * nash);
  /* sorties: double  result */
  /* Retourne l'erreur suivant la methode de la somme des carres */
  /* des ecarts pour les valeurs contenues dans nash.            */

double m_carre_ecarts(nash_ * nash);
  /* sorties: double  result */
  /* Retourne l'erreur suivant la methode de la moyenne des carres */
  /* des ecarts pour les valeurs contenues dans nash.              */

double m_norm_carre_ecarts(nash_ * nash);
  /* sorties: double  result */
  /* Retourne l'erreur suivant la methode de la moyenne de la          */
  /* norme des carres des ecarts pour les valeurs contenues dans nash. */

double ind_correl(nash_ * nash);
  /* sorties: double  result */
  /* Retourne l'erreur suivant la methode de l'indice de correlation */
  /* pour les valeurs contenues dans nash.                           */


double moy_val_calc(nash_ * nash);
  /* sorties: double  result */
  /* Retourne la moyenne des valeurs calculees  */
  /* pour les valeurs contenues dans nash.      */


double moy_val_obs(nash_ * nash);
  /* sorties: double  result */
  /* Retourne la moyenne des valeurs observees */
  /* pour les valeurs contenues dans nash.     */


double max(double a,
	   double b);
  /* sorties: double max */
  /* retourne le maximum de A et B */


double min(double a,
	   double b);
  /* sorties: double min */
  /* retourne le minimum de A et B */


double puissance(double a,
		 double b);
  /* sorties: double result */
  /* fonction qui retourne A^B */



#endif
