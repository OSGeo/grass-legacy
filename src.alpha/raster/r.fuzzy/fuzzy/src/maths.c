#define MOD_maths

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "maths.h"
#include "discrets.h"

/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/


/**************************************************************************/
/************** calcul de la somme des carres des ecarts ******************/
/**************************************************************************/
double s_carre_ecarts(nash_ * nash)
{
  nash_ * pteur;
  double result;

  result = 0;
  pteur = nash;
  while(pteur != NULL)
   {
     result = (pteur->val_calc - pteur->val_obs)*(pteur->val_calc - pteur->val_obs) + result;
     pteur = pteur->suiv;
   }
  return(result);
}


/**************************************************************************/
/************** calcul de la moyenne des carres des ecarts ****************/
/**************************************************************************/
double m_carre_ecarts(nash_ * nash)
{
  nash_ * pteur;
  double result;
  double i;

  i = 0;
  result = 0;
  pteur = nash;
  while(pteur != NULL)
   {
     result = (pteur->val_calc - pteur->val_obs)*(pteur->val_calc - pteur->val_obs) + result;
     i = i + 1;
     pteur = pteur->suiv;
   }
/****************************************************************************/
/* Modif Delclaux 23 novembre 2000                                            */
/* on calcule a la place de l'ancien, la RMSE ou EQM, ecart quadratique moyen */
/*                                                                            */
/* result = result/i;                                                         */
/*****************************************************************************/
  result = sqrt ( result / (i-1) );
  return(result);
}


/**************************************************************************/
/************** calcul de la moyenne des valeurs observees ****************/
/**************************************************************************/
double moy_val_calc(nash_ * nash)
{
  nash_ * pteur;
  double result;
  double i;

  i = 0;
  result = 0;
  pteur = nash;
  while(pteur != NULL)
   {
     result = pteur->val_calc + result;
     i = i + 1;
     pteur = pteur->suiv;
   }
  result = result/i;
  return(result);
}


/**************************************************************************/
/*************** calcul de la moyenne des valeurs calculees ***************/
/**************************************************************************/
double moy_val_obs(nash_ * nash)
{
  nash_ * pteur;
  double result;
  double i;

  i = 0;
  result = 0;
  pteur = nash;
  while(pteur != NULL)
   {
     result = pteur->val_obs + result;
     i = i + 1;
     pteur = pteur->suiv;
   }
  result = result/i;
  return(result);
}


/**************************************************************************/
/****************** calcul de la moyenne norme des ecarts *****************/
/**************************************************************************/
double m_norm_carre_ecarts(nash_ * nash)
{
  nash_ * pteur;
  double result;
  double result1;
  double mo;

  mo = moy_val_obs(nash);
  result = 0;
  result1 = 0;
  pteur = nash;
  while(pteur != NULL)
   {
     result = (pteur->val_calc - pteur->val_obs)*(pteur->val_calc - pteur->val_obs) + result;
     result1 = (pteur->val_obs - mo)*(pteur->val_obs - mo) + result1;
     pteur = pteur->suiv;
   }
  result = result/result1;
  return(result);
}


/**************************************************************************/
/****************** calcul de la moyenne par correlation ******************/
/**************************************************************************/
double ind_correl(nash_ * nash)
{
  nash_ * pteur;
  double mo;
  double mc;
  double result;
  double result1;
  double result2;

  mo = moy_val_obs(nash);
  mc = moy_val_calc(nash);
  pteur = nash;
  result = 0;
  result1 = 0;
  result2 = 0;
  while(pteur != NULL)
   {
     result = result + (pteur->val_obs - mo)*(pteur->val_calc - mc);
     result1 = result1 + (pteur->val_obs - mo)*(pteur->val_obs - mo);
     result2 = result2 + (pteur->val_calc - mc)*(pteur->val_calc - mc);
     pteur = pteur->suiv;
   }
  result = result/(sqrt(result1*result2));
  return(result);
}

/**************************************************************************/
/*********************** maximum de deux doubles **************************/
/**************************************************************************/
double max(double a,
	   double b)
{
  if ( a > b ) 
    return( a );
  else
    return( b );
}


/**************************************************************************/
/*********************** minimum de deux doubles **************************/
/**************************************************************************/
double min(double a,
	   double b)
{
  if ( a > b )
    return( b );
  else
    return( a );
}


/**************************************************************************/
/************************** fonction puissance ****************************/
/**************************************************************************/
double puissance(double a,
		 double b)
{
  if ( b == 0. )
    return( 1. );
  else
    {
      if ( a == 0. )
        return( 0. );
      else
        return( exp( b*log( a ) ) );
    }
}




