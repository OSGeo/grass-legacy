#define MOD_piles

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include "piles.h"
#include "fichiers.h"
#include "regles.h"


/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/

/**************************************************************************/
/********************* ajout d'un mot dans la pile ************************/
/**************************************************************************/
erreur ajout_nom(char    * name,
		 pilenom * Pi)
{
  node1  * nouv;
  pilenom  vue;


                /* verification de la non existance du nouveau nom */

  vue = (*Pi);
  while( vue != NULL )
    {
      if( strcmp( vue->name, name ) == 0 )
        return( 11 );
      else
        vue = vue->suiv;
    }
              /* ajout du nom si n'existe pas */
  nouv = ( node1 * )malloc( sizeof( node1 ) );
  if ( nouv == NULL )
    return( 10 );
  nouv->name = ( char * ) malloc( LONGMOT*sizeof( char ) );
  if ( nouv->name == NULL )
    return( 7 );
  strcpy( nouv->name, name );
  nouv->suiv = (*Pi);
  (*Pi) = nouv;

  return( 0 );
}


/***************************************************************/
/* Initalisation de la pile                                    */
/***************************************************************/
pile initialiser()
{
  pile pi;
  
  pi = (pilereg *) malloc (sizeof(pilereg));
  if ( pi == NULL )
    affiche_erreur(27,0,'\0');
  pi->tete = 0;
  return(pi);
}


/***************************************************************/
int depiler(pile pi)
{
  if( pi->tete == 0 )
    affiche_erreur(28,0,'\0');
  pi->tete = pi->tete - 1;
  return(pi->pil[pi->tete]);
}


/**************************************************************/
pile inv_pile(pile pi)
{
  pile nouv;
  int i;

  nouv = initialiser();
  for(i=0;i<pi->tete;i++)
    nouv->pil[i] = pi->pil[pi->tete-i-1];
  nouv->tete = pi->tete;
  return(nouv);
}


/**************************************************************/
void empiler(pile pi,
	     int elt)
{
  if ( pi->tete == NOMBREELT )
    affiche_erreur(30,0,'\0');
  pi->pil[pi->tete] = elt;
  pi->tete = pi->tete + 1;
}

/**************************************************************/
void AffichePile(pile pi)
{
  int i;

  printf("    nb_elt = %d\n",pi->tete);
  i = 0;
  while(i < pi->tete)
   {
     printf("    Valeur %d de la pile : %d\n",i,pi->pil[i]);
     i++;
   }
}

/******************************************************/
pile inf_post(pile inf)
{
  pile oper;
  pile post;
  operateur elt;
  operateur opprec;
  operateur tmp;
  int fin;

  post = initialiser();
  oper = initialiser();
  empiler(oper,paro);
  if (inf->tete != 0)
   {
     inf = inv_pile(inf);
     while(inf->tete > 0)
      {
        elt = depiler(inf);
        switch (elt)
         {
           case paro:empiler(oper,elt);
                     break;
           case parf:elt = depiler(oper);
                     while(elt != paro)
                      {
                        empiler(post,elt);
                        elt = depiler(oper);
                      }
                     break;
           default:  if (elt >= 10)
                       empiler(post,elt);
                     else
                      {
                        opprec = oper->pil[oper->tete-1];
                        while(priorite(elt) <= priorite(opprec))
                         {
                           empiler(post,opprec);
                           tmp = depiler(oper);
                           opprec = oper->pil[oper->tete-1];
                         }
                        empiler(oper,elt);
                      }
                     break;
         }
      }
     fin = 0;
     while((oper->tete != 0) && (fin == 0))
      {
        elt = oper->pil[oper->tete-1];
        if (elt == paro)
          fin = 1;
        else
         {
           empiler(post,elt);
           tmp = depiler(oper);
         }
      }  
   }
  else
    affiche_erreur(120,0,NULL);
   /* Attention : pile vide il n'y a pas de regle!! */
  return(post);  
}


