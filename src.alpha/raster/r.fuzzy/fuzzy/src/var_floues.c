#define MOD_var_floues

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "var_floues.h"
#include "erreur.h"
#include "dessin.h"
#include "definitions.h"

/*************************************************************************/
/******************** allocation d'une variable floue ********************/
/*************************************************************************/
varia_floue * cree_variable_floue(char * nom)
{
  varia_floue * result;

  result = ( varia_floue * )malloc( sizeof( varia_floue ));
  if ( result != NULL )
    {
      result->name = nom;
      result->liste_elem = NULL;
      /* MODIF 01-12-97 MOREL L */ 
      result->reinjecter = 0; /* initialisation de reinjecter a faux = 0 */
      result->val_sortie = 0.0; /* initialisation de la derniere valeur de sortie a 0*/
      /* MODIF 02-12-97 MOREL L */ 
      result->nash = NULL ; /* Mise a null du ptr */
      /* MODIF 09-01-98 MOREL L */ 
      
      result->val_sortieOld = 0.0; /* initialisation de l.avant derniere valeur de sortie a 0*/
    }

  return( result );
}

/*************************************************************************/
/******************** ajout d'un nombre flou dans la liste ***************/
/*************************************************************************/
void ajout_nb_flou(varia_floue * varia,
		   nb_flou     * nombre,
		   erreur      * err)
{
  node * nouv;
  node * vue;
      
  nouv = ( node * )malloc(sizeof( node ));
  if ( nouv == NULL )
    (*err) = 14;
  nouv->nombre = (*nombre);
  nouv->suiv = NULL;
  vue = varia->liste_elem;

  if( vue == NULL )
    varia->liste_elem = nouv;
  else
    {
      while( vue->suiv != NULL )
        vue = vue->suiv;
      vue->suiv = nouv;
     }
}


/**************************************************************************/
/********************* dessin d'une variable floue ************************/
/**************************************************************************/
void dessin_varia_floue(varia_floue * floue)
{
  double  a,b,c,d;
  node  * vue;
  int     numx,iaxex,iposx,numy,iaxey,iposy;
  float   dx,dy;
  float   xmin,xmax,ymin,ymax;
  char    legx[50];
  char    legy[50];
  int     i;
  float * Gx;
  float * Gy;
  int     numc;
  int     n;
  int     isymb;
  int     itrait;
  char    legc[50];
  char    titre[80];
  int     color;

 

  color = 2;

/*   Gx = ( float * ) malloc( nbpoints*sizeof( float ) ); */
/*   Gy = ( float * ) malloc( nbpoints*sizeof( float ) ); */

   Gx = ( float * ) malloc( NP0*sizeof( float ) ); 
   Gy = ( float * ) malloc( NP0*sizeof( float ) ); 
  isymb = 1;
  itrait = 1;
  for( i=0;i<50;i++ )
    {
      legx[i] = ' ' ;
      legy[i] = ' ' ;
    }
  numx = 0;
  numy = 0;
  iaxex = 1;
  iaxey = 1;
  iposx = 1;
  iposy = 1;
  dy = 0.1;
  dx = 0;
  
              /*******************************/
              /* determination des extremums */
              /*******************************/
  xmin = 10000;
  ymin = 0;
  xmax = 0;
  ymax = 1.1;
  vue = floue->liste_elem;
  while( vue != NULL )
    {
      switch( vue->nombre.type )
        {
          case trian:
            if ( vue->nombre.classe.tri[2] > xmax )
              xmax = vue->nombre.classe.tri[2];
            if ( vue->nombre.classe.tri[0] < xmin )
              xmin = vue->nombre.classe.tri[0];
            break;
          case trap:
            if ( vue->nombre.classe.tra[3] > xmax )
              xmax = vue->nombre.classe.tra[3];
            if ( vue->nombre.classe.tra[0] < xmin )
              xmin = vue->nombre.classe.tra[0];
            break;
          case lr:
            if ( vue->nombre.classe.L[2] > xmax )
              xmax = vue->nombre.classe.L[2];
            if ( vue->nombre.classe.L[0] < xmin )
              xmin = vue->nombre.classe.L[0];
            break;
	  case lrg:
	    if ( vue->nombre.classe.Lrg[3] > xmax )
              xmax = vue->nombre.classe.Lrg[3];
            if ( vue->nombre.classe.Lrg[0] < xmin )
              xmin = vue->nombre.classe.Lrg[0];
            break;
	    
        }
     vue = vue->suiv;
    }

  axex_( &numx, &iaxex, &iposx, &dx, &xmin, &xmax, legx ); 
  axey_( &numy, &iaxey, &iposy, &dy, &ymin, &ymax, legy );
  vue = floue->liste_elem;

  strcpy( titre, floue->name ); 
  for( i=strlen( titre );i<80;i++ )
    titre[i] = ' ';

               /***************************************************/
               /* definition de la courbe pour chaque nombre flou */
               /***************************************************/
   numc = 0;
  while( vue != NULL )
    {
      switch( vue->nombre.type )
        {
          case trian:
            n = 3;
            Gx[0] = vue->nombre.classe.tri[0];
            Gy[0] = 0.;
            Gx[1] = vue->nombre.classe.tri[1];
            Gy[1] = 1.;
            Gx[2] = vue->nombre.classe.tri[2];
            Gy[2] = 0.;
            strcpy( legc, vue->nombre.name );
            for( i=strlen( legc );i<50;i++ )
              legc[i] = ' ';
	    /*	    printf("DEBUG numc=%d\n",numc);*/
            courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &color, legc ); 
            break;

           case trap:
            n = 4;
            Gx[0] = vue->nombre.classe.tra[0];
            Gy[0] = 0.;
            Gx[1] = vue->nombre.classe.tra[1];
            Gy[1] = 1.;
            Gx[2] = vue->nombre.classe.tra[2];
            Gy[2] = 1.;
            Gx[3] = vue->nombre.classe.tra[3];
            Gy[3] = 0.;
            strcpy( legc, vue->nombre.name );
            for( i=strlen( legc );i<50;i++ )
              legc[i] = ' ';

            courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &color, legc ); 
            break;

          case lr:
            n = nbpoints;
            a = vue->nombre.classe.L[0];
            b = vue->nombre.classe.L[1];
            c = vue->nombre.classe.L[2];
            for( i=0;i<(nbpoints-1)/2;i++ )
              {
                Gx[i] = a+(float)i*(b-a)/(nbpoints-1)*2;
                Gy[i] = appartient_LR( &(vue->nombre), Gx[i] );
              }
            Gx[(nbpoints-1)/2] = b;
            Gy[(nbpoints-1)/2] = 1.;
            for( i=1;i<=(nbpoints-1)/2;i++ )
              {
                Gx[i+(nbpoints-1)/2] = b+ i*(c-b)/(nbpoints-1)*2;
                Gy[i+(nbpoints-1)/2] = appartient_LR( &(vue->nombre),Gx[i+(nbpoints-1)/2]);
              }
            strcpy( legc, vue->nombre.name );
            for( i=strlen( legc );i<50;i++ )

              legc[i] = ' ';

            courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &color, legc );
            break;
	case lrg:
            n = nbpoints;
            a = vue->nombre.classe.Lrg[0];
            b = vue->nombre.classe.Lrg[1];
            c = vue->nombre.classe.Lrg[2];
	    d = vue->nombre.classe.Lrg[3];
            for( i=0;i<(nbpoints-1)/2;i++ )
              {
                Gx[i] = a+(float)i*(b-a)/(nbpoints-1)*2;
                Gy[i] = appartient_LRG( &(vue->nombre), Gx[i] );
              }
            Gx[(nbpoints-1)/2] = b;
            Gy[(nbpoints-1)/2] = 1.;
	    Gx[((nbpoints-1)/2)+1] = c;
	    Gy[((nbpoints-1)/2)+1] = 1.;
            for( i=2;i<=(nbpoints-1)/2;i++ )
              {
                Gx[i+(nbpoints-1)/2] = c+ i*(d-c)/(nbpoints-1)*2;
                Gy[i+(nbpoints-1)/2] = appartient_LRG( &(vue->nombre),Gx[i+(nbpoints-1)/2]);
              }
            strcpy( legc, vue->nombre.name );
            for( i=strlen( legc );i<50;i++ )

              legc[i] = ' ';

            courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &color, legc );
            break;
        }
      vue = vue->suiv;

      /*      numc = numc + 1;  */
      /* numc=0;*/
      color = color + 1;
      /*if ( color == 1 )
        color = 2;*/
      if ( color == 8 )
        color = 2;
    }
  go_( titre );
  free( Gx );
  free( Gy );
}


