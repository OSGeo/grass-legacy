#define MOD_dessin

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "maths.h"
#include "dessin.h"


/**************************************************************************/
/********************* dessin de la reponse en sortie *********************/
/**************************************************************************/
void dessin_sortie( nash_ * na,
		   char  * nom,
		   char  * title )
{
  
  nash_ * vue;
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
  int     icolc;
  char    legc[50];
  char    titre[80];
  
  /**************************************************/
  /* allocation des deux tableaux de 10 000 points */
  /* voir parameter np0 ds graftcl.ins             */
  /*************************************************/
  Gx = ( float * ) malloc( NP0*sizeof( float ) );
  Gy = ( float * ) malloc( NP0*sizeof( float ) );

                   /**********************************************/
                   /*          mise a jour des titres            */ 
                   /* ! tous les caracteres doivent etre remplis */
                    /**********************************************/
  strcpy(titre, title);
  for( i = strlen( titre);i<80;i++)
    titre[i] = ' ';
  strcpy( legx, nom );
  i = strlen( legx );
  legx[i] = ' ';
  legx[i+1] = 'o';
  legx[i+2] = 'b';
  legx[i+3] = 's';
  legx[i+4] = '.';
  legx[i+5] = ' ';
  legx[i+6] = ' ';
  legx[i+7] = ' ';
/*  legx[i+8] = 'e'; */
  for( i=strlen( nom )+8;i<50;i++ )
    legx[i] = ' ';

  strcpy( legy, nom );
  i = strlen( legy );
  legy[i] = ' ';
  legy[i+1] = 'f';
  legy[i+2] = 'l';
  legy[i+3] = 'o';
  legy[i+4] = 'u';
/*  legy[i+5] = 'u';
  legy[i+6] = 'l';
  legy[i+7] = 'e';
  legy[i+8] = 'e'; */
  for( i=strlen( nom )+5;i<50;i++ )
    legy[i] = ' ';


                     /************************************************/
                     /* determination des minima et maxima en x et y */
                     /************************************************/
  vue = na;
  xmin = vue->val_obs;
  xmax = xmin;
  ymin = vue->val_calc;
  ymax = ymin;
  n = 0;
  while( vue != NULL )
    {
      Gx[n] = vue->val_obs;           /* remplissage des tableaux */
      Gy[n] = vue->val_calc;
      vue = vue->suiv;
      if ( Gx[n] < xmin )
        xmin = Gx[n];
      if ( Gx[n] > xmax )
        xmax = Gx[n];
      if ( Gy[n] < ymin )
        ymin = Gy[n];
      if ( Gy[n] > ymax )
        ymax = Gy[n];
      n= n + 1;
    }
  xmin = xmin - 2;
  ymin = ymin - 2;
  xmax = xmax + 2;
  ymax = ymax + 2;
  if ( xmin < ymin )
    ymin = xmin;
  else
    xmin = ymin;
  if( xmax > ymax )
    ymax = xmax;
  else
    xmax = ymax;  
    
  isymb = 3;
  itrait = 0;
  icolc = 2;
  numx = 1;
  numy = 1;
  iaxex = 1;
  iaxey = 1;
  iposx = 1;
  iposy = 1;
  dy = 0;
  dx = 0;
  numc = 1;
  strcpy( legc, " ");
  for( i=strlen( legc );i<50;i++)
    legc[i] = ' ';
  axex_( &numx, &iaxex, &iposx, &dx, &xmin, &xmax, legx ); 
  axey_( &numy, &iaxey, &iposy, &dy, &ymin, &ymax, legy );
  courbe_( &numc, Gx, Gy, &n, &isymb, &itrait, &icolc, legc );

                 /****************************/
                 /* trace de la bissectrice */ 
                 /****************************/

  /*  numc=1; */
  itrait = 1;
  n = 2;
  isymb = 1;
  icolc = 3;
  Gx[0] = xmin;
  Gy[0] = ymin;
  Gx[1] = xmax;
  Gy[1] = ymax;
  strcpy( legc, " ");
  for( i=strlen( legc );i<50;i++)
    legc[i] = ' ';
  courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &icolc, legc );

/*   fprintf(stderr,"dessin.c dans dessin_sortie, avant go\n"); */

  go_( titre );

/*   fprintf(stderr,"dessin.c dans dessin_sortie, apres  go\n"); */

   free( Gx ); 

/*   fprintf(stderr,"dessin.c dans dessin_sortie, apres freegx\n"); */

   free( Gy ); 

/*   fprintf(stderr,"dessin.c dans dessin_sortie, apres  freegy\n"); */

}
