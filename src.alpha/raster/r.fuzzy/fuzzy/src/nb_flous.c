#define MOD_nb_flous

#include <stdio.h>
#include <stddef.h>
#include "nb_flous.h"
#include "dessin.h"
#include "definitions.h"
#include "E_S.h"
#include "maths.h"
#include "var_floues.h" 


/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/

/*************************************************************************/
/*************** genere un nombre flou triangulaire **********************/
/*************************************************************************/
nb_flou * genere_triangle( double  a1,
			   double  a2,
			   double  a3,
			   char * nom)
{
  nb_flou * flou=NULL;

                       /*****************************/
                       /* allocation du nombre flou */
                       /*****************************/
  flou = (nb_flou *)(malloc(sizeof(nb_flou)-sizeof(union clas)+sizeof(triangle)));
  if ( flou != NULL )
    {
                       /**************************/
                       /* remplissage des champs */
                       /**************************/
      flou->name = nom;
      flou->type = trian;
      flou->classe.tri[0] = a1;
      flou->classe.tri[1] = a2;
      flou->classe.tri[2] = a3;
    }

  return( flou );
}


/*************************************************************************/
/*************** genere un nombre flou trapezoidale **********************/
/*************************************************************************/
nb_flou * genere_trapeze(double  a1,
			 double  a2,
			 double  a3,
			 double  a4,
			 char * nom)
{
  nb_flou * flou=NULL;

                       /*****************************/
                       /* allocation du nombre flou */
                       /*****************************/
  flou = (nb_flou *)(malloc(sizeof(nb_flou)-sizeof(union clas)+sizeof(trapeze)));
  if ( flou != NULL )
    {
                       /**************************/
                       /* remplissage des champs */
                       /**************************/
      flou->name = nom;
      flou->type = trap;
      flou->classe.tra[0] = a1;
      flou->classe.tra[1] = a2;
      flou->classe.tra[2] = a3;
      flou->classe.tra[3] = a4;
    }

  return( flou );
}

/*************************************************************************/
/******************** genere un nombre flou LR ***************************/
/*************************************************************************/
nb_flou * genere_LR( double  a1,
		     double  a2,
		     double  a3,
		     double  p,
		     double  q,
		     char * nom)
{
  nb_flou * flou=NULL;

                       /*****************************/
                       /* allocation du nombre flou */
                       /*****************************/
  flou = (nb_flou *)(malloc(sizeof(nb_flou)-sizeof(union clas)+sizeof(LR)));
  if ( flou != NULL )
    {
                       /**************************/
                       /* remplissage des champs */
                       /**************************/
      flou->name = nom;
      flou->type = lr;
      flou->classe.L[0] = a1;
      flou->classe.L[1] = a2;
      flou->classe.L[2] = a3;
      flou->classe.L[3] = p;
      flou->classe.L[4] = q;
    }

  return( flou );
}

/*************************************************************************/
/******************** genere un nombre flou LRG ***************************/
/*************************************************************************/
nb_flou * genere_LRG( double  a1,
		     double  a2,
		     double  a3,
		     double  a4,
		     double  p,
		     double  q,
		     char * nom)
{
  nb_flou * flou=NULL;

                       /*****************************/
                       /* allocation du nombre flou */
                       /*****************************/
  flou = (nb_flou *)(malloc(sizeof(nb_flou)-sizeof(union clas)+sizeof(LRG)));
  if ( flou != NULL )
    {
                       /**************************/
                       /* remplissage des champs */
                       /**************************/
      flou->name = nom;
      flou->type = lrg;
      flou->classe.Lrg[0] = a1;
      flou->classe.Lrg[1] = a2;
      flou->classe.Lrg[2] = a3;
      flou->classe.Lrg[3] = a4;
      flou->classe.Lrg[4] = p;
      flou->classe.Lrg[5] = q;
    }

  return( flou );
}

/*************************************************************************/
/************** lecture d'un nombre flou triangulaire ********************/
/*************************************************************************/
nb_flou * lecture_triangle(FILE   * fichier,
			   char   * carac)
{
  double    a1,a2,a3;
  char    * nom;
  nb_flou * flou;
  erreur  * err;

  (*carac) = lireblanc( fichier, (*carac) );
  a1 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  a2 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  a3 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  nom = lirenom( fichier, carac, err );

  flou = genere_triangle( a1, a2, a3, nom );
  return( flou );
}


/*************************************************************************/
/***************** lecture d'un nombre flou trapezoidale *****************/
/*************************************************************************/
nb_flou * lecture_trapeze(FILE   * fichier,
			  char   * carac)
{
  double    a1,a2,a3,a4;
  char    * nom;
  nb_flou * flou;
  erreur  * err;
  
  (*carac) = lireblanc( fichier, (*carac) );
  a1 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  a2 = lirenb( fichier, carac, err );
 
  (*carac) = lireblanc( fichier, (*carac) );
  a3 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  a4 = lirenb( fichier, carac, err );


  (*carac) = lireblanc( fichier, (*carac) );
  nom = lirenom( fichier, carac, err );

  flou = genere_trapeze( a1, a2, a3, a4, nom );

  return( flou );
}

/*************************************************************************/
/********************** lecture d'un nombre flou LR **********************/
/*************************************************************************/
nb_flou * lecture_LR(FILE   * fichier,
		     char   * carac)
{
  double    a1,a2,a3,p,q;
  char    * nom;
  nb_flou * flou;
  erreur  * err;
  
  (*carac) = lireblanc( fichier, (*carac) );
  a1 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  a2 = lirenb( fichier, carac, err );
 
  (*carac) = lireblanc( fichier, (*carac) );
  a3 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  p = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  q = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  nom = lirenom( fichier, carac, err );

  flou = genere_LR( a1, a2, a3, p, q, nom );
  return( flou );
}

/*************************************************************************/
/********************** lecture d'un nombre flou LRG **********************/
/*************************************************************************/
nb_flou * lecture_LRG(FILE   * fichier,
		     char   * carac)
{
  double    a1,a2,a3,a4,p,q;
  char    * nom;
  nb_flou * flou;
  erreur  * err;
  
  (*carac) = lireblanc( fichier, (*carac) );
  a1 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  a2 = lirenb( fichier, carac, err );
 
  (*carac) = lireblanc( fichier, (*carac) );
  a3 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  a4 = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  p = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  q = lirenb( fichier, carac, err );

  (*carac) = lireblanc( fichier, (*carac) );
  nom = lirenom( fichier, carac, err );

  flou = genere_LRG( a1, a2, a3,a4, p, q, nom );
  return( flou );
}

/**************************************************************************/
/**************** dessin d'un nombre floue triangulaire *******************/
/**************************************************************************/
void dessin_triangle(nb_flou * flou,
		     int       color)
{
  int     numx,iaxex,iposx,numy,iaxey,iposy;
  float   dx,dy;
  float   xmin,xmax,ymin,ymax;
  char    legx[50];
  char    legy[50];
  float * Gx;
  float * Gy;
  int     i,numc,n,isymb,itrait,icolc;
  char    legc[50];
  char    titre[80];

 /*  Gx = ( float * ) malloc( nbpoints*sizeof( float ) ); */
/*   Gy = ( float * ) malloc( nbpoints*sizeof( float ) ); */

  Gx = ( float * ) malloc( NP0*sizeof( float ) );
  Gy = ( float * ) malloc( NP0*sizeof( float ) );

  Gx[0] = flou->classe.tri[0];
  Gy[0] = 0.;
  Gx[1] = flou->classe.tri[1];
  Gy[1] = 1.;
  Gx[2] = flou->classe.tri[2];
  Gy[2] = 0.;

  isymb = 1;
  itrait = 1;
  icolc = color;
  for( i=0;i<50;i++ )
    {
      legx[i] = ' ' ;
      legy[i] = ' ' ;
      legc[i] = ' ' ;
    }
  numx = 0;
  numy = 0;
  iaxex = 1;
  iaxey = 1;
  iposx = 1;
  iposy = 1;
  dy = 0.1;
  dx = 0;
  
  xmin = Gx[0]-1;
  ymin = 0;
  xmax = Gx[2]+1;
  ymax = 1.1;

  n = 3;
  strcpy( titre, flou->name );
  axex_( &numx, &iaxex, &iposx, &dx, &xmin, &xmax, legx ); 
  axey_( &numy, &iaxey, &iposy, &dy, &ymin, &ymax, legy );
  numc = 0;
  courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &icolc, legc ); 
  go_( titre );
  free( Gx );
  free( Gy );
}


/**************************************************************************/
/**************** dessin d'un nombre floue trapezoidale *******************/
/**************************************************************************/
void dessin_trapeze(nb_flou * flou,
		    int       color)
{
  int     numx,iaxex,iposx,numy,iaxey,iposy;
  float   dx,dy;
  float   xmin,xmax,ymin,ymax;
  char    legx[50];
  char    legy[50];
  float * Gx;
  float * Gy;
  int     i,numc,n,isymb,itrait,icolc;
  char    legc[50];
  char    titre[80];

/*  Gx = ( float * ) malloc( nbpoints*sizeof( float ) ); */
/*   Gy = ( float * ) malloc( nbpoints*sizeof( float ) ); */

  Gx = ( float * ) malloc( NP0*sizeof( float ) );
  Gy = ( float * ) malloc( NP0*sizeof( float ) );

  Gx[0] = flou->classe.tra[0];
  Gy[0] = 0.;
  Gx[1] = flou->classe.tra[1];
  Gy[1] = 1.;
  Gx[2] = flou->classe.tra[2];
  Gy[2] = 1.;
  Gx[3] = flou->classe.tra[3];
  Gy[3] = 0.;

 isymb = 1;
  itrait = 1;
  icolc = color;
  for( i=0;i<50;i++ )
    {
      legx[i] = ' ' ;
      legy[i] = ' ' ;
      legc[i] = ' ' ;
    }
  numx = 0;
  numy = 0;
  iaxex = 1;
  iaxey = 1;
  iposx = 1;
  iposy = 1;
  dy = 0.1;
  dx = 0;
  
  xmin = Gx[0]-1;
  ymin = 0;
  xmax = Gx[3]+1;
  ymax = 1.1;

  n = 4;
  strcpy( titre, flou->name );
  axex_( &numx, &iaxex, &iposx, &dx, &xmin, &xmax, legx ); 
  axey_( &numy, &iaxey, &iposy, &dy, &ymin, &ymax, legy );
  numc = 0;
  courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &icolc, legc ); 
  go_( titre );
  free( Gx );
  free( Gy );
}



/**************************************************************************/
/********************** dessin d'un nombre floue LR ***********************/
/**************************************************************************/
void dessin_LR(nb_flou * flou,
	       int       color)
{
  double  a,b,c;
  int     numx,iaxex,iposx,numy,iaxey,iposy;
  float   dx,dy;
  float   xmin,xmax,ymin,ymax;
  char    legx[50];
  char    legy[50];
  float * Gx;
  float * Gy;
  int     i,numc,n,isymb,itrait,icolc;
  char    legc[50];
  char    titre[80];

/*   Gx = ( float * ) malloc( nbpoints*sizeof( float ) ); */
/*   Gy = ( float * ) malloc( nbpoints*sizeof( float ) ); */

  Gx = ( float * ) malloc( NP0*sizeof( float ) );
  Gy = ( float * ) malloc( NP0*sizeof( float ) );

  n = nbpoints;
  a = flou->classe.L[0];
  b = flou->classe.L[1];
  c = flou->classe.L[2];
  for( i=0;i<(nbpoints-1)/2;i++ )
    {
      Gx[i] = a+(float)i*(b-a)/(nbpoints-1)*2;
      Gy[i] = appartient_LR( flou, Gx[i] );
    }
  Gx[(nbpoints-1)/2] = b;
 Gy[(nbpoints-1)/2] = 1.;
  for( i=1;i<=(nbpoints-1)/2;i++ )
    {
      Gx[i+(nbpoints-1)/2] = b+ i*(c-b)/(nbpoints-1)*2;
      Gy[i+(nbpoints-1)/2] = appartient_LR(flou, Gx[i+(nbpoints-1)/2] );
    }
  isymb = 1;
  itrait = 1;
  icolc = color;
  for( i=0;i<50;i++ )
    {
      legx[i] = ' ' ;
      legy[i] = ' ' ;
      legc[i] = ' ' ;
    }
  numx = 0;
  numy = 0;
  iaxex = 1;
  iaxey = 1;
  iposx = 1;
  iposy = 1;
  dy = 0.1;
  dx = 0;
  xmin = a-1;
  ymin = 0;
  xmax = c+1;
  ymax = 1.1;

  strcpy( titre, flou->name );
  axex_( &numx, &iaxex, &iposx, &dx, &xmin, &xmax, legx ); 
  axey_( &numy, &iaxey, &iposy, &dy, &ymin, &ymax, legy );
  numc = 0;
  courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &icolc, legc ); 
  go_( titre );
  free( Gx );
  free( Gy );
}

/**************************************************************************/
/********************** dessin d'un nombre floue LRG ***********************/
/**************************************************************************/
void dessin_LRG(nb_flou * flou,
	       int       color)
{
  double  a,b,c,d;
  int     numx,iaxex,iposx,numy,iaxey,iposy;
  float   dx,dy;
  float   xmin,xmax,ymin,ymax;
  char    legx[50];
  char    legy[50];
  float * Gx;
  float * Gy;
  int     i,numc,n,isymb,itrait,icolc;
  char    legc[50];
  char    titre[80];

/*   Gx = ( float * ) malloc( nbpoints*sizeof( float ) ); */
/*   Gy = ( float * ) malloc( nbpoints*sizeof( float ) ); */

  Gx = ( float * ) malloc( NP0*sizeof( float ) );
  Gy = ( float * ) malloc( NP0*sizeof( float ) );

  n = nbpoints;
  a = flou->classe.Lrg[0];
  b = flou->classe.Lrg[1];
  c = flou->classe.Lrg[2];
  d = flou->classe.Lrg[3];
  for( i=0;i<(nbpoints-1)/2;i++ )
    {
      Gx[i] = a+(float)i*(b-a)/(nbpoints-1)*2;
      Gy[i] = appartient_LRG( flou, Gx[i] );
    }
  Gx[(nbpoints-1)/2] = b;
  Gy[(nbpoints-1)/2] = 1.;
  Gx[((nbpoints-1)/2)+1] = c;
  Gy[((nbpoints-1)/2)+1] = 1.;
  
  for( i=2;i<=(nbpoints-1)/2;i++ )
    {
      Gx[i+(nbpoints-1)/2] = c+ i*(d-c)/(nbpoints-1)*2;
      Gy[i+(nbpoints-1)/2] = appartient_LRG(flou, Gx[i+(nbpoints-1)/2] );
    }
  isymb = 1;
  itrait = 1;
  icolc = color;
  for( i=0;i<50;i++ )
    {
      legx[i] = ' ' ;
      legy[i] = ' ' ;
      legc[i] = ' ' ;
    }
  numx = 0;
  numy = 0;
  iaxex = 1;
  iaxey = 1;
  iposx = 1;
  iposy = 1;
  dy = 0.1;
  dx = 0;
  xmin = a-1;
  ymin = 0;
  xmax = d+1;
  ymax = 1.1;

  strcpy( titre, flou->name );
  axex_( &numx, &iaxex, &iposx, &dx, &xmin, &xmax, legx ); 
  axey_( &numy, &iaxey, &iposy, &dy, &ymin, &ymax, legy );
  numc = 0;
  courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &icolc, legc ); 
  go_( titre );
  free( Gx );
  free( Gy );
}

/**************************************************************************/
/********** fonction d'appartenance a un triangle *************************/
/**************************************************************************/
double appartient_triangle(nb_flou * flou,
			   double    x)
{
  double mux;
  double a,b,c;

  a = flou->classe.tri[0];
  b = flou->classe.tri[1];
  c = flou->classe.tri[2];

  if (( x<= a ) || ( x >= c ) )
    mux = 0.;
  else
    {
      if (( x > a ) && ( x <= b ) )
        mux = ( x - a ) / ( b-a );
      else
        mux = ( x - c ) / ( b-c );
    }

  return mux ;
}

/**************************************************************************/
/********** fonction d'appartenance a un trapeze **************************/
/**************************************************************************/
double appartient_trapeze (nb_flou *  flou,
			   double       x)  
{
  double mux;
  double a,b,c,d;
 
 
  a = flou->classe.tra[0];
  b = flou->classe.tra[1];
  c = flou->classe.tra[2];
  d = flou->classe.tra[3];
  

  if ( ( x <= a ) || ( x >= d ) )
    mux = 0.;
  else
    { 
      if ( ( x > a ) && ( x <= b ) )
        mux = ( x - a ) / ( b-a );
      else
        {
          if ( ( x > b ) && ( x <= c ) )
            mux = 1.;
          else
            mux = ( x - d ) / ( c-d ) ;
        }
    }
  return mux ;
}

/**************************************************************************/
/*************** fonction d'appartenance a un LR **************************/
/**************************************************************************/
double appartient_LR(nb_flou * flou,
		     double    x)
{
  double mux;
  double a,b,c;
  double p,q;

  a = flou->classe.L[0];
  b = flou->classe.L[1];
  c = flou->classe.L[2];
  p = flou->classe.L[3];
  q = flou->classe.L[4];

  if ( ( x <= a ) || ( x >= c ) )
    mux = 0.;
  else
    {
      if ( ( x > a ) && ( x <= b ) )
        mux = 1 - puissance( (b-x)/(b-a), p );
      else
        mux = 1 - puissance( (x-b)/(c-b), q );
    }
  return mux ;
}   

/**************************************************************************/
/*************** fonction d'appartenance a un LRG **************************/
/**************************************************************************/
double appartient_LRG(nb_flou * flou,
		     double    x)
{
  double mux;
  double a,b,c,d;
  double p,q;

  a = flou->classe.Lrg[0];
  b = flou->classe.Lrg[1];
  c = flou->classe.Lrg[2];
  d = flou->classe.Lrg[3];
  p = flou->classe.Lrg[4];
  q = flou->classe.Lrg[5];

 /* printf("%f a, %f b, %f c, %f d, %f p, %f q \n",a,b,c,d,p,q); */
  if ( ( x <= a ) || ( x >= d ) )
    mux = 0.;
  else
    {
      if ( ( x > a ) && ( x <= b ) )
        mux = 1 - puissance( ( b - x ) / ( b - a ), p );
      else
	{
	  if( (x > b) && ( x <= c) )
	    mux=1.;
	  else
	    mux = 1 - puissance( ( x - c) / ( d - c ), q );
	}
    }
  return mux ;
}   

/**************************************************************************/
/******************** discretisation d'un triangle ************************/
/**************************************************************************/
discret discretise_triangle(nb_flou * flou)
{
  double  a,c,tmp;
  point * nouv;
  int     i;
  discret result;
   
  a = flou->classe.tri[0];
  c = flou->classe.tri[2];

  /*printf("	discretise triangle\n");*/
  result = NULL;
  for( i=PREC;i>=0;i--)
    {
      nouv = ( point * )malloc( sizeof( point ) );
      /* MODIF 17-11-97 Test de l'allocation */
      if (nouv==NULL)
	{
	  affiche_erreur(12,0,NULL);
	  /* ERREUR : Ps d'allocation pour la discretisation Triangle */
	}
      /*printf("	x; c: %5.5lf, a: %5.5lf, i : %i, PREC : %i, x:%5.5lf \n",c,a,i,PREC,
	(c-a)*(double)i/(double)PREC+a);
      */
      /* MODIF 18-11-97 MOREL L. Il y a un plantage  sur ce calcul lorsque tmp==0 
       */
      /*tmp=(double)((c-a)*(double)i);
	if (tmp == 0.0 ) 
	  nouv->x=(double)0.0;
	  else
	  nouv->x =(double)( tmp/(double)PREC+a);
      */
      nouv->x = (c-a)*(double)i/(double)PREC+a;
      nouv->y = appartient_triangle( flou, nouv->x );
      /*printf("	y fin\n");*/
      nouv->suiv = result;
      result = nouv;
      nouv = NULL;
    }
  /*printf("	return result\n");*/
  return( result );
}


/**************************************************************************/
/******************** discretisation d'un trapeze *************************/
/**************************************************************************/
discret discretise_trapeze(nb_flou * flou)
{
  double  a,d;
  point * nouv;
  int     i;
  discret result;
   
  a = flou->classe.tra[0];
  d = flou->classe.tra[3];

/*/printf("	discretise trapeze\n"); */
  result = NULL;
  for( i=PREC;i>=0;i--)
    {
      nouv = ( point * )malloc( sizeof( point ) );
      /* MODIF 17-11-97 Test de l'allocation */
      if (nouv==NULL)
	{	  
	  affiche_erreur(12,0,NULL);
	  /* ERREUR : Ps d'allocation pour la discretisation trapeze */
	}

      nouv->x = (d-a)*(double)i/(double)PREC+a;
      nouv->y = appartient_trapeze( flou, nouv->x );
      nouv->suiv = result;
      result = nouv;
      nouv = NULL;
    }
  return( result );
}



/**************************************************************************/
/*********************** discretisation d'un LR ***************************/
/**************************************************************************/
discret discretise_LR(nb_flou * flou)
{
  double  a,c;
  point * nouv;
  int     i;
  discret result;
   
  a = flou->classe.L[0];
  c = flou->classe.L[2];

  /*printf("	discretise LR\n");*/
  result = NULL;
  for( i=PREC;i>=0;i--)
    {
      nouv = ( point * )malloc( sizeof( point ) );
      /* MODIF 17-11-97 Test de l'allocation */
      if (nouv==NULL)
	{
	  affiche_erreur(12,0,NULL);
	  /* ERREUR : Ps d'allocation pour la discretisation LR */
	}

      nouv->x = (c-a)*(double)i/(double)PREC+a;
      nouv->y = appartient_LR( flou, nouv->x );
      nouv->suiv = result;
      result = nouv;
      nouv = NULL;
    }
  return( result );
}


/**************************************************************************/
/*********************** discretisation d'un LRG ***************************/
/**************************************************************************/
discret discretise_LRG(nb_flou * flou)
{
  double  a,d;
  point * nouv;
  int     i;
  discret result;
   
  a = flou->classe.Lrg[0];
  d = flou->classe.Lrg[3];

  /* printf("	discretise LRG\n"); */
  
  result = NULL;
  for( i=PREC;i>=0;i--)
    {
      nouv = ( point * )malloc( sizeof( point ) );
      /* MODIF 17-11-97 Test de l'allocation */
      if (nouv==NULL)
	{
	  affiche_erreur(12,0,NULL);
	  /* ERREUR : Ps d'allocation pour la discretisation LRG */
	}

      nouv->x = (d-a)*(double)i/(double)PREC+a;
      nouv->y = appartient_LRG( flou, nouv->x );
      nouv->suiv = result;
      result = nouv;
      nouv = NULL;
    }
  return( result );
}


