#define MOD discrets

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "discrets.h"
#include "dessin.h"
#include "definitions.h"
#include "maths.h"


/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/




void LibereDiscret(discret Dis)

/* Modif 14-10-97 */
/* Parcours la liste des points de Dis  et les liberes un par un les cases memoire, par appel recursif */
{
  
  if (Dis->suiv==NULL) 
    {
      free(Dis);
      /* printf("DEBUG :: regle.c LibereDis \n"); */
    }
  else
    {
      LibereDiscret(Dis->suiv);
      Dis->suiv = NULL;
      LibereDiscret(Dis);
    }     
}




/**************************************************************************/
/********************* dessin d'un tableau discret ************************/
/**************************************************************************/
void dessin_discret(discret dis,
		    int     color)
{
  point * vue;
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

              /*********************************************/
              /*            mise a jour des tires          */
              /* ! tous les carateres doivent etre remplis */
              /*********************************************/
  for( i=0;i<50;i++ )
    {
      legx[i] = ' ';
      legy[i] = ' ';
    }
  for( i=0;i<80;i++ )
    titre[i] = ' ';
  strcpy( legc , "              variable floue de sortie            ");
  for ( i=strlen(legc);i<50;i++);
    legc[i] =' ';
  isymb = 1;
  itrait = 1;
  icolc = color;
  numx = 0;
  numy = 0;
  iaxex = 1;
  iaxey = 1;
  iposx = 1;
  iposy = 1;
  dy = 0.1;
  dx = 0;
  numc = 0;


                   /********************************************/
                   /*    comptage du nombre de points ( n )    */
                   /* et determination des extremums en x et y */
                   /********************************************/
  ymin = 0;
  xmax = 0;
  ymax = 1.1;
  vue = dis;
  n = 1;
  xmin = vue->x;
  while( vue->suiv != NULL )
    {
      n = n+1;
      vue = vue->suiv;
    }
  xmax  = vue->x;

  axex_( &numx, &iaxex, &iposx, &dx, &xmin, &xmax, legx ); 
  axey_( &numy, &iaxey, &iposy, &dy, &ymin, &ymax, legy );

                  /****************************/
                  /* remplissage des tableaux */
                  /****************************/
  vue = dis;

/*   Gx = ( float * ) malloc( n*sizeof( float ) ); */
/*   Gy = ( float * ) malloc( n*sizeof( float ) ); */

  Gx = ( float * ) malloc( NP0*sizeof( float ) );
  Gy = ( float * ) malloc( NP0*sizeof( float ) );
  i = 0;
  while( vue != NULL )
    {
      Gx[i] = vue->x;
      Gy[i] = vue->y;
      vue = vue->suiv;
      i  = i + 1;
    }
  courbe_(&numc, Gx, Gy, &n, &isymb, &itrait, &icolc, legc );
  go_( titre );
  free( Gx );
  free( Gy );
}


/**************************************************************************/
/*********** multiplication par un scalaire d'un discret ******************/
/**************************************************************************/
void mult_sca(discret dis,
	      double  x)
{
  point * vue;
 
  vue = dis;
  while( vue != NULL )
    {
      vue->y  = vue->y*x;          /* chaque point est affecte */
      vue = vue->suiv;
    } 
}

/**************************************************************************/
/**************** fonction d'appartenance a un discret ********************/
/**************************************************************************/
void app_discret(double * rep,
		 discret dis,
		 double  x)
{
  point * vue;
  point * vue1;
  double  a,b;

 
  vue = dis;
  if( x <= vue->x )     /* x est-il avant l'ensemble de definition de dis ? */
    (*rep) = 0.;        /* oui -> rep = 0 */
  else                  /* non */
    {
      while(( vue != NULL )&&( x > vue->x ) ) /* cherche si x est dans      */
        {                                     /* l'intervalle de def de dis */
          vue1 = vue;                         /* et si oui entre quels points */
          vue = vue->suiv;
        }
      if ( vue == NULL )                      /* x est apres l'intervalle */
        (*rep) = 0.;
      else                                    /* x est dedans */
        {
          a = ( vue->y - vue1->y )/( vue->x - vue1->x );
          b = ( vue->x*vue1->y - vue->y*vue1->x )/( vue->x - vue1->x );
          (*rep) = a*x + b;

        }
    }
}


/**************************************************************************/
/**************** minimum de deux tableaux discrets ***********************/
/**************************************************************************/
discret min_discret(discret dis,
		    discret dis1)
{
  discret dis2;
  double  inf,sup;
  point * vue;
  double  sup1,sup2;
  point * nouv; 
  double  y1,y2;
  int     i;


                  /***********************************************/
                  /* recherche d'un intervalle unique ou peuvent */
                  /* etre definis les deux discrets              */
                  /***********************************************/
  inf = min( dis->x, dis1->x );

  vue = dis;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  sup1 = vue->x;
  vue = dis1;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  sup2 = vue->x;
  sup = max( sup1, sup2 );
                   
                  /****************************************/
                  /* calcul du minimum sur cet intervalle */
                  /****************************************/
  dis2 = NULL;
  for(i=PREC;i>=0;i--)
    {
      nouv = ( point * )malloc( sizeof( point ) );
                 /*****************************/
                 /* decoupage de l'intervalle */
                 /*****************************/
      nouv->x = (sup-inf)*(double)i/(double)PREC+inf; 
                 /**********************************************************/
                 /* recalcule les fonctions d'appartenance et prend le min */
                 /**********************************************************/   
      app_discret( &y1, dis, nouv->x );
      app_discret( &y2, dis1, nouv->x );
      nouv->y = min( y1, y2 );
      nouv->suiv = dis2;
      dis2 = nouv;
    }
  return( dis2 );
}       


/**************************************************************************/
/**************** maximum de deux tableaux discrets ***********************/
/**************************************************************************/
discret max_discret(discret dis,
		    discret dis1)
{
  discret dis2;
  double  inf,sup;
  point * vue;
  double  sup1,sup2;
  point * nouv; 
  double  y1,y2;
  int     i;

                  /***********************************************/
                  /* recherche d'un intervalle unique ou peuvent */
                  /* etre definis les deux discrets              */
                  /***********************************************/
  inf = min( dis->x, dis1->x );

  vue = dis;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  sup1 = vue->x;
  vue = dis1;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  sup2 = vue->x;
  sup = max( sup1, sup2 );

                  /****************************************/
                  /* calcul du maximum sur cet intervalle */
                  /****************************************/
  dis2 = NULL;
  for(i=PREC;i>=0;i--)
    {
      nouv = ( point * )malloc( sizeof( point ) );
                 /*****************************/
                 /* decoupage de l'intervalle */
                 /*****************************/
      nouv->x = (sup-inf)*(double)i/(double)PREC+inf;
                 /**********************************************************/
                 /* recalcule les fonctions d'appartenance et prend le max */
                 /**********************************************************/ 
      app_discret( &y1, dis, nouv->x );
      app_discret( &y2, dis1, nouv->x );
      nouv->y = max( y1, y2 );
      nouv->suiv = dis2;
      dis2 = nouv;
    }
  return( dis2 );
}    


/**************************************************************************/
/**************** produit de deux tableaux discrets ***********************/
/**************************************************************************/
discret prod_discret(discret dis,
		     discret dis1)
{
  discret dis2;
  double  inf,sup;
  point * vue;
  double  sup1,sup2;
  point * nouv; 
  double  y1,y2;
  int     i;

                  /***********************************************/
                  /* recherche d'un intervalle unique ou peuvent */
                  /* etre definis les deux discrets              */
                  /***********************************************/
  inf = min( dis->x, dis1->x );

  vue = dis;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  sup1 = vue->x;
  vue = dis1;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  sup2 = vue->x;
  sup = max( sup1, sup2 );

                  /****************************************/
                  /* calcul du produit sur cet intervalle */
                  /****************************************/
  dis2 = NULL;
  for(i=PREC;i>=0;i--)
    {
      nouv = ( point * )malloc( sizeof( point ) );
                 /*****************************/
                 /* decoupage de l'intervalle */
                 /*****************************/
      nouv->x = (sup-inf)*(double)i/(double)PREC+inf;
                 /***********************************************************/
                 /* recalcule les fonctions d'appartenance et prend le prod */
                 /***********************************************************/ 
      app_discret( &y1, dis, nouv->x );
      app_discret( &y2, dis1, nouv->x );
      nouv->y = y1 * y2 ;
      nouv->suiv = dis2;
      dis2 = nouv;
    }
  return( dis2 );
}    

/**************************************************************************/
/************************* ajout en fin de liste **************************/
/**************************************************************************/
void ajout_fin( discret * dis,
		double    x,
		double    y)
{
  point * vue;
  point * nouv;

                  /**********************************************/
                  /* allocation et remplissage du nouveau point */
                  /**********************************************/
  nouv = ( point * ) malloc( sizeof( point ) );
  nouv->x = x;
  nouv->y = y;
  nouv->suiv = NULL;
  vue = (*dis);

                  /********************************/
                  /* recherche de la fin de liste */
                  /********************************/
  if ( vue == NULL )
    (*dis) = nouv;             /* liste vide -> ajout en tete */
  else
    {
      while( vue->suiv != NULL )
        vue = vue->suiv;
      vue->suiv = nouv;
    } 
}

/**************************************************************************/
/************************* recopie d'un discret ***************************/
/**************************************************************************/
discret copie(discret dis)
{
  discret dis1;
  point * vue;

  vue = dis;
  dis1 = NULL;
  while( vue != NULL )
    {
      ajout_fin( &dis1, vue->x, vue->y );
      vue = vue->suiv;
    }
  return( dis1 );
}      
  


/**************************************************************************/
/********************** combinaison par minimum ***************************/
/**************************************************************************/
discret comb_minimum(discret dis1,
		     double  mu1,
		     discret dis2,
		     double  mu2)
{
  discret dis11;
  discret dis22;
  discret result;
  
  dis11 = copie( dis1 );
  dis22 = copie( dis2 );

  mult_sca( dis11, mu1 );
  mult_sca( dis22, mu2 );
  result = min_discret( dis11, dis22 );
  LibereDiscret(dis1);
  LibereDiscret(dis11);
  LibereDiscret(dis22);
  return( result );
}


/**************************************************************************/
/*********************** ecretage d'un discret ****************************/
/**************************************************************************/
void ecrete(discret dis,
	    double  mu)
{
  point * vue;

  vue = dis;
  while( vue != NULL )
    {
      if ( vue->y > mu )      
        vue->y = mu;
      vue = vue->suiv;
    }
}


/**************************************************************************/
/***************** combinaison par minimum avec crete *********************/
/**************************************************************************/
discret comb_mini_crete(discret dis1,
			double  mu1,
			discret dis2,
			double  mu2)
{
  discret dis11;
  discret dis22;
  discret result;
 
  dis11 = copie( dis1 );
  dis22 = copie( dis2 );

  ecrete( dis11, mu1 );
  ecrete( dis22, mu2 );
  result = min_discret( dis11, dis22 );
  LibereDiscret(dis1);
  LibereDiscret(dis11);
  LibereDiscret(dis22);
  return( result );
}



/**************************************************************************/
/********************** combinaison par maximum ***************************/
/**************************************************************************/
discret comb_maximum(discret dis1,
		     double  mu1,
		     discret dis2,
		     double  mu2)
{
  discret dis11;
  discret dis22;
  discret result;
  
  dis11 = copie( dis1 );
  dis22 = copie( dis2 );

  mult_sca( dis11, mu1 );
  mult_sca( dis22, mu2 );
  result = max_discret( dis11, dis22 );
  LibereDiscret(dis1);
  LibereDiscret(dis11);
  LibereDiscret(dis22);
  return( result );
}
  

/**************************************************************************/
/***************** combinaison par maximum avec crete *********************/
/**************************************************************************/
discret comb_maxi_crete(discret dis1,
			double  mu1,
			discret dis2,
			double  mu2)
{
  discret dis11;
  discret dis22;
  discret result;
 
  dis11 = copie( dis1 );
  dis22 = copie( dis2 );

  ecrete( dis11, mu1 );
  ecrete( dis22, mu2 );

  result = max_discret( dis11, dis22 );
  LibereDiscret(dis1);
  LibereDiscret(dis11);
  LibereDiscret(dis22);
  return( result );
}


/**************************************************************************/
/********************* combinaison par somme ponderee *********************/
/**************************************************************************/
discret comb_somme_pond( discret dis1,
			 double  mu1,
			 discret dis2,
			 double  mu2)
{
  discret result;
  discret dis11;
  discret dis22;
  double  inf,sup;
  point * vue;
  double  sup1,sup2;
  point * nouv; 
  double  y1,y2;
  int     i;
  double  maxi;


  dis11 = copie( dis1 );
  dis22 = copie( dis2 ); 
  mult_sca( dis11, mu1 );
  mult_sca( dis22, mu2 );

                  /***********************************************/
                  /* recherche d'un intervalle unique ou peuvent */
                  /* etre definis les deux discrets              */
                  /***********************************************/
  inf = min( dis11->x, dis22->x );

  vue = dis11;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  sup1 = vue->x;
  vue = dis22;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  sup2 = vue->x;
  sup = max( sup1, sup2 );

  maxi = 0.;
  result = NULL;
  for(i=PREC;i>=0;i--)
    {
      nouv = ( point * )malloc( sizeof( point ) );
                 /*****************************/
                 /* decoupage de l'intervalle */
                 /*****************************/
      nouv->x = (sup-inf)*(double)i/(double)PREC+inf;
               /***************************************************************/
               /* calcul des fonctions d'appartenance et de la somme ponderee */
               /***************************************************************/
      app_discret( &y1, dis11, nouv->x );
      app_discret( &y2, dis22, nouv->x );
      nouv->y = (y1+y2);
      if ( nouv->y > maxi )
        maxi = nouv->y;
      nouv->suiv = result;
      result = nouv;
    }
  vue = result;
  while( vue != NULL )
    {
      vue->y = vue->y / maxi;
      vue = vue->suiv;
    }
  LibereDiscret(dis1);
  LibereDiscret(dis11);
  LibereDiscret(dis22);
  return( result );
}

/**************************************************************************/
/************************ integration d'un discret ************************/
/**************************************************************************/
double integre( discret dis,
		double  min,
		double  max)
{
  double  rep;
  point * vue;
  double  y;

  rep = 0;
  vue = dis;
  while( vue != NULL )
    {
      if ((vue->suiv != NULL )&&(vue->suiv->x < max )&&
          ( vue->x >= min)&&(vue->x <= max ))
        rep = rep + ( vue->y + vue->suiv->y )*( vue->suiv->x - vue->x )/2;
      else
        if ((vue->suiv != NULL )&&( vue->x >= min)&&(vue->x <= max ))
          {
            app_discret( &y, dis, max );
            rep = rep + ( vue->y + y ) * ( max - vue->x )/2;
          }
      vue = vue->suiv;
    }
  return( rep );
}
  

/**************************************************************************/
/****************integration ponderee d'un discret ************************/
/**************************************************************************/
double integre_pond( discret dis,
		     double  min,
		     double  max)
{
  double  rep;
  point * vue;
  double  y;

  rep = 0;
  vue = dis;
  while( vue != NULL )
    {
      if ((vue->suiv != NULL )&&(vue->suiv->x < max )&&
          ( vue->x >= min)&&(vue->x <= max ))
        rep = rep + (vue->suiv->x*vue->y+vue->suiv->x*vue->suiv->y)*( vue->suiv->x - vue->x )/2;
      else
        if ((vue->suiv != NULL )&&( vue->x >= min)&&(vue->x <= max ))
          {
            app_discret( &y, dis, max );
            rep = rep + (max*vue->y + max*y)  * ( max - vue->x )/2;
          }
      vue = vue->suiv;
    }
  return( rep );
}


/**************************************************************************/
/**************** deflouification par centre de gravite *******************/
/**************************************************************************/
double deflou_gravite(discret dis,
		      int     dessin)
{
  double  rep;
  point * vue;
  double  min;
  double  max;
  
  if ( dessin == 1 )
    dessin_discret( dis, 2 );

  min = dis->x;

  vue = dis;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  max = vue->x;
  rep = integre_pond( dis, min, max ) / integre( dis, min, max );

  return( rep );
}


/**************************************************************************/
/**************** deflouification par moyenne des max *********************/
/**************************************************************************/
double deflou_max( discret dis,
		   int     dessin)
{
  point * vue;
  double  max;
  double  nb;
  double  rep;

  max = 0.;
  vue = dis;
  while( vue != NULL )
    {
      if( vue->y > max )
        max = vue->y;
      vue = vue->suiv;
    }
  nb = 0.;
  rep = 0.;
  vue = dis;
  while( vue != NULL )
    {
      if ( vue->y == max )
        {
          rep = rep + vue->x;
          nb = nb+1.;
        }
      vue = vue->suiv;
    }
  if ( dessin == 1 )
    dessin_discret( dis, 2 );
 

  return( rep/nb );
}


/**************************************************************************/
/******************* deflouification par point median *********************/
/**************************************************************************/
double deflou_median( discret dis,
		      double  erreur,
		      int     dessin)
{
  double  min,max;
  double  inf,sup;
  point * vue;
  double  milieu;
  double int1,int2;
  int    compteur;

  min = dis->x;
  vue = dis;
  while( vue->suiv != NULL )
    vue = vue->suiv;
  max = vue->x;
  milieu = (min+max)/2;

  int1 = integre( dis, min, milieu );
  int2 = integre( dis, milieu, max );

  sup = max;
  inf = min;
  compteur = 0;
  while( (fabs( (double)(int2-int1) ) > erreur) && (compteur < 100) )
    {
      compteur++;
      if ( int1 > int2 )
        sup = milieu;
      else
        inf = milieu;
      milieu = ( inf+sup )/2;
      int1 = integre( dis, min, milieu );
      int2 = integre( dis, milieu, max );
    }
  if ( dessin == 1 )
    dessin_discret( dis, 2 );

  return( milieu );
}
