/* Derniere mise a jour 14-05-98*/
#define MOD_fichiers
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "fichiers.h"
#include "nb_flous.h"
#include "piles.h"
#include "regles.h"
#include "classes_floues.h"
#include "definitions.h"
#include "erreur.h"
#include "maths.h"
#include "E_S.h"


/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/

/*************************************************************************/
/******************** lecture fichier de variable floues *****************/
/*************************************************************************/
classe_floue * lecture_nb_floues( FILE   * fichier,
				  erreur * err)
{
  char           carac;
  int            i,j,nb;
  classe_floue * floue;
  int            type;
  nb_flou      * flou;
  char         * nom;
           
                    /***************************/
                    /* allocation de la classe */
                    /***************************/
  floue = ( classe_floue * )(malloc( sizeof( classe_floue ) ) );
  if ( floue != NULL )
    {
      carac = lire( fichier );
             /***************************************/
             /* on passe au dessus des commentaires */
             /***************************************/
      while( carac == '#' )
        {
          while( carac != '\n')
            carac = lire( fichier );
          carac = lire( fichier );
        }
            /**************************************/
            /* on recupere le nombre de variables */
            /**************************************/
      nb = lireint( fichier, &carac, err );
      floue->nb = nb;
      carac = lireblanc( fichier, carac );
      i = 1;
            /************************************/
            /* lecture de chaque variable floue */
            /************************************/
      j = lireint( fichier,&carac, err );
      while( i <= nb )
        {
            /********************************/
            /* on lit le nom de la variable */
            /********************************/
          carac = lireblanc( fichier, carac );
          nom = lirenom( fichier, &carac, err );
          floue->tab[i-1] = cree_variable_floue( nom );
          if ( floue->tab[i-1] == NULL )   /* allocation mauvaise -> erreur */
            {
              (*err) = 13;
              return( NULL );
            }
          carac = lireblanc( fichier, carac );
             /**************************************************/
             /* on lit les nombres flous qui lui sont associes */
             /**************************************************/
          j = lireint( fichier, &carac, err );
          while( j == i )
            { 
                          /************************/
                          /* on recupere son type */
                          /************************/
              carac = lireblanc( fichier, carac );
              type = lireint( fichier, &carac, err );
      	      switch( type )
                {
                  case trian:
                    flou = lecture_triangle( fichier, &carac );
                    break;
	          case trap:
                    flou = lecture_trapeze( fichier, &carac );
                    break;
                  case lr:
                    flou = lecture_LR( fichier, &carac );
                    break;
   		  case lrg:
		    flou = lecture_LRG( fichier, &carac );
		    break;

                 }
              (*err) = 0;
              ajout_nb_flou( floue->tab[i-1], flou, err ); 
              if ( (*err) != 0 )
                return( NULL );
              carac = lireblanc( fichier, carac );
              /*if ( carac == '-' ) MODIF 19-01-97 MOREL L. le teste de la 
	       fin du fichier ne se fait plus par -1, mais par la fonction
	      qui permet de savoir si on est a la fin du fichier.*/ 
	      if ( feof( fichier ))
                {
                  i = nb+1;
                  j = i+1;
                }
              else  
		j = lireint( fichier,&carac,err);/* lecture de la ligne suivante */
            }
          i = i + 1;
        }   
    } 
  else
    (*err) = 12; 
  return( floue );                         
}
      

/**************************************************************************/
/***************** test de la definition d'un triangle ********************/
/**************************************************************************/
erreur test_triangle(FILE    * fichier,
		     char    * carac,
		     int     * nb_ligne,
		     pilenom * Pi)
{
  erreur err;
  double a,b,c;
  char * nom;
  
  err = 0;
                     /*************************/
                     /* lecture du 1er nombre */
                     /*************************/
  a = lirenb( fichier, carac, &err );
  if ( err != 0 )                     /* probleme -> retourne erreur */
    return( err );
  
  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 2eme nombre */
                    /**************************/
  b = lirenb( fichier, carac, &err );
   if ( err != 0 )                    /* probleme -> retourne erreur */
     return( err );
  if ( b < a )                        /* regarde si l'ordre est respecte */
    return( 8 );
  if ( a == b )
    return( 113 );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 3eme nombre */
                    /**************************/
  c = lirenb( fichier, carac, &err );
  if ( err != 0 )                     /* probleme -> retourne erreur */
    return( err );
  if ( c < b )                        /* regarde si l'ordre est respecte */
    return( 8 );
  if ( b == c ) 
    return( 113 );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                   /*********************************/
                   /* lecture du nom du nombre flou */
                   /*********************************/
  nom = lirenom( fichier, carac, &err );
  if ( err != 0 )                     /* probleme -> retourne erreur */
    return( err );
  err = ajout_nom( nom, Pi );         /* regarde si le nom est nouveau */
  return( err );
}


/**************************************************************************/
/***************** test de la definition d'un trapeze *********************/
/**************************************************************************/
erreur test_trapeze( FILE    * fichier,
		     char    * carac,
		     int     * nb_ligne,
		     pilenom * Pi)
{
  erreur err;
  double a,b,c,d;
  char * nom;
  
  err = 0;
                    /*************************/
                    /* lecture du 1er nombre */
                    /*************************/
  a = lirenb( fichier, carac, &err );
  if ( err != 0 )                        /* probleme -> retourne erreur */
    return( err );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 2eme nombre */
                    /**************************/
  b = lirenb( fichier, carac, &err );
  if ( err != 0 )                        /* probleme -> retourne erreur */
     return( err );
  if ( b < a )                           /* regarde si l'ordre est respecte */
    return( 8 );
  if ( a == b )
    return( 113 );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 3eme nombre */
                    /**************************/
  c = lirenb( fichier, carac, &err );
  if ( err != 0 )                        /* probleme -> retourne erreur */
    return( err );
  if ( c < b )                           /* regarde si l'ordre est respecte */
    return( 8 );
  if ( b == c )
    return( 114 );
  
  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 4eme nombre */
                    /**************************/
  d = lirenb( fichier, carac, &err );
  if ( err != 0 )                        /* probleme -> retourne erreur */
    return( err );
  if ( d < c )                           /* regarde si l'ordre est respecte */
    return( 8 );
  if ( c == d )
    return( 113 );
 
  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                   /*********************************/
                   /* lecture du nom du nombre flou */
                   /*********************************/
  
  nom = lirenom( fichier, carac, &err );
  if ( err != 0 )                        /* probleme -> retourne erreur */
    return( err );
  err = ajout_nom( nom, Pi );           /* regarde si le nom est nouveau */
  return( err );
}


/**************************************************************************/
/********************* test de la definition d'un LR **********************/
/**************************************************************************/
erreur test_LR(FILE    * fichier,
	       char    * carac,
	       int     * nb_ligne,
	       pilenom * Pi)
{
  erreur err;
  double a,b,c,p,q;
  char * nom;
  
  err = 0;
                    /*************************/
                    /* lecture du 1er nombre */
                    /*************************/
  a = lirenb( fichier, carac, &err );
  if ( err != 0 )                        /* probleme -> retourne erreur */
    return( err );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 2eme nombre */
                    /**************************/
  b = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
     return( err );
  if ( b < a )                          /* regarde si l'ordre est respecte */
    return( 8 );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 3eme nombre */
                    /**************************/
  c = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );
  if ( c < b )                          /* regarde si l'ordre est respecte */
    return( 8 );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 4eme nombre */
                    /**************************/
  p = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 5eme nombre */
                    /**************************/
  q = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                   /*********************************/
                   /* lecture du nom du nombre flou */
                   /*********************************/
  nom = lirenom( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );
  err = ajout_nom( nom, Pi );           /* regarde si le nom est nouveau */
  return( err );
}

/**************************************************************************/
/********************* test de la definition d'un LRG **********************/
/**************************************************************************/
erreur test_LRG(FILE    * fichier,
	       char    * carac,
	       int     * nb_ligne,
	       pilenom * Pi)
{
  erreur err;
  double a,b,c,d,p,q;
  char * nom;
  
  err = 0;
                    /*************************/
                    /* lecture du 1er nombre */
                    /*************************/
  a = lirenb( fichier, carac, &err );
  if ( err != 0 )                        /* probleme -> retourne erreur */
    return( err );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 2eme nombre */
                    /**************************/
  b = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
     return( err );
  if ( b < a )                          /* regarde si l'ordre est respecte */
    return( 8 );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                    /**************************/
                    /* lecture du 3eme nombre */
                    /**************************/
  c = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );
  if ( c < b )                          /* regarde si l'ordre est respecte */
    return( 8 );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );

                    /**************************/
                    /* lecture du 4eme nombre */
                    /**************************/
  d = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );
  if ( d < c )                          /* regarde si l'ordre est respecte */
    return( 8 );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );

                    /**************************/
                    /* lecture du 5eme nombre */
                    /**************************/
  p = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );

                    /**************************/
                    /* lecture du 6eme nombre */
                    /**************************/
  q = lirenb( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );

  (*carac) = lireblanc1( fichier, (*carac), nb_ligne );
                   /*********************************/
                   /* lecture du nom du nombre flou */
                   /*********************************/
  nom = lirenom( fichier, carac, &err );
  if ( err != 0 )                       /* probleme -> retourne erreur */
    return( err );
  err = ajout_nom( nom, Pi );           /* regarde si le nom est nouveau */
  return( err );
}

/**************************************************************************/
/********************* test fichier variables floues **********************/
/**************************************************************************/ 
erreur test_floue( FILE * fichier,
		   int  * nb_ligne)
{
  char    carac;
  int     nb;
  int     i,j;
  erreur  err;
  char  * nom;
  int     type;
  pilenom Pi;


  Pi = NULL;
  err = 0;
  (*nb_ligne) = 1;
  carac = lire( fichier );
    
             /*********************************/
             /* verification des commentaires */
             /*********************************/
  while( carac == '#' )
    {
      while( carac != '\n')
        carac = lire( fichier );
      (*nb_ligne) = (*nb_ligne) + 1;
      carac = lire( fichier );
    }
             /************************************/
             /* on verifie le nombre de variable */
             /************************************/
  carac = lireblanc1( fichier, carac, nb_ligne );
  nb = lireint( fichier, &carac, &err );
  if ( err != 0 )                         /* probleme -> retourne erreur */
    return( err );
  if ( ( nb == 0 )||( nb > NBVARMAX ) )  /* regarde si le nombre est correct */
    return( 1 );
   carac = lireblanc1( fichier, carac, nb_ligne );
  i = 1;

             /*****************************************/
             /* verification de chaque variable floue */
             /*****************************************/
 j = lireint( fichier, &carac, &err );  /* lecture du 1er chiffre */

   if ( err != 0 )                      /* probleme -> retourne erreur */
    return( err );
  if ( j != 1 )                   /* regarde si la 1ere varia est definie */
    return( 2 );
 
/* DEBUG
printf("DEBUG fichier.c :: Verification des variables. Nombre : %i\n",nb);*/

  while( i <= nb )
    {
             /**********************************/
             /* on verifie le nom de variables */
             /**********************************/
      carac = lireblanc1( fichier, carac, nb_ligne );
      nom = lirenom( fichier, &carac, &err );
      if ( err != 0 )                   /* probleme -> retourne erreur */
        return( err );
      err = ajout_nom( nom, &Pi );      /* regarde si le nom est nouveau */
      if ( err != 0 )
        return( err );
      carac = lireblanc1( fichier, carac, nb_ligne );
             /******************************************************/
             /* on verifie les nombres flous qui lui sont associes */
             /******************************************************/
      j = lireint( fichier, &carac, &err );
      if ( err != 0 )                  /* probleme -> retourne erreur */
        return( err );
      if( j != i )                     /* regarde s'il y en a au moins 1 */
        return( 4 );
        while( j == i )
        {
          carac = lireblanc1( fichier, carac, nb_ligne );
             /********************************/
             /* on verifie le type du nombre */
             /********************************/
          type = lireint( fichier, &carac, &err );
          if ( err != 0 )              /* probleme -> retourne erreur */
            return( err );
          
          carac = lireblanc1( fichier, carac, nb_ligne );

          switch( type )
            {
              case trian:
                err = test_triangle( fichier, &carac, nb_ligne, &Pi ); 
                if ( err != 0 )
                  return( err );
                break;
              case trap:
                err = test_trapeze( fichier, &carac, nb_ligne, &Pi );         
                if ( err != 0 )
                  return( err );
                 break;
              case lr:
                err = test_LR( fichier, &carac, nb_ligne, &Pi );
                if ( err != 0 )
                  return( err );
                break;
	      case lrg:
		err= test_LRG( fichier, &carac, nb_ligne, &Pi );
		if ( err != 0 )
                  return( err );
                break;
              default:            /* le type n'est pas defini ->erreur */
                return( 9 );
            }
          carac = lireblanc1( fichier, carac, nb_ligne );
          /* if ( carac == '-' ) MODIF 19-01-97 MOREL L. le teste de la 
	       fin du fichier ne se fait plus par -1, mais par la fonction
	      qui permet de savoir si on est a la fin du fichier.*/
	  if ( feof( fichier )) 
            return( 0 );
          else
            {
              j = lireint( fichier, &carac, &err ); /* lecture de la ligne */
              if ( err != 0 )                       /* suivante            */
                return( err );                 /* probleme -> erreur */
            }
        }
      if ( j != i+1 )       /* regarde si la variable suivante est definie  */
        return( 2 );
      else
        {
          if ( j > nb )
            return( 115 );
          else
            i = i + 1;
        }
          
    }
}



/**************************************************************************/
/*************************** lire fichier entree **************************/
/**************************************************************************/ 
void lire_entree(FILE * fichier)
{
  char           carac;
  char         * nom;
  int            i;
  classe_floue * floues;
  FILE         * fichier1;
  FILE         * donnee;
  pile           pi;
  pile           post;
  list_regle   * regles;
  int            num;
  char         * result;
  char         * fic_int;
  int            fic_inter;
  int            config[4];
  int            deflou;
  int            combi;
  int            res_model;
  int            validite;
  erreur         err;
  erreur         errf;
  int            n_ligne;
  char         * ch_err;
  int            varia;
  char         * title_sortie;

  ch_err = (char *) malloc (LONGMOT*sizeof(char));
  carac = lire( fichier );
     
                   /****************************/
                   /* lecture des commentaires */
                   /****************************/  
  while( carac == '#' )
    {
      while( carac != '\n')
        carac = lire( fichier );
      carac = lire( fichier );
    }
 
  nom = ( char * ) malloc( longfic*sizeof( char ) );
  result = ( char * ) malloc( longfic*sizeof( char ) );
  fic_int = ( char * ) malloc( longfic*sizeof( char ) );
  i = 0;

                   /**************************************************/
                   /* lecture du nom du fichier des variables floues */
                   /**************************************************/
  while( carac != '\n' )
    {
      nom[i] = carac;
      i = i + 1;
      carac = lire( fichier );
    }
  nom[i] =  '\0';
          
  fichier1 = fopen( nom, "r" );
  printf("... reading fuzzy variable file %s \n",nom );
         /* regarde s'il y a des erreurs dans ce fichier */
  
  
  errf = test_floue( fichier1 , &n_ligne ) ;

/*   printf("... error ds variable floue file sur  ligne no: %d \n",n_ligne ); */

  affiche_erreur( errf , n_ligne, NULL );

/*   affiche_erreur( test_floue( fichier1 , &n_ligne ), n_ligne, NULL ); */

  fclose( fichier1 );
  fichier1 = fopen( nom, "r" );
  floues = lecture_nb_floues( fichier1 ,&err);    /* mise en place de la classe floue */
  fclose( fichier1 );
 
                   /****************************************/
                   /* lecture du nom du fichier des regles */
                   /****************************************/
  carac = lire( fichier );
  i = 0;
  while( carac != '\n' )
    {
      nom[i] = carac;
      i = i + 1;
      carac = lire( fichier );
    }
  nom[i] = '\0';
        
  fichier1 = fopen( nom, "r" );
  printf("... reading rule file %s \n",nom );
         /* regarde s'il y a une erreur dans ce fichier */
         /* et mise en place des piles                  */
  n_ligne = lecture_regle( fichier1, &pi ,floues , &regles , &err , &ch_err );

  /*  printf("... error ds  rule fileligne no: %d \n",n_ligne );*/

  /* 05-11-97 MOREL L. printf("Fin Fichier des Regles.\n"); ds la fonction lecture_regle */
  /* Debug impression du contenu de regle. 
  printf("\nDEBUG fichier.c aff regle:: %s\n",floues->regle);*/

  affiche_erreur(err,n_ligne,ch_err);
  fclose( fichier1 );
  post = inf_post(pi);
  
                   /*************************************/
                   /* lecture du mode de fonctionnement */
                   /*************************************/
  carac = lire( fichier );
  num = lireint(fichier,&carac,&err);
  if (num != 0 )           /* mode calib/valid -> saisie du type de validite */
   {
      validite = num ;
      num = 1;   
   }
  else 
    num = 2; 
	/* Ce qui precede est ridicule !! */


                   /***************************************/
                   /* lecture du nom du fichier de donnes */
                   /***************************************/
  carac = lireblanc_chgtligne( fichier, carac );
  carac = lire( fichier );
    i = 0;
  while( carac != '\n' )
    {
      nom[i] = carac;
      i = i + 1;
      carac = lire( fichier );
    }
  nom[i] = '\0';
  donnee = fopen( nom, "r" );
  if ( fichier1 == NULL )
    affiche_erreur(116,0,NULL); /* Impossible ouvrir fichier de donnees */
  else
    printf("... reading data file %s \n",nom );
                   /***************************************/
                   /* lecture du nom du fichier de sortie */
                   /***************************************/
  carac = lire( fichier );
    i = 0;
  while( carac != '\n' )
    {
      result[i] = carac;
      i = i + 1;
      carac = lire( fichier );
    }
  result[i] = '\0';
                   /*******************************************************/
                   /* lecture du nom du fichier intermediaire s'il existe */
                   /*******************************************************/
  carac = lire( fichier );
  if ( ( isalpha(carac) ) || ( carac == '/' ) )/* il y a un fic intermediaire */
   {   
     i = 0;
     while( carac != '\n' )
      {
        fic_int[i] = carac;
        i = i + 1;
        carac = lire( fichier );
      }
     fic_int[i] = '\0';
     fic_inter = 1;
   }
  else
   fic_inter = 0;                      /* il n'y en a pas */
                   /*************************************/
                   /* lecture de la combinaison desiree */
                   /*************************************/
  carac = lireblanc_chgtligne( fichier, carac );
  carac = lire( fichier );
  combi = lireint(fichier,&carac,&err);
                   /*****************************************/
                   /* lecture de la deflouification desiree */
                   /*****************************************/
  carac = lireblanc_chgtligne( fichier, carac );
  carac = lire( fichier );
  deflou = lireint(fichier,&carac,&err);
                   /***********************************/
                   /* lecture des operations desirees */
                   /***********************************/
  carac = lireblanc_chgtligne( fichier, carac );
  carac = lire( fichier );
  config[0] = lireint(fichier,&carac,&err);
  carac = lireblanc_chgtligne( fichier, carac );
  config[1] = lireint(fichier,&carac,&err);
  carac = lireblanc_chgtligne(fichier,carac);
  config[2] = lireint(fichier,&carac,&err);
  carac = lireblanc_chgtligne(fichier,carac);
  config[3] = lireint(fichier,&carac,&err);
            /*****************************************************************/
            /* lecture du titre de la courbe en sortie si elle est souhaitee */
            /*****************************************************************/
  carac = lireblanc_chgtligne(fichier,carac);
  carac = lire( fichier );
  res_model = lireint(fichier,&carac,&err);
  title_sortie = ( char * ) malloc( 50 * sizeof( char ) );
  carac = lireblanc_chgtligne( fichier, carac );
  if ( res_model == 1 )       /* elle est demandee -> saisie du nom */
    {
      i = 0;
      carac = lire( fichier );
      while( carac != '\n' )
       {
          title_sortie[i] = carac;
          i = i + 1;
          carac = lire( fichier );
        }
      title_sortie[i] = '\0';
    }
 
            /*************************************************************/
            /* lecture des variables a dessiner si elles sont souhaitees */
            /*************************************************************/
 while( ! feof( fichier ) )
    {
      carac = lire( fichier );
      if ( isdigit( carac ) )            /* le dessin de certaines variables */
        {                                /* est demandee                     */
          while( carac != '\n' )
            {
              varia = lireint( fichier, &carac, &err );
              dessin_varia_floue( floues->tab[varia-1] );
              carac = lirespace( fichier, carac );
            }
        }
    }

 /*DEBUG MODIF 14-05-97 
   printf("\nAppel de fonction Lecture_fic_donnees avec les parametres suivants :\n");
   
   printf("   regles nb : %i\n",regles->num);
   printf("   fic result: %s\n",result);
   printf("   fic interm: %s\n",fic_int);
   printf("\n   Classe floues nb : %i\n---------------------------\n",floues->nb);
   printf("   Classe floues nb var entree : %i\n",floues->nb_var_entree);
   printf("   Classe floues nb var sortie : %i\n",floues->nb_var_sortie);
   printf("   Classe floues nb var utilise : %i\n",floues->nb_var_utilise);
   printf("   Classe floues nb var reinjecter : %i\n",floues->nb_var_reinjecter);

   printf("   Classe floues regle : %s\n",floues->regle);
   printf("   Classe floues var sortie : %s\n\n------------------------------\n",floues->var_sortie);
   
   printf("   Donnee : ptr sur le fichier donnee -> ok\n");
   printf("   Pile Post:\n");
   AffichePile(pi);
   AffichePile(post);
   printf("   Config          : [%i][%i][%i][%i]\n",config[0],config[1],config[2],config[3]);
   printf("   Methode defloue : %i\n", deflou);
   printf("   Methode combi   : %i\n", combi);
   printf("   validite        : %i\n", validite);
   printf("   num previ/calib : %i\n", num);
   printf("   fic inter O/N   : %i\n", fic_inter);
   printf("   visu result O/N : %i\n", res_model);
   printf("   titre sortie    : %s\n", title_sortie);
   printf("	\n");
   printf("Appel lecture_fic_donnees\n\n"); */


/*   fprintf(stderr,"fichiers.c : avant lecture_fic_donnees\n");  */

 lecture_fic_donnees(&regles, result, fic_int, floues, donnee, post, config, deflou,combi, validite, num,fic_inter, res_model,title_sortie);

/*  fprintf(stderr,"fichiers.c : apres  lecture_fic_donnees\n"); */

}


/**************************************************************************/
/*************************** test fichier entree **************************/
/**************************************************************************/
erreur test_parametres( FILE * fichier,
			int  * nb_ligne)
{
  erreur err;
  char   carac;
  char * nom;
  int    i,j;
  FILE * fichier1;
  int    num;
  char   carac1;
  int    nb;
  int    varia;
  
                /***************************************************/
                /* regarde si le fichier des parametres est ouvert */ 
                /***************************************************/
  if ( fichier == NULL )
    return( 111 );
  (*nb_ligne) = 1;
  err = 0;
                   /****************************/
                   /* lecture des commentaires */
                   /****************************/ 
  carac = lire( fichier );
  while( carac == '#' )
    {
      while( carac != '\n' )
        carac = lire( fichier );
      (*nb_ligne) = (*nb_ligne) + 1;
      carac = lire( fichier );
    }
  
  nom = ( char * ) malloc( longfic*sizeof( char ) );
      
           /*******************************************************/
           /* test de l'existence du fichier des variables floues */
           /*******************************************************/
  i = 0;
  while( carac != '\n' )
    {
      nom[i] = carac;
      i = i + 1;
      carac = lire( fichier );
    }
  nom[i] = '\0';
  fichier1 = fopen( nom , "r" );
  if ( fichier1 == NULL )          /* probleme d'ouverture -> erreur */
    return( 100 );   
  else                             /* sinon recuperer le nombre de variables */
    {                              /* floues                                 */
      carac1 = lire( fichier1 );
      while( carac1 == '#' )
        {
          while( carac1 != '\n' )
            carac1 = lire( fichier1 );
          carac1 = lire( fichier1 );
        }
      carac1 = lireblanc( fichier1, carac1 );
      nb = lireint( fichier1, &carac1,&err );
    }
  fclose( fichier1 );
  (*nb_ligne ) = (*nb_ligne ) + 1;

        /*********************************************/
        /* test de l'existence du fichier des regles */
        /*********************************************/
  carac = lire( fichier );
  i = 0;
  while( carac != '\n' )
    {
      nom[i] = carac;
      i = i + 1;
      carac = lire( fichier );
    }
  nom[i] = '\0';
  fichier1 = fopen( nom , "r" );
  if ( fichier1 == NULL )           /* probleme d'ouverture ->erreur */
    return( 101 );
  fclose( fichier1 );
  (*nb_ligne ) = (*nb_ligne ) + 1;

       /****************************************************/
       /* test du type de calcul ( calib/valid ) ou normal */
       /****************************************************/
  carac = lire( fichier );
  num = lireint( fichier, &carac, &err );
  if ( err != 0 )                       /* probleme -> erreur */
    return( err );


/*  if ( ( num != 0 ) && ( num != 1 )&& ( num != 2 )&&( num != 3 )&& ( num != 4 ) ) /*   /* regarde si le chiffre est correct */

	/* ridicule.... l'ensemble des entiers est ordone que je sache */
	/* j'ajoute le format numero 5 */

	if (num<0 && num>5)

    return( 103 );

  carac = lireblanc_chgtligne( fichier , carac );
  carac = lire( fichier );
  (*nb_ligne ) = (*nb_ligne ) + 1;
           /**********************************************/
           /* test de l'existence du fichier des donnees */
           /**********************************************/
  i = 0;
  while( carac != '\n' )
    {
      nom[i] = carac;
      i = i + 1;
      carac = lire( fichier );
    }
  nom[i] = '\0';
  fichier1 = fopen( nom , "r" );
  if ( fichier1 == NULL )              /* probleme d'ouverture -> erreur */
    return( 104 );
  fclose( fichier1 );

  carac = lire( fichier );
  (*nb_ligne ) = (*nb_ligne ) + 1;

      
           /*********************************************/
           /* test si validite du fichier des resultats */
           /*********************************************/
  i = 0;
  while( carac != '\n' )
    {
      nom[i] = carac;
      i = i + 1;
      carac = lire( fichier );
    }
  nom[i] = '\0';
  fichier1 = fopen( nom , "w" );
  if ( fichier1 ==  NULL )            /* probleme d'ouverture -> erreur */
    return( 105 );
  fclose( fichier1 );

 

  carac = lire( fichier );
  (*nb_ligne ) = (*nb_ligne ) + 1;

           /*********************************************************/
           /* test de validite du fichier intermediaire s'il existe */
           /*********************************************************/
   while( carac != '\n' )
     carac = lire( fichier );
   (*nb_ligne ) = (*nb_ligne ) + 1;


                   /**********************************/
                   /* test de la combinaison desiree */
                   /**********************************/
  carac = lire( fichier );
  num = lireint(fichier,&carac,&err);
  if ( err != 0 )                     /* probleme -> erreur */
    return( err );
  if ( ( num < 1 )||( num > 5 ) )     /* regarde si le chiffre est coherent */ 
    return( 107 );
  carac = lireblanc_chgtligne( fichier, carac );
  carac = lire( fichier );
  (*nb_ligne ) = (*nb_ligne ) + 1;

  num = lireint(fichier,&carac,&err);
  if ( err != 0 )                     /* probleme -> erreur */
    return( err );
  if ( ( num < 1 )||( num > 3 ) )     /* regarde si le chiffre est coherent */
   return( 108 );
  carac = lireblanc_chgtligne( fichier, carac );
 (*nb_ligne ) = (*nb_ligne ) + 1;
  
  carac = lire( fichier );
  num = lireint( fichier, &carac, &err );
   if ( err != 0 )                   /* probleme -> erreur */
    return( err );
  if ( ( num < 0 )||( num > 1 ) )    /* regarde si le chiffre est coherent */   
    return( 109 );
  carac = lireblanc1( fichier, carac, nb_ligne );
  num = lireint( fichier, &carac, &err);
  if ( err != 0 )                    /* probleme -> erreur */
    return( err );
  if ( ( num < 0 )||( num > 1 ) )    /* regarde si le chiffre est coherent */
    return( 109 );
  carac = lireblanc1( fichier, carac, nb_ligne );
  num = lireint( fichier, &carac, &err );
  if ( err != 0 )                   /* probleme -> erreur */
    return( err );
  if ( ( num < 0 )||( num > 1 ) )   /* regarde si le chiffre est coherent */
    return( 109 );
  carac = lireblanc1( fichier, carac, nb_ligne );
  num = lireint( fichier, &carac, &err );
  if ( err != 0 )                   /* probleme -> erreur */
    return( err );
  if ( ( num < 0 )||( num > 1 ) )   /* regarde si le chiffre est coherent */
    return( 109 );

            /*****************************************************/
            /* test de la courbe en sortie si elle est souhaitee */
            /*****************************************************/
  carac = lireblanc_chgtligne( fichier, carac );
  (*nb_ligne ) = (*nb_ligne ) + 1;
  carac = lire( fichier );
  num = lireint( fichier, &carac, &err);
  if ( err != 0 )                      /* probleme -> erreur */
    return( err );
  if ( ( num < 0 )||( num > 1 ) )      /* regarde si le chiffre est coherent */
    return( 110 );
  if ( num == 1 )                      /* courbe desiree -> on lit le titre */
    { 
      carac = lireblanc_chgtligne( fichier, carac );
      (*nb_ligne ) = (*nb_ligne ) + 1;
      carac = lire( fichier );
      while( carac != '\n' )
        carac = lire( fichier );
    }
            /**********************************************************/
            /* test des variables a dessiner si elles sont souhaitees */
            /**********************************************************/
   while( ! feof( fichier ) )
    {
      carac = lire( fichier );
      if ( carac == '\n' )
        (*nb_ligne) = (*nb_ligne) + 1;
      if ( isdigit( carac ) )           /* il y en a */
        {
           while( carac != '\n' )
            {
              varia = lireint( fichier, &carac, &err );
              if ( err != 0 )             /* probleme -> erreur */
                return( err );
              if (( varia == 0 )||( varia > nb ) )
                return( 112 );          /* regarde si le chiffre est coherent */
              carac = lirespace( fichier, carac );
            }
        }
    }    

  free( nom );
  return ( err );
}

/**************************************************************************/
/********************* lecture du fichier des regles **********************/
/**************************************************************************/
int lecture_regle(FILE * fichier,
		  pile * pi,
		  classe_floue * classe,
		  list_regle ** regles,
		  erreur * err,
		  char ** ch_err)
{
  char carac;
  int i,j;
  int indice;
  int * ench_var;     /* tableau d'enchainement des variables dans la regle */
  int var_act;        /* variable actuelle dans ce tableau */
  char * nom;
  int nb_ligne;
  int test_validite_regle; /* 05-11-97 MOREL l. permet d'avoir une copie du nb 
			      de regle a verifier */
  int rep;
  list_regle * nouv;
  int num_regle;
  /* MODIF MOREL L. 24-11-97 rajoute une var */
  node * ptrnode; /* pour la recherche du ptr de la solution */
  int var_sortie; /* pr stocker le num de la var de sortie */
  
  /* Fin MODIF */
  (* err) = 0;
  (* ch_err)[0] = '\0';

                   /****************************/
                   /* lecture des commentaires */
                   /****************************/
  nb_ligne = 1;
  carac = lire(fichier);
  while( carac == '#' )
   {
     while( carac != '\n' )
       carac = lire(fichier);
     nb_ligne++;
     carac = lire(fichier);
   }

  carac = lireblanc1(fichier,carac,&nb_ligne);
  classe->regle = (char *) malloc (100*sizeof(char));
  if (classe->regle == NULL)     /* mauvaise allocation -> erreur */
   {
     (* err) = 7;
     return(0);
   }
  i = 0;
  nom = lirenom(fichier,&carac,err);
  if ( strcmp(nom,"IF") != 0 )
   {
     (* err) = 15;
     return(nb_ligne);
   }
  carac = lireblanc1(fichier,carac,&nb_ligne);
  if ( carac != '(' )
   {
     (* err) = 16;
     return(nb_ligne);
   }
  carac = lire(fichier);
  carac = lireblanc1(fichier,carac,&nb_ligne);


               /***********************/
               /* lecture de la regle */
               /***********************/

  /* lit la ligne de la regle et stock dans classe->regle[i] chaque mot de la */
  /* regle separe par un point                                                */
  while( carac != '\n' )
   {
     if ( (carac == ' ') || (carac == '	') )    /*le deuxieme est la tabulation */
       carac = '.';
     classe->regle[i] = carac;
     i++;
     carac = lire(fichier);
   }
  classe->regle[i] = '\0';  /* Termine la regle par \0 */
  
  /* DEBUG 
printf("\nDEBUG Fichier.c : Lec Regle : %s\n",classe->regle);*/

              /************************************/
              /* lecture de la variable de sortie */
              /************************************/
  /* lit le nom de la variable de sortie et ramene classe->regle a la fin */
  nom = lit_var_sortie(&classe->regle, i,&classe->nb_var_sortie); 
  classe->var_sortie = nom;
  
  /* DEBUG 14-05-98
     printf("DEBUG Fichier.c : aff sortie : %s\n",classe->var_sortie);
  */
             /*************************************************/
             /* constitution du tableau d'ordre des variables */
             /*************************************************/
  ench_var = (int *) malloc (100*sizeof(int));
  if (ench_var == NULL)  /* probleme d'allocation -> erreur */
   {
     (* err) = 20;
     return(0);
   }
  var_act = 0;
  /* MODIF 14-05-98
     printf("DEBUG :: avt transpile %s \n",classe->regle);
  */
  (* err) = trans_pile(pi,classe->regle,&var_act,ench_var,classe);
  
  affiche_erreur((* err),0,'\0');
  indice = 0;
  /* MODIF 14-05-98
     printf("DEBUG :: avt classement var sortie --%s--\n",classe->var_sortie);
  */
  nom = (char *) malloc (LONGMOT*sizeof(char));
  if (nom == NULL)
    {
      affiche_erreur(13,0,NULL);/* Erreur d'alloction de memoire pour le classement de la var de sortie */
    }
  /* Stocke le nombre de var en entree de 0 a var_act-1 */
  classe->nb_var_entree = var_act;
  classe->nb_var_reinjecter = 0;
  for (j=0;j<classe->nb_var_sortie;j++)
    { /* pour chaque var de sortie je recupere son nom et je le met ds enc_var.
       indice pointe sur le 1er caratere de la variable de sortie
      */
      rep = 0; /* ici rep est utilise comme indice */
      while (classe->var_sortie[indice] != '.') 
	{/* recupere le nom de la var sortie */
	  nom[rep] = classe->var_sortie[indice];
	  rep++;
	  indice++;
	}
      nom[rep] = '\0';
      /* maintenant qu'on a la var de sortie on va l'affecter a ench_var */
      /* MODIF 14-05-98
      printf("var sortie num %i -> --%s-- \n",j,nom);
      */
      i=0; rep=0;
      while ((i<classe->nb) && (rep==0))
	{/* recherche la var de sortie qui possede comme nom : nom */
	  if (strcmp(nom,classe->tab[i]->name) == 0)
	    rep=1;
	  else
	    i++; /* passe a la variable suivante */
	}
      if (rep==1)
	{
	  /* MODIF 14-05-98
	     printf("var sortie num %i -> --%s-- %i\n",j,nom,i);
	  */
	  var_sortie = i;/* i contient le num correspondant a la var de sortie */
	  ench_var[var_act] = i;
	  var_act++;
	  /* MODIF 2-12-97 MOREL L. Code pour verifier si cette variable i est deja 
	     reinjecter ou pas */
	  /* Test de reinjection */
	  if (classe->tab[var_sortie]->reinjecter == 1)
	    {
	      affiche_erreur(117,0,nom);
	      /* !!! Erreur sur variable de sortie deja reinjectee */
	      /* impossible d'avoir une meme variable reinjectee plus d'une fois */
	    }
	  else /* ps encore reinjecter on cherche si elle est reinjecter */
	    {
	      i = 0; rep = 0;
	      /* var_act represente le nombre de var utilise - classe->nb_var_sortie = 
		 nb de var en entree 
	      */
	      /* MODIF 14-05-98
	      printf("Parcours ds ench_var de %i a %i ",i,classe->nb_var_entree  );
	      */
		while ((i < classe->nb_var_entree ) && (rep==0))
		  {
		    if (ench_var[i] == var_sortie )
		      { /* c' une var d'entree reinjecte */
			rep=1;
		      }
		    else
		      i++;
		  }
		if ( rep == 1 )
		  {
		    /* MODIF 14-05-98
		       printf("la var %s est une entree reinjecter \n\n",nom);
		       */
		    classe->tab[var_sortie]->reinjecter = 1;
		    classe->nb_var_reinjecter++;
		  }
		/* MODIF 14-05-98
		   else
		   printf("la var %s n'est pas une entree reinjecter\n\n",nom);
		*/
	      }
	  /* Fin MODIF */
	}
      else
	{
	  affiche_erreur(118,0,nom);
/* Erreur lors de la lecture de la regle pas de definition variable de sortie */
	}
	  
      
      while (classe->var_sortie[indice] == '.')
	indice++;
    }
  free ( nom );
  /* il y a une variable qui est compte en plus */
  var_act--;

  /* Modif 21-11-97 MOREL L. mise en place du nombre de variable de sortie et
     du nombre de variable utilise dans la regle */
  
  classe->nb_var_utilise = var_act;
  
  /* DEBUG */
/* MODIF 14-05-98
   printf("DEBUG :: Fichier.c : ench_var : Nb var %i\n",var_act);
   printf("nb var sortie %i, nbvar utilise %i \n",classe->nb_var_sortie ,
   classe->nb_var_utilise );
	 */
  /* MODIF 02-12-97 Affectation de ench_var */
  classe->ench_var = ench_var;
/* MODIF 14-05-98
   for(i=0; i<=var_act; i++)
   printf("Var %i, ",classe->ench_var[i]);  
   
   printf("\n");
*/
/*affiche(pi);*/

              /********************************/
              /* lecture de toutes les regles */
              /********************************/

  num_regle = 1;
  carac = lireblanc1(fichier,carac,&nb_ligne);
  test_validite_regle = var_act;
  (* regles) = (list_regle *) malloc (sizeof(list_regle));
  if ((* regles) == NULL)
   {
     (* err) = 19;
     return(0);
   }
  (* regles)->suiv = NULL;        /* allocation pour la premiere regle */

  /* while (carac != '-') MODIF 19-01-97 MOREL L. le teste de la 
	       fin du fichier ne se fait plus par -1, mais par la fonction
	      qui permet de savoir si on est a la fin du fichier.*/ 
while( ! feof( fichier ) )  
   {
     var_act = 0;
     /* Modif 21-11-97 MOREL L. l'allocation suivante se fait en fonction de
	classe->nb je le modifie pour le faire en fonction de var_act qui est 
	stocker ds test_validite_regle 
	(* regles)->reg_act = (info *) malloc (classe->nb*sizeof(info));
     */
     (* regles)->reg_act = (info *) malloc (test_validite_regle*sizeof(info));
     /* Modif 03-11-97 Morel L. */
     /* le teste SUIVANT ne correspond pas avec l'allocation qui se fait juste au dessus*/
     /* Remplace if (classe->regle == NULL) par if ((* regles)->reg_act == NULL)	*/
     /* probleme d'allocation -> erreur							*/
     if ((* regles)->reg_act == NULL)
      {
        (* err) = 21;
        return(0);
      }
     /*
       printf("DEBUG :: Regle Num : %i\n",num_regle);
     */
     while (test_validite_regle >= var_act) 
       /* 5-11-97 MOREL L. on parcours le fichier des regles  */
       /* et pour chaque var floue definissant la regle on    */
       /* verifie s'il le nom existe dans ls variable foues   */
      {					    
					    
        nom = lirenom(fichier,&carac,err); /* defini ds la classe floues   */ 
        rep = verifie_var(nom,&var_act,ench_var,classe);
	/* verification de nom avec la classe flou */ 
        if (rep == 1)
         {
           (* err) = 17;
           strcpy((* ch_err),nom);
           return(nb_ligne);
         }

        /* numero de la variable = ench_var[var_act] */
	/* Modif 21-11-97 MOREL L. 
	   Ancien Code :

	     strcpy((* regles)->reg_act[ench_var[var_act]].name,nom);
	     (* regles)->reg_act[ench_var[var_act]].valeur = 0.0;

	   Ce code est remplace par les deux lignes ci dessous :
	   L'ench_var contient l'enchainement des variables ds la regle, mais pour
	   la mise en place en memoire des regles il suffit de lire le fichier et
	   de saisir variable par variable sans se preocuper de l'enchainement 
	   puisque celui est bon dans le fichier des regles (normalement)


	   place la variable actuel	*/

	strcpy((* regles)->reg_act[var_act].name,nom);  
	(* regles)->reg_act[var_act].valeur = 0.0;





	/* Modif MOREL L. 24-11-97 j'ai rajoute deux autres variables a la 
	   structure info qui me permet d'avoir le num de la variable de sortie 
	   correspondant a i pour tab[i] dans la classe_floues et le ptr sur une
	   structure node qui est celui de la var qu'on lit actuellement dans le 
	   fichier regle.
	   
	   Ce qui permet d'accelerer le calcul des eval_bool eval_dof pour la suite.
	   */

	i=0; rep=0;
	while ((i<classe->nb) && (rep==0))
	  {/* recherche la var de sortie qui possede comme nombre flou : nom celui
	    qu'on vient de lire ds le fichiers des regles.*/
	    ptrnode = classe->tab[i]->liste_elem;
	    while ((ptrnode != NULL) && (rep==0) )
	      { /* parcours les nombre floues de la variable i */
		if (strcmp(nom,ptrnode->nombre.name)==0)
		  rep=1;
		else
		  ptrnode = ptrnode->suiv;
	      }
	    if ( rep==0 ) i++; /* passe a la variable suivante */
	  }
	if (rep==1)
	  {
	    (* regles)->reg_act[var_act].num_var_sortie = i;
	    (* regles)->reg_act[var_act].var_sortie = ptrnode;
	    /*
	      printf("Trouve ds classe la regle : %s == %s %i \n",
	      (* regles)->reg_act[var_act].name,
	      (* regles)->reg_act[var_act].var_sortie->nombre.name,
	      i);
	    */
	  }
	else
	  {
	    affiche_erreur(25,0,(* regles)->reg_act[var_act].name);
	    /* Erreur sur lecture des regles definition variable flou introuvable */

	  }
	/* Fin MODIF */

	/*
	  printf("DEBUG ::	regle %i : %s\n",var_act,nom);
	*/
        carac = lireblanc_chgtligne(fichier,carac);
        var_act++;
      }
    if(carac != '\n')   /* a voir si il y a des espaces apres le dernier mot */
     {
       (* err) = 18;
       return(nb_ligne);
     }
    carac = lireblanc1(fichier,carac,&nb_ligne);
    (* regles)->num = num_regle;
    num_regle++;
    /* if (carac != '-') MODIF 19-01-97 MOREL L. le teste de la 
	       fin du fichier ne se fait plus par -1, mais par la fonction
	      qui permet de savoir si on est a la fin du fichier.*/
    if (! feof( fichier ))
     {
      nouv = (list_regle *) malloc (sizeof(list_regle));
      if (nouv == NULL)
       {
         (* err) = 19;
         return(0);
       }
      nouv->suiv = (* regles);
      (* regles) = nouv;
     }
   }

  /* MODIF 14-05-97  
     printf("\n\nAffichage des regles :\n");
     AfficheRegles( ( * regles) , test_validite_regle);
  */
  
  return(0);
}

  
/**************************************************************************/
/****************** ecriture du fichier intermediaire *********************/
/**************************************************************************/
void ecrit_fic_inter(int *fic_interm,
		     char * sortie,
		     double dof,
		     info * reg_act,
		     classe_floue * classe)
{
  char * nom;
  int i;
  node * pteur;
  int rep;
  double val;

  nom = (char *) malloc (50*sizeof(char));
  /* MODIF 27-11-97 MOREL L.
     pour prendre en cpte le nombre exacte de variable d'entree
     Ancien code :
     for(i=0;i<=classe->nb;i++)
     */
  for(i=0;i<classe->nb_var_entree;i++)
   {

     /* MODIF 23-12-97 MOREL L
	pour prendre en compte le num de var et nom le i qui pose pb qud les 
	var ne sont pas definie ds le meme ordre aue le fichier des regles
	
	pteur = classe->tab[i]->liste_elem;
     */
       
     pteur = classe->tab[reg_act[i].num_var_sortie]->liste_elem;
     rep = 0;
     while ((pteur != NULL) && (rep == 0))
      {
        if (strcmp(reg_act[i].name,pteur->nombre.name) == 0)
         {
           rep = 1;
           switch(pteur->nombre.type)
            {
              case 0:val = appartient_triangle(&pteur->nombre,reg_act[i].valeur);
                     break;
              case 1:val = appartient_trapeze(&pteur->nombre,reg_act[i].valeur);
                     break;
              case 2:val = appartient_LR(&pteur->nombre,reg_act[i].valeur);
                     break;
	      case 3 :val= appartient_LRG(&pteur->nombre,reg_act[i].valeur);
		     break;
            }
         }
        pteur = pteur->suiv;
      }
     if (rep == 1)
       {
	 sprintf(nom,"    %5.5lf %s",val,reg_act[i].name);
	 write(fic_interm,nom,strlen(nom));
       }
   }
  sprintf(nom,"    %5.5lf %s\n",dof,sortie);
  write(fic_interm,nom,strlen(nom));
  free(nom);  /* MODIF MOREL L. la desalocation du nom n'etait pas faite*/
}



/**************************************************************************/
/********************* lecture du fichier des donnees *********************/
/**************************************************************************/
void lecture_fic_donnees(list_regle  ** regles,
			 char         * result,
			 char         * fic_int,
			 classe_floue * floues,
			 FILE         * donnee,
			 pile           post,
			 int            config[4],
			 int            deflou,
			 int            combi,
			 int            validite,
			 int            num,
			 int            fic_inter,
			 int            visu_result,
			 char         * title_sortie)
{
  char * nom;
  int i;
  double sortie;
  double obs;
  int fic_sortie;
  int fic_interm;
  list_regle * pteur;
  char carac;
  double val;
  sol * solution;
  int visu;
  nash_ * nash;
  nash_ * nouv;
  double ind_nash;
  int num_uplet;
  int err;
  int ligne;
  /* MODIF MOREL L. 28-11-97 des variables en plus pour prendre en compte
     plusieurs var de sortie */
  int var_debut, var_fin, num_var_sortie, j,ok;
  /* MODIF 19-12-97 MOREL L.
     ces variables me permette de faire une lecture differente pour le mode
     toutes var reinjecter */
  int tout_reinjecter, entete_lu, nb_iteration_faire, nb_iteration_fait;
  
  double sortieold;
  discret  tab_dis;

  ligne = 0;

  nom = (char *) malloc (100*sizeof(char));
  nash = NULL;
  fic_sortie = creat(result,MODE);
  if (fic_inter == 1)
     fic_interm = creat(fic_int,MODE);


if (validite!=5) {
	/* Ajout pour enlever toutes les "saletes" de MODFLOU */
  switch(combi)
   {
     case 1:sprintf(nom,"# Agregation by minimum \n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom));
            break;
     case 2:sprintf(nom,"# Agregation by minimum with crest \n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom));
            break;
     case 3:sprintf(nom,"# Agregation by maximum \n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom)); 
            break;
     case 4:sprintf(nom,"# Agregation by maximum with crest \n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom));
            break;
     case 5:sprintf(nom,"# Agregation by weihted sum \n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom));
            break;
   }
  switch(deflou)
   {
     case 1:sprintf(nom,"# Defuzzification by maximum \n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom));
            break;
     case 2:sprintf(nom,"# Defuzzification by center of gravity \n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom));
            break;
     case 3:sprintf(nom,"# Defuzzification by median \n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom));
            break;
   }


}

  switch(num)
   {
     case 1:
		if (validite!=5) {
			sprintf(nom,"# calibration/validation mode\n");
			write(fic_sortie,nom,strlen(nom));
         	   if (fic_inter == 1)
        	      write(fic_interm,nom,strlen(nom));
		}
            break;
     case 2:sprintf(nom,"# prevision mode\n");
            write(fic_sortie,nom,strlen(nom));
            if (fic_inter == 1)
              write(fic_interm,nom,strlen(nom));
            break;
   }
/*****************************************************************************************/
/* Modif le 15/10/97 MOREL Luciano : ecriture de la modelisation des operateurs logiques */
/* Ajout des ecritures ds ls fichiers						    	 */
/*****************************************************************************************/

/* Operateur NOT */
if (config[0]==0) 
	{
	  sprintf(nom,"#NOT : Min, ");
	  write(fic_sortie,nom,strlen(nom));
          if (fic_inter == 1)
          	write(fic_interm,nom,strlen(nom));
	}
	else
	{
	  sprintf(nom,"#NOT : Prod, ");
	  write(fic_sortie,nom,strlen(nom));
          if (fic_inter == 1)
          	write(fic_interm,nom,strlen(nom));
	}

/* Operateur OR */
if (config[1]==0) 
	{
	  sprintf(nom,"OR : Min, ");
	  write(fic_sortie,nom,strlen(nom));
          if (fic_inter == 1)
          	write(fic_interm,nom,strlen(nom));
	}
	else
	{
	  sprintf(nom,"OR : Prod, ");
	  write(fic_sortie,nom,strlen(nom));
          if (fic_inter == 1)
          	write(fic_interm,nom,strlen(nom));
	}

/* Operateur AND */
if (config[2]==0) 
	{
	  sprintf(nom,"AND : Min, ");
	  write(fic_sortie,nom,strlen(nom));
          if (fic_inter == 1)
          	write(fic_interm,nom,strlen(nom));
	}
	else
	{
	  sprintf(nom,"AND : Prod, ");
	  write(fic_sortie,nom,strlen(nom));
          if (fic_inter == 1)
          	write(fic_interm,nom,strlen(nom));
	}

/* Operateur XOR */
if (config[3]==0) 
	{
	  sprintf(nom,"XOR : Min \n");
	  write(fic_sortie,nom,strlen(nom));
          if (fic_inter == 1)
          	write(fic_interm,nom,strlen(nom));
	}
	else
	{
	  sprintf(nom,"XOR : Prod \n");
	  write(fic_sortie,nom,strlen(nom));
          if (fic_inter == 1)
          	write(fic_interm,nom,strlen(nom));
	}

/*******************************************************/
/* Fin de modif pour les operateurs logique : MOREL L. */
/*******************************************************/

    /* lecture des commentaires */

  carac = lire(donnee);
  while( carac == '#' )
   {
     while( carac != '\n' )
       carac = lire(donnee);
     ligne = ligne + 1;
     carac = lire(donnee);
   }

    /* lecture des valeurs et execution lignes par lignes */

  carac = lireblanc1( donnee, carac, &ligne );
  num_uplet = 0;
  /* MODIF 1-12-97 MOREL L. Rajout ds le fichier de sortie les 
     infos sur ls valeurs en sorties */

  if (num==1)/* Mode calib */
    {
    /* sprintf(nom,"#Obs	Var de sorties : %s\n",floues->var_sortie);
       sprintf(nom,"#Obs	"); 
       On ne fait rien

    */
      sprintf(nom,"#");
      
    }
  else/* Mode previ */
    {
      /* sprintf(nom,"#uplet	Var de sorties : %s\n",floues->var_sortie);  */
      sprintf(nom,"#uplet	");
    } 
    
  write(fic_sortie,nom,strlen(nom));
  /* parcours des variables de sortie et les inscrits un par un ds le fichier
     de sortie
  */
  var_debut = floues->nb_var_entree;
  var_fin = floues->nb_var_utilise;
  
  for (i=var_debut;i<=var_fin;i++)
    {
      num_var_sortie = floues->ench_var[i];
      if (num==1)/* Mode calib */
	sprintf(nom,"%s_Obs    %s    ",floues->tab[num_var_sortie]->name,
		floues->tab[num_var_sortie]->name);
      else/* Mode previ */
	sprintf(nom,"%s	",floues->tab[num_var_sortie]->name);
      write(fic_sortie,nom,strlen(nom));
    }
  write(fic_sortie,"\n",strlen("\n"));   

  ok = 1;
  entete_lu = 0;
  tout_reinjecter = (floues->nb_var_entree == floues->nb_var_reinjecter ); 
  nb_iteration_fait = 1;
  /* DEBUG MODIF 14-05-98
     printf("Toutes les variables sont reinjectees %i\n",tout_reinjecter);
  */
  /* while ((carac != '-') && (ok == 1 ))MODIF 19-01-97 MOREL L. le test de la 
	       fin du fichier ne se fait plus par -1, mais par la fonction
	      qui permet de savoir si on est a la fin du fichier.*/
while ((! feof( donnee )) && (ok == 1 ))
   { /* while lecture des donnees ligne par ligne */
     num_uplet++;
     /* MODIF 14-05-97 Affichage du upplet i
	printf("\nuplet %i\n",num_uplet);
     */
     if (fic_inter == 1)
       {
         sprintf(nom,"uplet %d:",num_uplet);
         write(fic_interm,nom,strlen(nom));
       }
     /* MODIF 14-05-97
	printf("DEBUG : val lu : ");


	MODIF 21-11-97 Lecture des donnees en fonction du nombre de variable
	d'entree et non plus en fonction du nombre de var declare
	Ancien code :
	for(i=0;i<floues->nb-1;i++)
     
	printf("Lecture des donnees et affectation %i \n",floues->nb_var_utilise );
     
	
	MODIF 18-12-97 MOREL L.
	Mise en place de la lecture lorsque tout est reinjecter 
	1 ere ligne c' les donnees 
	2 eme ligne c' le nb_iteration_faire
	*/
     if (entete_lu == 0) { /* on n'est ps en mode tout reinjecte + prevision */
       for(i=0;i<floues->nb_var_entree;i++)
	   {
	     
	     val = lirenb(donnee,&carac,&err);
	     /* MODIF 02-12-97 MOREL L mise en place de la reinjection de
		la valeur calculer si necessaire */
	     num_var_sortie = floues->ench_var[i];

	     if ( num_uplet == 1 ) 
	       /* 1er ligne lecture de la valeur et initialisation 
		  de la valeur de sortie et la valeur de sortieOld 
		  = a la valeur lue */
	       {
		 floues->tab[num_var_sortie]->val_sortie = val;
		 floues->tab[num_var_sortie]->val_sortieOld = val;
		 /* MODIF 14-05-97 Affichage du upplet i 
		    printf(" Var %i : %5.5lf-O %5.5lf",i , 
		    floues->tab[num_var_sortie]->val_sortieOld,
		    floues->tab[num_var_sortie]->val_sortie );
		 */
	       }
	     else 
	       {/* test si cette var est reinjectee 
		   num_var_sortie = floues->ench_var[i];*/
		 if (floues->tab[num_var_sortie]->reinjecter == 1 )
		   { /* cette var est reinjecter */
		     val = floues->tab[num_var_sortie]->val_sortie;

		     /* MODIF 14-05-97 Affichage du upplet i
			printf(" Var R%i : %5.5lf",i , val);
		     */
		   }
		 /* MODIF 14-05-97 Affichage du upplet i
		    else
		    printf(" Var N%i : %5.5lf",i , val);
		 */
		 
	       }
	     
	     if (fic_inter == 1)
	       {
		 sprintf(nom," %5.5lf",val);
		 write(fic_interm,nom,strlen(nom));
	       }
	     pteur = (* regles);
	     while(pteur != NULL)
	       {
		 pteur->reg_act[i].valeur = val;
		 pteur = pteur->suiv;
	       }
	     carac = lireblanc1( donnee, carac, &ligne );
	   }
	 /* MODIF 19-12-97 MOREL L. 
	    lecture des valeurs obs pour chaque vr de sortie 
	    cette lecture est effectuer que ds le mode de calibration
	    sinon dans le mode de prevision elle est a zero
	    
	 */
       var_debut = floues->nb_var_entree;
       var_fin = floues->nb_var_utilise;
       
       for (j=var_debut;j<=var_fin;j++)
	 {
	   if (num==1)/* Mode calib */
	     obs = lirenb(donnee,&carac,&err);
	   else
	     obs = 0.0;
	   
	   num_var_sortie = floues->ench_var[j];
	   floues->tab[num_var_sortie]->val_obs = obs;
	   
	   if (num==1) /* Mode calib */
	     {
	       carac = lireblanc1( donnee, carac, &ligne );
	       if (fic_inter == 1)
		 {
		   sprintf(nom," %5.5lf",obs);
		   write(fic_interm,nom,strlen(nom));
		 }
	       /* MODIF 14-05-97 Affichage du upplet i
		  printf(" obs %5.5lf ",floues->tab[num_var_sortie]->val_obs);
	       */
	     }
	 }

	 if (fic_inter == 1)
	   {
	     write(fic_interm,"\n",strlen("\n"));
	   }
	 visu = lireint(donnee,&carac,&err);

	 /* MODIF 14-05-97 Affichage du upplet i
	    printf(" visu %i",visu);
	 */
	 carac = lireblanc1( donnee, carac, &ligne );
	 /* MODIF 14-05-97 Affichage du upplet i
	    printf("\n");
	 */
	 
	 if ( (tout_reinjecter == 1 ) && (num == 2) ) /* Mode Previ */
	   { /* le mode de lecture des fichiers de donnees passe en
		tout reinjecte que si on est en mode previ car dans
		ce mode on ne lit pas les vals observees
		*/
	     
	     nb_iteration_faire = lireint(donnee,&carac,&err);
	     printf("!!! Toutes les variables reinjectees nb iteration : %i\n", nb_iteration_faire);
	     entete_lu = 1;
	   }
	   
       }
     else {/* toute les var sont reinjecter */

       nb_iteration_fait++;
       if ( nb_iteration_fait == nb_iteration_faire )
	 ok = 0;

       for(i=0;i<floues->nb_var_entree;i++)
	   {
	     num_var_sortie = floues->ench_var[i];
	     /* MODIF 14-05-97 Affichage du upplet i
		printf(" Var %i : %5.5lf-O %5.5lf",i , 
		floues->tab[num_var_sortie]->val_sortieOld,
		floues->tab[num_var_sortie]->val_sortie );
	     */

	     val = floues->tab[num_var_sortie]->val_sortie;

	     /* MODIF 14-05-97 Affichage du upplet i
		printf(" Var R%i : %5.5lf",i , val);
	     */
	     
	     if (fic_inter == 1)
	       {
		 sprintf(nom," %5.5lf",val);
		 write(fic_interm,nom,strlen(nom));
	       }
	     
	     pteur = (* regles);
	     while(pteur != NULL)
	       {
		 pteur->reg_act[i].valeur = val;
		 pteur = pteur->suiv;
	       }

	   }
       /* il n'y a pas de valeur obs si mode previ 	  */
       if (num==1) /* Mode calib */
	 {
	   var_debut = floues->nb_var_entree;
	   var_fin = floues->nb_var_utilise;
	   
	   for (j=var_debut;j<=var_fin;j++)
	     {
	       num_var_sortie = floues->ench_var[j];
	       /* MODIF 14-05-97 Affichage du upplet i
		  printf(" obs %5.5lf ",floues->tab[num_var_sortie]->val_obs);
	       */
	       if (fic_inter == 1)
		 {
		   sprintf(nom," %5.5lf",floues->tab[num_var_sortie]->val_obs);
		   write(fic_interm,nom,strlen(nom));
		 }
	     }
	 }
       if (fic_inter == 1)
	 {
	   write(fic_interm,"\n",strlen("\n"));
	 }
       /* MODIF 14-05-97 Affichage du upplet i 
	  printf(" visu %i",visu);
	  printf("\n");
	*/
	 
     }
	 
     solution = test_regles(post,(* regles),config,floues,fic_inter,fic_interm);
     if ( solution == NULL )
       {
	 if (validite==5) 
	   {
	     write(fic_sortie,"0\n",2);
	   }
	 else 
	   {
	     sprintf(nom,"# uplet num %d : Aucune regle  utilisee\n",num_uplet);
	     write(fic_sortie,nom,strlen(nom));
	   }
       }
     else
       {
	 /* MODIF MOREL L. 28-11-97 Mise en place de la deflouification 
	    pour plusieurs variables de sortie */
	 var_debut = floues->nb_var_entree;
	 var_fin = floues->nb_var_utilise;
	 /* MODIF 14-05-97 Affichage du upplet i
	    printf("Debut %i fin %i Var de sortie Num ->",var_debut,var_fin);
	    */
	 for (i=var_debut;i<=var_fin;i++)
	   {
	     /* deflouification en fonction de la sortie */     
	     sortie = combi_deflou(solution,floues,deflou,combi,visu,i,&tab_dis);
	     LibereDiscret(tab_dis);
	     /* MODIF  02-12-97 MOREL L. ench_var[i] est le num de la variable 
		de sortie ds l'ensemble classe floue. */
	     num_var_sortie = floues->ench_var[i];
	     sortieold = floues->tab[num_var_sortie]->val_sortie;
	     floues->tab[num_var_sortie]->val_sortieOld = sortieold;
	     floues->tab[num_var_sortie]->val_sortie = sortie;

	     /* MODIF 14-05-97 Affichage du upplet i
		printf(" %i : %5.5lf-O %5.5lf",i,
		floues->tab[num_var_sortie]->val_sortieOld,
		floues->tab[num_var_sortie]->val_sortie);
		
		MODIF 09/01/98 MOREL L.
		si la variable pour laquelle on va ecrire le resultat de la
		deflouification ds le fichier de sortie est une var reinjecter
		selon le mode calib ou previ ecrire respectivement :
		la val obs lu, la valeur precedement calcule ou lu. cette valeur
		est contenu ds pteur->reg_act[i].valeur i correspondant au num
		de var par rapport au tableau floues->ench_var[i]; C'est le num
		par rapport a la regle et non pas par rapport a la variable

		     if ( floues->tab[num_var_sortie]->reinjecter == 1 )
		       sortie = pteur->reg_act[i].valeur;
		*/

	     switch(num)
	       {
	       case 1:/* Mode calib */
		 if (validite==5) 
		   {

		     sprintf(nom,"%5.5lf  ",sortie);
		     write(fic_sortie,nom,strlen(nom));
		   } 
		 else 
		   {

		     sprintf(nom,"%5.5lf    %5.5lf    ",
			     floues->tab[num_var_sortie]->val_obs,
			     sortie);
		     write(fic_sortie,nom,strlen(nom));
		   }
		 break;
	       case 2:/* Mode previ  */
		 /* Modif 7/10/97 MOREL L. impression dans le fichier de sortie 
		    le numero et le  resultat */
		 
		 if ( floues->tab[num_var_sortie]->reinjecter == 1 )
		   sprintf(nom,"%d       %5.5lf",num_uplet, sortieold);
		 else
		   sprintf(nom,"%d       %5.5lf",num_uplet,sortie);
		 
		 write(fic_sortie,nom,strlen(nom));
		 break;
	       }
	     
	     /* Modif 06-11-97 MOREL L. Ici la mise en place de la liste 
		des resultats et des val observees ne sont pas necessaire 
		dans le cas ou on ne la demande pas 
	     */
	     /* MODIF 02-12-97 Mise en place du code pour le calcul de nash
		pour chq var de sortie 
		Ancien code :
		if ( ( num == 1 ) || ( visu_result == 1 ) )
		{
		nouv = (nash_ *) malloc (sizeof(nash_));
		nouv->suiv = nash;
		nouv->val_obs = obs;
		nouv->val_calc = sortie;
		nash = nouv;
		}
	     */
	     if ( ( num == 1 ) || ( visu_result == 1 ) )
	       {
		 nouv = (nash_ *) malloc (sizeof(nash_));
		 nouv->suiv = floues->tab[num_var_sortie]->nash;
		 if ( num == 1 ) /* Mode calib */
		   nouv->val_obs = floues->tab[num_var_sortie]->val_obs;
		 else /* Mode previ */
		   nouv->val_obs = num_uplet;

		 nouv->val_calc = sortie;
		 floues->tab[num_var_sortie]->nash = nouv; 
	       }
	     /* Fin MODIF */


	   }
	 write(fic_sortie,"\n",strlen("\n"));
	 /*printf("\n");
	   modif 06-11-97 Mode Calibration */
         /* free( solution ); Modif 06-11-97 Remplace la liberation d'une case par 
	    une fonction qui libere */
	 /* toutes les cases du programmes
	  */
	 
	 /* il y a autant d'appel de liberation qu'il y a de solution itermediaire ds le fichier 
	    intermediaire */
	 /* tout a fait normal */
	 /*printf("DEBUG : fichiers.c lecture_fic_donnees Libere solution \n");*/
	  LibereSolution(solution);

       } /* MOREL L Fin du else de Solution */
   } /* MOREL L Fin du while pour la lecture du fichier de donnee */

/* MODIF 02-12-97 Mise en place du code pour l'affichage du calcul de nash
   pour chq var de sortie   */
  if ( ( num == 1 ) || ( visu_result == 1 ) )
    { 
      var_debut = floues->nb_var_entree;
      var_fin = floues->nb_var_utilise;
      /* MODIF 14-05-97 Affichage du upplet i
	 printf("DEBUG :: Debut %i fin %i Calcul de nash\n",var_debut,var_fin);
      */
      for (i=var_debut;i<=var_fin;i++)
	{  
	  num_var_sortie = floues->ench_var[i];
	  if (num == 1)
	    switch(validite)
	      {
	      case 1:ind_nash = s_carre_ecarts(floues->tab[num_var_sortie]->nash);
		/* MODIF 14-05-98 
		   Validite par la somme des carres des ecarts de %s : %lf */
		   
		sprintf(nom,"\n# Sum of squared errors of %s : %lf\n",
			floues->tab[num_var_sortie]->name, ind_nash);
		write(fic_sortie,nom,strlen(nom));
		printf("%s",nom);
		break;
	      case 2:ind_nash = m_carre_ecarts(floues->tab[num_var_sortie]->nash);
		sprintf(nom,"\n# Root mean squared error of %s : %lf\n",
			floues->tab[num_var_sortie]->name, ind_nash );
		write(fic_sortie,nom,strlen(nom));
		printf("%s",nom);
		break;
	      case 3:ind_nash = m_norm_carre_ecarts(floues->tab[num_var_sortie]->nash);
		sprintf(nom,"\n# Normalized average of squared errors of %s : %lf\n",
			floues->tab[num_var_sortie]->name, ind_nash);
		write(fic_sortie,nom,strlen(nom));
		printf("%s",nom);
		break;
	      case 4:ind_nash = ind_correl(floues->tab[num_var_sortie]->nash);
		sprintf(nom,"\n# Correlation coefficient of %s : %lf\n",
			floues->tab[num_var_sortie]->name, ind_nash);
		write(fic_sortie,nom,strlen(nom));
		printf("%s",nom);
		break;
	      }
	  
	  if ( visu_result == 1 )
	    {
	      sprintf(nom,"%s",floues->tab[num_var_sortie]->name);
/*	      sprintf(nom,"%s -- Affichage pour %s --",title_sortie,floues->tab[num_var_sortie]->name);*/
/* MODIF 14-05-97 
   printf("Demande d'affichage pour %s \n",floues->tab[num_var_sortie]->name);
*/

/*   fprintf(stderr,"fichier.c : proc lecture_fic ... , avant dessin_sortie \n"); */

	      dessin_sortie(floues->tab[num_var_sortie]->nash, 
			    floues->tab[ floues->nb-1 ]->name, nom);
			    /* title_sortie ); remplqcer pr nom*/

/*   fprintf(stderr,"fichier.c : proc lecture_fic ... , apres dessin_sortie\n"); */


	    }
	}
    }
  /* desallocation final */

/*   fprintf(stderr,"fichier.c : proc lecture_fic ... , avant free(nom)\n"); */

  free(nom);  /* 11/97 MOREL L. Libere la var nom */

/*   fprintf(stderr,"fichier.c : proc lecture_fic ... , apres free(nom)\n"); */


}

