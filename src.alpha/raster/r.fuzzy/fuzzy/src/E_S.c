#define MOD E_S

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "E_S.h"
#include "erreur.h"
#include "definitions.h"

/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/



/**************************************************************************/
/*************************** lire dans un fichier *************************/
/**************************************************************************/
char lire(FILE * fichier)
{

 return(fgetc( fichier ));

}

/*************************************************************************/
/*************************** lire un reel ********************************/
/*************************************************************************/
double lirenb( FILE   * fichier,
	       char   * carac,
	       erreur * err)
{
  /* MODIF 08-12-98 RORET Sebastien
     La procedure etait inutilement complexe, j'ai tout enleve */
  double d;
  /* On considere qu'un caractere du fichier a deja ete lu !!! 
     On revient d'1 char */
  fseek(fichier, -1, SEEK_CUR);
  if (fscanf(fichier, "%lf", &d) == 0) 
    {
      *err=5;
      return 0;
    }
  else {
    /* memes conditions qu'en entree - carac contient le prochain char*/
    *carac=(char)fgetc(fichier);
    return d;
  }
}      


/*************************************************************************/
/*************************** lire un entier ******************************/
/*************************************************************************/
int lireint(FILE   * fichier,
	    char   * carac,
	    erreur * err)
{
  int chaine[20];
  int i;
  int result;
  int j;

  i = 0;
  j = 1;
  result = 0;

  if ( isdigit( (*carac) ) == 0 )  /* le 1er caractere est-il un chiffre ? */
    {
      (*err) = 5;                  /* non -> erreur */
      return(0);
    } 
  while( isdigit( (*carac) ) )     /* oui -> mets tous les chiffres dans une */
    {                              /* chaine                                 */
      chaine[i] = (*carac);
      (*carac) = lire( fichier );
      i = i+1;
    }
  i = i-1;

  while( i>= 0 )                  /* converti la chaine en nombre */
    {
      result = result + j*(chaine[i]-'0');
      j = j*10;
      i = i -1;
    }
  
  return( result );
}


/*************************************************************************/
/*************************** lire les blancs *****************************/
/*************************************************************************/
char lireblanc(FILE * fichier,
	       char   carac)
{
  while( isspace( carac ) )
    carac = lire( fichier );
  
  return( carac );
}

/*************************************************************************/
/*************************** lire les espaces ****************************/
/*************************************************************************/
char lirespace( FILE * fichier,
		char   carac)
{
  while(( carac == ' ')||( carac == '	'))
    carac = lire( fichier );
  return( carac );
}


/*************************************************************************/
/**************** lire les blancs avec comptage des lignes ***************/
/*************************************************************************/
char lireblanc1( FILE * fichier,
		 char   carac,
		 int  * nb_ligne)
{
  while( isspace( carac ) )
    {
      if ( carac == '\n' )
        (* nb_ligne )++;
      carac = lire( fichier );
    }
  return( carac );
}



/*************************************************************************/
/*************** lire les blancs sans saut de ligne **********************/
/*************************************************************************/
char lireblanc_chgtligne( FILE * fichier,
			  char   carac)
{
  while(( carac == ' ' )||( carac == '	' ) )
    carac = lire( fichier );
  
  return( carac );
}


/*************************************************************************/
/*************************** lire les noms *******************************/
/*************************************************************************/
char * lirenom( FILE   * fichier,
		char   * carac,
		erreur * err)
{
  int    i;
  char * chaine;

   chaine = ( char * )malloc( (LONGMOT+1)*sizeof( char ));
   if ( chaine == NULL )              /* probleme d'allocation ? */
     {
       (*err) = 7 ;                   /* oui -> erreur */
       return( NULL ); 
     }

   if ( isalpha( (*carac) ) )    /* le 1er caractere est-il alpha-numerique ? */
    {
      i = 0;                     /* oui -> place les lettres dans une chaine */
      while( ((*carac) != ' ')&&((*carac) != '	')&&((*carac) != '\n') )
        {
          if ( i == LONGMOT )
            {
              (*err) = 6;
              return( NULL );
            }
          chaine[i] = (*carac);
          chaine[i+1] = '\0';
          (*carac) = lire( fichier );
          i = i + 1;
        }
    }
  else                               /* non -> erreur */
    {
      (*err) = 5;
      return( NULL );
    }
  return( chaine );
}


/**************************************************************************/
/********************** saisie d'un mot du clavier  ***********************/
/**************************************************************************/
char * saisie_nom()
{
  char * nom;
  int    i;

  i=0;
  nom = ( char * ) malloc( 100*sizeof( char ) );
  scanf("%c",&nom[i]);
  if ( nom[i] == '\n' )
    scanf("%c",&nom[i]);
  while( nom[i] != '\n')
    {
      i = i + 1;
      scanf( "%c",&nom[i]);
    }
  nom[i] = '\0';
  return( nom );
}

