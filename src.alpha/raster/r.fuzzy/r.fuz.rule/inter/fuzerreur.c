#define MOD_erreur
#include <stdio.h>
#include <stddef.h>
#include "fuzerreur.h"

/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/


/**************************************************************************/
/******************** affichage des message d'erreur **********************/
/**************************************************************************/
void Erreur(erreur err,
	    int    nb,
	    char * ch_err)
{
  switch( err )
    {
      case 0:
        /*printf(" Pas de problemes !!!! \n"); on affiche rien*/
        break;
      case 1:
        printf("\n!!! Error in allocating memory for a string\n");
        break;
      case 2:
        printf("\n!!! Error in allocating memory for the structure `classe floue` \n");
        break;
      case 3:
        printf("\n!!! Error in allocating memory for the structure `variables`\n");
        break;
      case 4:
	printf("\n!!! Error : unable to open rule file %s \n",ch_err);
	break;
      case 41:
	printf("\n!!! Error : unable to open fuzzy variable file");
	printf("\n    of input raster map %s \n",ch_err);
	break;
      case 42:
	printf("\n!!! Error : unable to open fuzzy variable file");
	printf("\n    of output raster map %s \n",ch_err);
	break;
      case 43:
	printf("\n!!! Error : unable to open raster map %s \n",ch_err);
	break;
      case 5:
	printf("\n!!! Error : number of input raster maps does not match");
	printf("\n    the variable number of fuzzy rule file\n");
	break;
      case 6:
	printf("\n!!! Error : fuzzy variables associated to the different raster maps");
	printf("\n    can't be the same\n");
	break;
      case 7:
	printf("\n!!! Error : %s is not a valid option\n",ch_err);
	break;
    }
  if ( err != 0 )
    exit(0);
} 
  
