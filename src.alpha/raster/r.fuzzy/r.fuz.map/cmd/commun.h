/* .h pour les fonctions communes aux r.fuz.*			*/

#ifndef __COMMUN_FUZ
#define __COMMUN_FUZ

#include "gis.h"
#include "fichiers.h"
#include <string.h>

/* Macros pour MIN et MAX */
#define MAX(A,B) ((A>B)?A:B)
#define MIN(A,B) ((A<B)?A:B)

classe_floue * charge_fichier_flou(char * Raster) ;
int Copy_File(int Src,
	      int Dest) ;
void lowercase(char * chaine) ;

#endif
