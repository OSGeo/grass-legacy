/* Quelques fonctions communes aux r.fuz.*				*/

#include "commun.h"

classe_floue * charge_fichier_flou(char * Raster)

{
	FILE *		FuzzyFile ;
	erreur		err ;
	classe_floue *	Floue ;

	/* Teste l'existence du fichier de variables floues */
	/*	if ( !G_find_file("cell",Raster,G_mapset())) { */
	if ( !G_find_file("fuzzy",Raster,G_mapset())) {
		printf("\n!!! Error : fuzzy variable file not found (raster map not fuzzified)\n");
		exit(1);
	}

	/* Essaye d'ouvrir le fichier */
	if ( (FuzzyFile = G_fopen_old("fuzzy",Raster,G_mapset())) == NULL ) {
		/* G_mapset() a ajouter */
		printf("\n!!! Error in opening fuzzy variable file\n");
		exit(1);
	}

	/* Lecture du fichier pour recuperer la classe floue	*/
	/* lecture() fait partie de libflou			*/

	/* modification de lecture suite a maj de libflou par Morel */
	/* Floue = lecture(FuzzyFile, & err); */
	Floue = lecture_nb_floues(FuzzyFile, & err);

	if (err) {
		/* Il faudrait appeller le traitement de l'erreur de libflou */
		printf("\n!!! Error in reading fuzzy variable file\n");
		exit(1);
	}

	return Floue ;

}

/*-------------------------------------------------------------------------*/
int Copy_File ( int Src,
	       int Dest )
{
	char	Temp ;
	int	err = 1 ;	/* 0 when error in writing file */

	while ( err!=0 && read(Src,&Temp,1)!=0 ) {
		err = write(Dest,&Temp,1) ;
	}

	close (Src) ;
	close (Dest) ;

	return err ;
}

void lowercase ( char * chaine)
{
	while (*chaine!='\0') {
		*chaine = tolower(*chaine) ;
		chaine ++ ;;
	}
}


