/*-------------------------------------------------------------------------*/
/*** Fonctions pour l'affichage texte des classes de variable floues	***/

#include "affiche_fuzzy.h"

/*** Affiche le type de la variable					***/
void Print_FuzzyType( class Type )
{
	switch (Type) {
		case trian : printf("TFN ->  "); break ;
		case trap : printf("TRFN ->  "); break ;
		case lr : printf("LR ->  "); break ;
	        case lrg : printf("LRG -> "); break;
		default : printf("Unknown");
	}
}

/*** Affiche les caracteristiques d'une variable.			***/
/*** 3 Types -> 3 Fonctions + 1 pour choisir				***/
void Print_FuzzyTriangle(triangle Triangle)
{
	printf("%f, %f, %f",Triangle[0],Triangle[1],Triangle[2]);
}

void Print_FuzzyTrapeze  (trapeze Trapeze)
{
	printf("%f, %f, %f, %f",Trapeze[0],Trapeze[1],Trapeze[2],Trapeze[3]);
}

void Print_FuzzyLR ( LR LRType)
{
	printf("%f, %f, %f  p=%f  q=%f",LRType[0],LRType[1],
		LRType[2],LRType[3],LRType[4]) ;
}

void Print_FuzzyLRG (LRG LRGType)
{
	printf("%f, %f, %f %f  p=%f  q=%f",LRGType[0],LRGType[1],
		LRGType[2],LRGType[3],LRGType[4],LRGType[5]) ;
}
void Print_FuzzyCarac(nb_flou FuzzyNbr)
{
	switch ( FuzzyNbr.type ) {
		case trian : Print_FuzzyTriangle( FuzzyNbr.classe.tri ) ; break ;
		case trap : Print_FuzzyTrapeze( FuzzyNbr.classe.tra ) ; break ;
		case lr : Print_FuzzyLR( FuzzyNbr.classe.L ) ; break ;
	        case lrg : Print_FuzzyLRG( FuzzyNbr.classe.Lrg ) ; break ;
		default : printf("Unknown fuzzy set type\n");
	}
}

/*** Affiche le nom, le type et les parametres d'une variable floue 	***/
void Print_FuzzyVar ( varia_floue * FuzzyVar)

{
	node *		Noeud = FuzzyVar -> liste_elem ;

	printf("\nVariable name : %s\n",FuzzyVar -> name );

	while ( Noeud != NULL ) {
		printf("- %s : ",(Noeud -> nombre).name);
		Print_FuzzyType( (Noeud -> nombre).type) ;
		Print_FuzzyCarac( Noeud -> nombre ) ;
		printf("\n");

		Noeud = Noeud -> suiv ;
	}

}

/*** Affiche toutes les variables d'une classe floue			***/
void Print_FuzzyClass(classe_floue * Floue )
{
	int i ;

	printf("\n %i fuzzy variables :\n",Floue -> nb);
	for ( i=0 ; i<Floue -> nb ; i++ ) {
		Print_FuzzyVar(Floue -> tab[i]) ;
	}

}
