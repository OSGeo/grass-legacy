/* Fichier : r.fuz.set.h					*/
/* Necessite la bibliotheque : libflou.a			*/
/* Necessite les bibliotheques : graf, outils, desphi, memg,	*/
/*	X11, SC1.O (Fortran), F77, m				*/

#define  MAXNAMLEN    255
#include <fcntl.h>
#include <stdio.h>
#include "gis.h"
#include <dirent.h>
#include "math.h"
#include "graph_grass.h"
#include "commun.h"
#include "affiche_fuzzy.h"
#include <sys/stat.h>
#include "var_floues.h"


/* Si le define COLOR_TABLE_FIXED est mis, la table des couleurs*/
/* sera partagee avec les autres applications, si le define	*/
/* est commente, alors COLOR_TABLE_FLOAT devient actif, et les	*/
/* couleurs sont locales a la fentre graphique GRASS		*/
#define COLOR_TABLE_FIXED

/* Nombre de points utilises pour tracer une fonction d'apparte-*/
/* nence de type LR						*/
/* Ce nombre sert dans une fonction inspiree de libflou, et qui	*/
/* necessite un nombre impair					*/
#define NB_POINTS_LR    21

/*int Copy_File() ;*/
/* int Src, int Dest						*/
/* Copy le fichier Src dans le fichier Dest. Les deux fichiers	*/
/* doivent etre prealablement ouverts (lecture pour Src et	*/
/* ecriture (ou append) pour Dest				*/
/* Les fichiers sont fermes par la fonction			*/
/* Renvoie l'erreur envoyee par write si erreur			*/

int Assoc_MapFuzzy(char * Raster,char * Fuzzy);  
/* char * Raster, * Fuzzy 					*/
/* Initialise l'element Fuzzy dans le mapset en copiant le	*/
/* fichier Fuzzy dans le repertoire correspondant au raster	*/
/* Raster. Cree le repertoire Fuzzy s'il n'existe pas		*/

/*void Display_FuzzyClass_GrafGKS() ;                           */
/* classe_floue * Floue						*/
/* Affiche une classe floue dans GrafGKS			*/

void Display_FuzzyVar_Grass(varia_floue * FuzzyVar);				       
/* Affiche la variable dans la fenetre Grass			*/

void Display_FuzzyClass_Grass(classe_floue * Floue);						

int Visu_Variables(char * Raster,int Flags);						       
/* Affiche le contenu de la classe decrite dans le fichier de	*/
/* variable floues du raster Raster suivant les parametres Flags*/
/* Bit 0 : Imprime les caracteristiques des variables a l'ecran	*/
/* Bit 1 : Affichage avec GrafGKS				*/
/* Bit 2 : Affichage avec Grass					*/



