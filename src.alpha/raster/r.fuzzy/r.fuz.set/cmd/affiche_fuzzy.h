/* Fichier d'entete pour l'affiche texte des classes floues	*/
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include "fichiers.h"

void Print_FuzzyType(class Type) ;
/* class Type							*/
/* Affiche un texte equivalent au Type (trangle, trapeze, lr	*/
/* ou inconnu)							*/

void Print_FuzzyTriangle(triangle Triangle) ;
/* triangle Triangle						*/

void Print_FuzzyTrapeze (trapeze Trapeze) ;
/* trapeze Trapeze						*/

void Print_FuzzyLR (LR LRType) ;
/* LR LRType							*/

void Print_FuzzyLRG(LRG LRGTYPE);
/*LRG LRGTYPE */

void Print_FuzzyCarac(nb_flou FuzzyNbr) ;
/* nb_flou FuzzyNbr						*/

void Print_FuzzyVar (varia_floue * FuzzyVar) ;
/* varia_floue * FuzzyVar					*/

void Print_FuzzyClass(classe_floue * Floue) ;
/* classe_floue * Floue						*/
