#define MOD_erreur
#include <stdio.h>
#include <stddef.h>
#include "erreur.h"

/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/


/**************************************************************************/
/******************** affichage des message d'erreur **********************/
/**************************************************************************/
void affiche_erreur(erreur err,
		    int    nb_ligne,
		    char * ch_err)
{
  switch( err )
    {
      case 0:
        /*printf(" Pas de problemes !!!! \n"); on affiche rien*/
        break;
      case 1:
        printf("\n!!! Nombre de variables floues incoherent sur ligne %d \n",nb_ligne); 
        break;
      case 2:
        printf("\n!!! Variable floue absente sur ligne %d \n",nb_ligne);
        break;
      case 3:
        printf("\n!!! Probleme de lecture d'un mot sur ligne %d \n",nb_ligne);
        break;
      case 4:
        printf("\n!!! Absence de nombre flou definissant la variable floue \n");
        printf("      definie sur ligne %d !\n",nb_ligne-1);
        break;
      case 5:
        printf("\n!!! Caractere lu a la ligne %d de type non conforme \n",nb_ligne);
        break;
      case 6:
        printf("\n!!! Mot lu a la ligne %d trop long (LONGMOT caracteres max.) \n",nb_ligne);
        break;
      case 7:
        printf("\n!!! Probleme d'allocation pour une chaine de caractere \n");
        break;
      case 8:
        printf("\n!!! Chiffres du nombre flou sur ligne %d non ordonnes \n",nb_ligne );
      case 9:
        printf("\n!!! Type de nombre flou  inexistant sur ligne %d \n",nb_ligne);
        break;
      case 10:
        printf("\n!!! Probleme d'allocation pour un node1 \n");
        break;
      case 11:
        printf("\n!!! Nom du nombre ou variable floue sur ligne %d deja existant \n",nb_ligne);
        break;
      case 12:
        printf("\n!!! Probleme d'allocation pour la classe floue \n");
        break;
      case 13:
        printf("\n!!! Probleme d'allocation pour une variable floue \n");
        break;
      case 14:
        printf("\n!!! Probleme d'allocation pour un nombre flou \n");
        break;
      case 15:
        printf("\n!!! Erreur a la ligne %d du fichier des regles : IF est attendu \n",nb_ligne);
        break;
      case 16:
        printf("\n!!! Erreur a la ligne %d du fichier des regles : ( est attendue \n",nb_ligne);
        break;
      case 17:
        printf("\n!!! Erreur a la ligne %d du fichier des regles : la variable %s n'est\n",nb_ligne,ch_err);
        printf("    pas une des classes possibles a cette place \n");
        break;
      case 18:
        printf("\n!!! Erreur a la ligne %d du fichier des regles : il y a trop de variables \n",nb_ligne);
        break;
      case 19:
        printf("\n!!! Probleme d'allocation pour une list_regle \n");
        break;
      case 20:
        printf("\n!!! Probleme d'allocation d'un tableau d'entier \n");
        break;
      case 21:
        printf("\n!!! Probleme d'allocation pour une info \n");
        break;
      case 22:
        printf("\n!!! Erreur dans la regle : il manque une ou des parenthese(s) fermante \n");
        break;
      case 23:
        printf("\n!!! Erreur dans la regle : il manque une ou des parenthese(s) ouvrante \n");
        break;
      case 24:
        printf("\n!!! Erreur dans la regle : il y a deux operateurs logiques a la suite\n");
        printf("    ou un en debut de regle\n");
        break;
      case 25:
        printf("\n!!! Erreur dans la regle : la chaine %s est inconnue \n",ch_err);
        break;
      case 26:
        printf("\n!!! Erreur dans la regle : il y a deux variables a la suite \n");
        break;
      case 27:
        printf("\n!!! Probleme d'allocation pour une pile \n");
        break;
      case 28:
        printf("\n!!! Erreur : impossible de depiler une pile vide \n");
        break;
      case 29:
        printf("\n!!! Erreur : impossible de depiler une pile vide \n");
        printf("          Le depilage etant depuis le bas.\n");
        break;
      case 30:
        printf("\n!!! Erreur : impossible d'empiler car la pile est pleine !!!\n");
        break;
      case 100:
        printf("\n!!! Probleme dans le fichier  parametres :\n");
        printf("    Le nom du fichier des variables floues est invalide \n");
        break;
      case 101:
        printf("\n!!! Probleme dans le fichier  parametres ( ligne %d ) :\n",nb_ligne);
        printf("    Le nom du fichier des regles est invalide \n");
        break;
      case 102:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne);
        printf("    Indice incorrect pour le choix de calib/valid ou prevision\n");
        break;
      case 103:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne);
        printf("    Indice incorrect pour le choix du calcul d'indice de validation \n");
        break;
      case 104:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne);
        printf("    Le nom du fichier ds donnees est invalide \n");
        break;
      case 105:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne);
        printf("    Impossible d'ouvrir le fichier resultats \n" );
        break;
      case 106:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne); 
        printf("    Impossible d'ouvrir le fichier intermediaire \n");
        break;
      case 107:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne); 
        printf("    Indice incorrect pour le choix de l'agregation des regles \n" );
        break;
      case 108:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne); 
        printf("    Indice incorrect pour le choix de la deflouification \n");
        break;
      case 109:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne); 
        printf("    Indice incorrect pour le choix des (t-normes, t-conormes) \n");
        break;
      case 110:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne); 
        printf("    Indice incorrect pour le choix de la visualisation \n");
        break;
      case 111:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne); 
        printf("    Le nom du fichier des parametres est invalide \n");
        break;
      case 112:
        printf("\n!!! Probleme dans le fichier  parametres (ligne %d) :\n",nb_ligne); 
        printf("    Affichage demande pour des variables non definies \n");
        break;
      case 113:
        printf("\n!!! Probleme dans le fichier des variables floues (ligne %d) :\n",nb_ligne);
        printf("    Le nombre flou a une barre verticale :  veuillez changer l'une des bornes\n");
        break;
      case 114: 
        printf("\n!!! Probleme dans le fichier des variables floues (ligne %d) :\n",nb_ligne);
        printf("    Le trapeze est en fait un triangle ( b = c ) : veuillez le redefinir \n");
        break;
      case 115:
        printf("\n!!! Probleme dans le fichier des variables floues (ligne %d) :\n",nb_ligne);
        printf("    Incompatibilite entre le numero variable et leur nombre\n");  
	break;
    case 116:
      printf("ATTENTION = le fichier ne peut etre ouvert\n");
      break;
    case 117:
        printf("!!! Erreur sur variable de sortie %s deja reinjectee : \n",ch_err);
	printf("    impossible d'avoir une meme variable reinjectee plus d'une fois \n");
	break;
    case 118:
        printf("!!! Erreur lors de la lecture de la regle : \n");
	printf("    pas de definition variable de sortie %s \n",ch_err);
	break;
    case 119 :
        printf(" Erreur : Aucun fichier de parametres n'a ete specifie \n");
	break;
    case 120:
        printf("Attention : pile vide il n'y a pas de regle!!\n");
	break;
    case 121:
        printf("ERREUR :: THEN a ete omis dans la regle.\n");
	break;
    case 122:
       printf("ERREUR :: Le ptr sur l'op %d n'a pas peu aboutir ds la recherche\n",nb_ligne);
       printf("ds la table des variables a tab[op-10] pb lors de la def des var\n");
       printf("definise vos variable ds l'ordre d'utilisation ds le fichiers des regles\n");
       break;
    case 123:
      printf(" Nb flou non trouve \n");
      break;
    case 124 :
      printf("ERREUR :: Le pointeur est vide\n");
      break;
    }
  if ( err != 0 )
    exit(0);
}
  
