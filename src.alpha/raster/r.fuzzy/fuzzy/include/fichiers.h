/**************************************************************************/
/*        include pour les operations sur les fichiers : fichiers.c       */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                Modifier le 10/97 par L. MOREL                          */
/*                   derniere modif : 10/97                               */
/**************************************************************************/
#ifndef INT_fichiers
#define INT_fichiers

#include "classes_floues.h"
#include "erreur.h"
#include "regles.h"

/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/

typedef struct node1
          {
            char         * name;
            struct node1 * suiv;
          }node1;

typedef node1 * pilenom;     /* pile qui contient tous les noms des variables */
                             /* et des nombres flous . Cela sert dans les     */
                             /* fonctions de tests                            */


/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/

/* Morel L. Fonctions */



/* Morel L. Fonctions Fin*/

classe_floue * lecture_nb_floues( FILE * fichier,
				  erreur * err);
  /* sorties: classe_floue * result */
  /* Fonction qui construit la classe floue RESULT a partir d'un fichier */
  /* de definition des nombre floues caracterisant la classe floue	 */
  /*  FICHIER est prealablement ouvert en lecture.			 */

void lire_entree(FILE * Fichier);
  /* fonction qui lit le Fichier de parametres et execute toutes les   */
  /* commandes qui lui sont demandees                                  */

erreur test_triangle(FILE    * fichier,
		     char    * carac,
		     int     * nb_ligne,
		     pilenom * Pi);
  /* sorties: erreur    err      */
  /*          char    * carac    */
  /*          int     * nb_ligne */
  /*          pilenom * Pi       */
  /* fonction qui verifie les coordonnes du triangle contenues dans FICHIER */
  /* Elle verifie aussi que le nom qui lui est attribue n'existe pas deja   */

erreur test_trapeze(FILE    * fichier,
		    char    * carac,
		    int     * nb_ligne,
		    pilenom * Pi);
  /* sorties: erreur    err      */
  /*          char    * carac    */
  /*          int     * nb_ligne */
  /*          pilenom * Pi       */
  /* fonction qui verifie les coordonnes du trapeze contenues dans FICHIER  */
  /* Elle verifie aussi que le nom qui lui est attribue n'existe pas deja   */

erreur test_LR(FILE    * fichier,
	       char    * carac,
	       int     * nb_ligne,
	       pilenom * Pi);
  /* sorties: erreur    err      */
  /*          char    * carac    */
  /*          int     * nb_ligne */
  /*          pilenom * Pi       */
  /* fonction qui verifie les coordonnes du L-R contenues dans FICHIER .    */
  /* Elle verifie aussi que le nom qui lui est attribue n'existe pas deja   */

erreur test_floue(FILE * fichier,
		  int  * nb_ligne);
  /* sorties: erreur err      */
  /*          int  * nb_ligne */
  /* test du fichier des variables floues . Si une erreur est detectee son */
  /* code est renvoye dans erreur ainsi que le numero de la ligne          */
  /* test s'il n'y a pas deux noms identiques				   */

erreur test_parametres(FILE * fichier,
		       int  * nb_ligne);
  /* sorties: erreur err      */
  /*          int  * nb_ligne */
  /* test du fichier des parametres . Si une erreur est detectee son code  */
  /* est renvoye dans erreur ainsi que le numero de la ligne               */

void sauve_triangle(FILE * fichier);
  /* sorties: FILE * fichier */
  /* fonction qui demande les caracteristiques d'un triangle et les sauve */
  /* dans FICHIER                                                         */
  
void sauve_trapeze(FILE * fichier);
  /* sorties: FILE * fichier */
  /* fonction qui demande les caracteristiques d'un triangle et les sauve */
  /* dans FICHIER                                                         */

void sauve_LR(FILE * fichier); 
  /* sorties: FILE * fichier */
  /* fonction qui demande les caracteristiques d'un triangle et les sauve */
  /* dans FICHIER                                                         */

void sauve_varia_floue(FILE * fichier,
		       int    num);
  /* sorties: FILE * fichier */
  /* fonction qui demande les caracteristiques de la variable floue NUM et */
  /* les sauve dans FICHIER                                                */   

char * sauve_fichier_flou();
  /* sorties: char * nom */
  /* fonction qui permet l'entree et la sauvegarde du fichier des variables */
  /* floues au clavier . Elle renvoi le nom de ce fichier                   */

char * sauve_fichier_parametres();
  /* sorties: char * nom */
  /* fonction qui permet l'entree et la sauvegarde du fichier des parametres */
  /* au clavier . Elle renvoi le nom de ce fichier                           */


int lecture_regle(FILE * fichier,
		  pile * pi,
		  classe_floue * classe,
		  list_regle ** regles,
		  erreur * err,
		  char ** ch_err);
  /* sorties: list_regle   * regles  */
  /*          pile         * pi      */
  /*          int            err     */
  /*          int            n_ligne */
  /* Lit le fichier des regles , regarde si il y a des erreurs de format */
  /* et range dans regles toutes les regles a verifier. La pile pi est   */
  /* deja en infixee pour la regle lue dans le fichier.                  */
  /* classe->regle contient la chaine de chaque mot composant la regle   */
  /* separe par un point : si la regle est TAir OR Tsol, classe->regle = */
  /* TAir.OR.Tsol */

void ecrit_fic_inter(int          * fic_interm,
		     char         * sortie,
		     double         dof,
		     info         * reg_act,
		     classe_floue * classe);
  /* Ecrit dans le fichier intermediaire les variables avec leur fonction */
  /* d'appartenance ainsi que le dof et la variable de sortie.            */

/* Changement de l'entete pour faire correspondre avec les variables qui sont par MOREL L. 30-10-97 */
/* appele par la fonction lire_entree */
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
			 int            res_model,
			 char          * title_sortie);
  /* Lit le fichier de donnees et calcule au fur et a mesure la valeur de  */
  /* sortie - en suivant toutes les options indiquees par les parametres - */
  /* qui est alors rangee dans le fichier de sortie.                       */

#endif
