/**************************************************************************/
/*                 include pour les regles : regles.c                     */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/*                                                                        */
/*                   derniere modif : 10/97 par  MOREL l.                 */
/**************************************************************************/
#ifndef INT_regles
#define INT_regles
#include "erreur.h"
#include "definitions.h"
#include "var_floues.h"
#include "piles.h"
#include "classes_floues.h"
/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/

typedef enum op     /*   operateurs pouvant apparaitre dans le langage  */      
  {
    NOT,        /*  0  */
    AND,        /*  1  */
    OR,         /*  2  */
    XOR,        /*  3  */
    paro,       /*  4  */
    parf        /*  5  */
  }operateur;

typedef struct info           /* info de chaque variable pour l'execution */
  {                           /* lors de la verification des regles et du */
    char name[LONGMOT];       /* calcul des DOF                           */
    double valeur;            /* name contient le nom de la classe        */
    int num_var_sortie;	      /* valeur contient la valeur de la variable */
    node * var_sortie;	      /* sortie correspond numero i de la variable */
  }info;                        /* de sortie correspondant a tab[i] et */
			      /* node ptr sur la var sortie correspondant a tab[i]->list_elem */
/* de tel sortie de info.name == tab[i]->list_elem->nombre.name */

typedef struct list_regle      /* list_regle contient la liste de toutes  */
  {                            /* les regles generees                     */
    info * reg_act;            /* num est le numero de la regle           */
    struct list_regle * suiv;
    int num;
  }list_regle;

typedef struct sol        /* sol est la liste de toutes les "regles" verifiees */
  {                       /* elle contient le nom de la classe de la var de    */
    double dof;           /* sortie avec le dof associee pour la combinaison   */
    char name[LONGMOT];
    int num_regle;
    /* MODIF MOREL L. 24-11-97 */
    list_regle * regle;   /* permet d'avoir le ptr sur la regle ce qui accelere le tps de calcul */
    /* Fin MODIF */
    struct sol * suiv;
  }sol;


/*********************************************************************/
/*                    declarations des procedures                    */
/*********************************************************************/

/* Morel L. Fonctions */
void LibereSolution(sol * Sol);
  /* sorties :  rien			*/
  /* Parcours la liste des solutions Sol et liberes un par un les cases memoire */
  /* par appel recursif */


/********************************************************************(*****/
/******* Affichage de la solution *****************************************/
/**************************************************************************/
void AfficheSolution(sol * Sol,
		     int nb_var);
  /* sorties :  rien			*/
  /* Parcours la lite des solutions Sol et l'affiche */

/********************************************************************(*****/
/******* Affichage de la solution *****************************************/
/**************************************************************************/
void AfficheRegles(list_regle  * Regles,
		   int           Nb_var);
  /* Parcours la liste des regles et l'affiche */


erreur trans_pile(pile         * pi,
		  char         * regle,
		  int          * var_act,
		  int          * ench_var,
		  classe_floue * classe);
  /* sorties: pile         * pi       */
  /*          int          * var_act  */
  /*          int          * ench_var */
  /* Transforme la regle ecrite en une pile d'operations.              */
  /* De plus cette fonction range dans ench_var, qui est un tableau ,  */
  /* l'enchainement des variables dans la regle pour les verifications */
  /* ulterieures et var_act donne le nombre de variables apparues.     */

/*********************************************************************/
char * lit_av_point(char * regle,
		    int  * i);
  /* sorties: int  * i     */
  /*          char * var   */
  /* Retourne la chaine ecrite dans la chaine entrante jusqu'au         */
  /* premier point ou parenthese ou encore retour chariot.              */
  /* Tient a jour i qui est la "tete de lecture" de la chaine entrante. */

/*********************************************************************/
erreur trouve_nom(operateur    * op,
		  char         * nom,
		  int          * op_int,
		  classe_floue * classe);
  /* sorties: operateur    * op     */
  /*          erreur         err    */
  /* Renvoi le code operateur correspondant a la chaine passee en */
  /* parametre et verifie si il est autorise suivant op_int qui   */
  /* interdit certains operateurs et est remis a jour ici.        */


/*********************************************************************/
int priorite(operateur op);
  /* sorties: int       prio */
  /* Renvoi la priorite de l'operateur. */

/*********************************************************************/
int eval_bool(pile           post,
	      info         * reg_act,
	      classe_floue * classe);
  /* sorties: int            bool    */
  /* Retourne le resultat booleen correspondant a l'execution */
  /* de la pile post avec les valeurs de reg_act.             */

/*********************************************************************/
double eval_dof(pile           post,
		info         * reg_act,
		int            config[4],
		classe_floue * classe);
  /* sorties: double         dof       */
  /* Retourne le dof correspondant a l'execution de la pile post */
  /* avec les valeurs de reg_act et de la maniere indiquee par   */
  /* config - au niveau de chaque operateurs -.                  */

/*********************************************************************/
int verifie(int            num_var,
            char           classe_var[LONGMOT],
            double         valeur,
            classe_floue * classe);
  /* sorties: int            bool                */
  /* Retourne 1 si la valeur est dans la classe demandee de la */
  /* variable num_var et 0 sinon.                              */

/*********************************************************************/
char * lit_var_sortie(char ** regle ,
		      int     i,
		      int * nb_var_sortie);
  /* sorties: char  * reg   */
  /* Specification externe modifier 10/97 par MOREL l. 
     Extrait de la chaine regle du systeme, qui est composer de ce qui se trouve apres "IF ("
     jusqu'a la fin de la regle et recupere le nom de la variable de sortie qui sera retourner.
     La chaine regle est sectionnee de facon a ce que regle ne contienne que ce qui est ecris entre
     le "IF (" et le ") THEN".*/

/*********************************************************************/
int verifie_var(char         * nom,
		int          * var_act,
		int          * ench_var,
		classe_floue * classe);
  /* sorties: int            bool     */
  /* Verifie si nom est l'une des classes de la variable ench_var[var_act]. */

/*********************************************************************/
void saisie_valeur(list_regle  ** regles,
		   classe_floue * classe);
  /* sorties: list_regle  ** regles */
  /* Saisie a la main d'un uplet de valeur range dans regles. */

/*********************************************************************/
sol * test_regles(pile           post,
		  list_regle   * regles,
		  int            config[4],
		  classe_floue * classe,
		  int            fic_inter,
		  int            fic_interm);
  /* sorties: sol          * solution   */
  /* Teste l'ensemble des regles et retourne dans solution la liste */
  /* des regles verifiees.                                          */

/*********************************************************************/
double combi_deflou(sol          * solution,
		    classe_floue * classe,
		    int            deflou_type,
		    int            combi_type,
		    int            visu,
		    int            num_var_sortie,
		    discret        * ptr_dis);
  /* sorties: double         val_sortie  */
  /* Retourne la valeur de sortie apres avoir combine les differentes     */
  /* regles et defloue la variable de sortie suivant la methode indiquee. */
  /* De plus elle indique au procedures de deflouification si il faut     */
  /* visualiser ou non la sortie.                                         */


#endif
