/**************************************************************************/
/*          include pour les toutes les erreurs : *.c                     */
/*                                                                        */
/*                cree par C. DELON et J.F. SARRAUT                       */
/**************************************************************************/
#ifndef INT_erreur
#define INT_erreur



/**************************************************************************/
/*                       declaration des types                            */
/**************************************************************************/

typedef int erreur ; 
       /* 0 = pas d'erreur                                                 */
       /* 1 = nombre de variables floues incoherent                        */
       /* 2 = il manque une variable floue                                 */
       /* 3 = probleme de lecture d'un mot                                 */
       /* 4 = pas de definition d'une variable flou                        */
       /* 5 = le caractere lu n'est pas du type attendu                    */
       /* 6 = le mot lu est trop long                                      */
       /* 7 = probleme d'allocation d'une chaine de caracteres             */
       /* 8 = les chiffres des nombres flous ne sont pas dans l'ordre      */
       /* 9 = type de nombre flou inexistant                               */
       /* 10 = probleme d'allocation pour un node1                         */
       /* 11 = nom de variable floue qui se repete                         */
       /* 12 = probleme d'allocation pour la classe floue                  */
       /* 13 = probleme d'allocation pour une variable floue               */
       /* 14 = probleme d'allocation d'un nombre flou                      */
       /* 15 = Pas de IF dans le fichier des regles                        */
       /* 16 = Pas de ( en debut de regle                                  */
       /* 17 = Erreur dans une ecriture de classe dans les regles          */
       /* 18 = trop de variables dans une des regles                       */
       /* 19 = Probleme d'allocation pour une liste_regle                  */
       /* 20 = Probleme d'allocation pour un tableau d'entier              */
       /* 21 = Probleme d'allocation pour une info                         */
       /* 22 = Il manque des parentheses fermantes dans la regle           */
       /* 23 = Il manque des parentheses ouvrantes dans la regle           */
       /* 24 = Il y a deux operateurs logique a la suite dans la regle     */
       /* 25 = Une chaine definit dans une regle est inconnue              */
       /* 26 = Il y deux variables a la suite dans la regle                */
       /* 27 = Probleme d'allocation pour une pile                         */
       /* 28 = Impossible de depiler une pile vide                         */
       /* 30 = Impossible d'empiler car la pile est pleine                 */
       /* 100 = fichier des var. floues inexistant ou mauvais chemin       */
       /* 101 = fichier des regles inexistant ou mauvais chemin            */
       /* 102 = le chiffre ne correspond pas a calib/valid ou normal       */
       /* 103 = type de validite inconnue                                  */
       /* 104 = fichier des donnees inexistant ou mauvais chemin           */
       /* 105 = impossibilite d'ouvrir le fichier des resultats            */
       /* 106 = impossibilite d'ouvrir le fichier intermediaire            */
       /* 107 = type de combinaison des resultats inexistant               */
       /* 108 = type de deflouification inexistant                         */
       /* 109 = configuration des operateurs inconnue                      */
       /* 110 = chiffre inadapte pour la visualisation des resultats       */
       /* 111 = fichier des parametres inexistant ou mauvais chemin        */   
       /* 112 = chiffre incoherent pour la representation d'une var. floue */
       /* 113 = presence d'une barre verticale dans un nombre flou         */
       /* 114 = Un nombre flou trapezoidal et en fait triangulaire ( b=c ) */
       /* 115 = incompatibilite entre le numero d'une variable floue et    */
       /*       leur nombre declare au debut du fichier des var. floues    */


/*************************************************************************/
/*               declaration des fonctions et procedures                 */
/*************************************************************************/
void affiche_erreur(erreur err,
		    int    nb_ligne,
		    char  * ch_err);
     /* fonction qui affiche les erreurs qui peuvent etre generees.      */

#endif
