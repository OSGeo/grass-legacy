/* Fichier en-tete pour les graphiques sous Grass */

#include "gis.h"
#include "math.h"
#include "minmax.h"

/* Nombre initiaux des initialisations pour trouver les	*/
/* min et max...					*/
#define	MAXX_INIT	-10000.0
#define MAXY_INIT	-10000.0
#define MINX_INIT	10000.0
#define MINY_INIT	10000.0


/* BORDURE_X et BORDURE_Y definissent l'espace laisse entre	*/
/* le cadre et le bord de l'ecran graphique			*/
#define BORDURE_X	30
#define BORDURE_Y	30

/* Rapport entre la hauteur et la largeur du cadre		*/
/* Hauteur = Largeur * COEF_HL					*/
#define COEF_HL		0.75

/* Nombre maximal de courbes affichees a l'ecran au meme moment	*/
/* Pour une coherence avec GrafGKS, MAX_VAR_DISPLAY est mis	*/
/* a 10								*/
#define MAX_VAR_DISPLAY 10

/* Structure Courbe. Contient les informations sur un trace	*/
typedef struct Courbe {
	float *		X ;		/* Liste des X	*/
	float *		Y ;		/* Liste des Y	*/		
	int		nb_pts ;	/* Nombre de points dans la liste */
	int		Couleur ;	/* Couleur de la courbe */
	char *		Nom ;		/* Nom de la courbe */
	struct Courbe *	Suivante ;	/* Pointeur sur la courbe suivante*/
} Courbe ;

/* Structure Graphique. Ensemble de courbes */
typedef struct Graphique {
	int		Nb_Trace ;	/* Nombre de courbes contenues	*/
	float		MaxX ;		/* Les Maxima et Minima de	*/
	float		MaxY ;		/* l'ensemble des courbes	*/
	float		MinX ;
	float		MinY ;
	char *		Titre ;		/* Titre du graphique		*/
	struct Courbe *	C ;
} Graphique ;

struct Graphique * Create_Graph(char * Title);
/* Renvoie une structure Graphique initialisee avec le titre	*/
/* Title							*/

struct Graphique * Add_To_Graph(struct Graphique * Graph,struct Courbe * C);						
/* Ajoute la Courbe *C au Graphique *Graph. Attention, si tout	*/
/* n'a pas ete initialise correctement avec Create_Graph() et	*/
/* Create_Drawing(), les effets seront indesirables		*/

struct Courbe * Create_Drawing(
float * X,
float * Y,
int Nb,
char * Title,
int Couleur );								       							
/* Initialise une structure Courbe avec les points passes en	*/
/* parametre, le titre et la couleur				*/

void Destroy_Drawing( struct Courbe * Drawing);					
/* Libere la memoire utilisee par une structure Courbe		*/

void Destroy_Graph( struct Graphique * Graph);					
/* Libere la memoire utilisee par une structure Graphique	*/

void HautLarg(int * Hauteur,int * Largeur);						
/* Place dans Hauteur et Largeur les dimensions d'un cadre qui	*/
/* s'inscrit dans l'ecran graphique, en tenant compte de	*/
/* BORDURE_X, BORDURE_Y et de COEF_HL				*/

void Trace_Cadre() ;
/* Trace le cadre en faisant un appel a HautLarg()		*/

void Display_Drawing_Grass( 
 struct Courbe * C,					       
 float MinX,							
 float MinY,							
 float coefX,						       
 float coefY,						        
 int Hauteur);                                                  
							
/* Affiche la courbe *C ainsi que ces successeurs, MinX et MinY	*/
/* sont les memes que dans la structure Graphique, coefX et	*/
/* coefY sont des rapports pour l'affichage (calcules dans	*/
/* Display_Graph_Grass() ), Hauteur est l'ordonnee de la legende*/

void Display_Graph_Grass(struct Graphique * Graph);			       
/* S'occupe de l'affichage de *Graph dans la fenetre graphique	*/
/* de Grass courante.						*/

