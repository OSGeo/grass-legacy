/* Fichier .c pour l'affichage de graph dans Grass */

#include <string.h>
#include "graph_grass.h"
/* Initialise la structure graphique			*/
/* L'allocation memoire est faite par la fonction	*/
struct Graphique * Create_Graph(char *Title )
{
	struct Graphique * Graph ;
       
	if ( (Graph = (struct Graphique *) 
			malloc(sizeof(struct Graphique))) != NULL)
	  {
	    
	    Graph -> Titre = (char *) malloc (sizeof(char) * (strlen(Title)+1));

	    if ((Graph -> Titre)==NULL) return 0 ;
	    Graph -> Nb_Trace = 0;
	    Graph -> MaxX = MAXX_INIT ;
	    Graph -> MaxY = MAXY_INIT ;
	    Graph -> MinX = MINX_INIT ;
	    Graph -> MinY = MINY_INIT ;
	    Graph -> C = NULL ;
  
	    strcpy(Graph -> Titre,Title); 
	    
	    return Graph;
	  }
	return 0 ;
}


/* Ajoute une Courbe a un Graphique */
struct Graphique * Add_To_Graph(
struct Graphique *	Graph ,
struct Courbe *		C )
{
	int i ;

	/* Attention Graph DOIT avoir ete initialise ! */

	if (Graph -> C) 
	  {
	    /* Ce n'est pas la premiere courbe */
	    struct Courbe * PtrC = Graph -> C;
	    
	    /* On se positionne sur la derniere courbe enregistree */
	    while (PtrC -> Suivante != NULL) 
	      {
		PtrC = PtrC -> Suivante ;
	      }
	    
	    PtrC -> Suivante = C ;
	    
	  }
	else 
	  {
	    /* C'est la premiere courbe */
	    Graph -> C = C ;
	  }
	
	Graph -> Nb_Trace ++ ;
	
	/* Changement des Max et Min */
	for (i = 0 ; i < C-> nb_pts ; i++ ) 
	  {
		Graph -> MaxX = MAX(Graph -> MaxX,C -> X[i]) ;
		Graph -> MaxY = MAX(Graph -> MaxY,C -> Y[i]) ;
		Graph -> MinX = MIN(Graph -> MinX,C -> X[i]) ;
		Graph -> MinY = MIN(Graph -> MinY,C -> Y[i]) ;
	  } 

        return Graph;
}

/* Initialise la structure de courbe	*/
/* L'allocation se fait par la fonction	*/
struct Courbe * Create_Drawing(
float * X,
float * Y,
int Nb,
char * Title,
int Couleur)
{
       
        struct Courbe *	Drawing ;
	
	if ( (Drawing = (struct Courbe *)
		malloc (sizeof (struct Courbe)))!=NULL ) 
	  {
	    Drawing -> Nom = (char *) malloc ( sizeof(char) * (strlen(Title)+1));
	    Drawing -> X = (float *) malloc ( sizeof(float) * Nb ) ;
	    Drawing -> Y = (float *) malloc ( sizeof(float) * Nb ) ;

	    if ( Drawing -> X==NULL || Drawing -> Y==NULL ||
		 Drawing -> Nom==NULL) 
	      {

		/* Erreur, la structure n'a pas pu etre	*/
		/* entierement allouee			*/
		free(Drawing) ;
		return 0 ;
	      }	

		strcpy(Drawing -> Nom,Title) ;
		memmove(Drawing -> X , X , Nb * sizeof(float) ) ;
		memmove(Drawing -> Y , Y , Nb * sizeof(float) ) ;
		Drawing -> Couleur = Couleur ;
	        Drawing -> Suivante = NULL ; 
		Drawing -> nb_pts = Nb ;
		
		return Drawing ;

	  }
	return 0 ;
}

/* Detruit la structure Courbe ainsi que ces successeurs */
void Destroy_Drawing(struct Courbe *	Drawing )
{
      
        if (Drawing -> Suivante != NULL)
		   Destroy_Drawing(Drawing -> Suivante) ;
	
	/*free(Drawing->Nom);
	free(Drawing->X);
	free(Drawing->Y); */ 

}

/* Detruit la structure Graphique */
void Destroy_Graph(struct Graphique *	Graph) 
{


  if (Graph -> C != NULL) Destroy_Drawing(Graph->C);
    
}

/* Trace le cadre pour l'affichage de la variable floue */
void Trace_Cadre()
{
        int Hauteur ; 
	int Largeur ; 
	int x[4],y[4] ;
	int i=0;

 
  /* R_font("greeks"); */ 	/* set text parameters */
  /* R_text_size(10,10); */ 
       
   Largeur =( R_screen_rite()-R_screen_left()-2*BORDURE_X) ;
   Hauteur =(R_screen_bot()-R_screen_top()-2*BORDURE_Y) ;
 
   if (Hauteur > (Largeur * COEF_HL) ) 
     {
       Hauteur = (int) (floor( (Largeur) * COEF_HL)) ;
     } 
   else 
     {
       Largeur = (int) (floor( (Hauteur) / COEF_HL)) ;
     }

   x[0]= BORDURE_X ; y[0] = BORDURE_Y ;
   x[1]= BORDURE_X ; y[1] =Hauteur+BORDURE_Y ;
   x[2]=Largeur+BORDURE_X ; y[2] = Hauteur+BORDURE_Y ;
   x[3]=Largeur+BORDURE_X ; y[3] = BORDURE_Y ;
	
   R_polyline_abs(x,y,4);
}

/* Affiche une courbe */
void Display_Drawing_Grass(struct Courbe *	C,
			   float		MinX, 
			   float		MinY, 
			   float		coefX,
			   float		coefY,
			   int		Hauteur)
{
 int j;
	/* Si la courbe est effectivement allouee */
	if ( C!= NULL) 
	  {
	     
		int i ;
		int x=0;
		int y=0 ;
		
		/* Affiche la legende */

		R_color(C->Couleur) ;
		R_move_abs(BORDURE_X,Hauteur+BORDURE_Y+(C->Couleur)*15);
		R_cont_rel(10,0);
		R_move_rel(5,0);
		
		R_color(1);
		R_text(C -> Nom);
		
		
		/* Affiche le graphique */
		R_color(C->Couleur) ;
		if ( C -> nb_pts > 0 ) 
		  {
		    
		    x = (int) floor ((BORDURE_X + (C -> X[0] - MinX) * coefX)); 
		    y = (int) floor ((BORDURE_Y + (C -> Y[0] - MinY) * coefY));
		    
		    R_move_abs( x,y ) ;
		    
		    
		    for (i=1 ; i < C->nb_pts ; i++)
		      {
			x = (int) floor (BORDURE_X + ((C -> X[i] - MinX) * coefX)) ;
			
			y = (int) floor (BORDURE_Y +((C -> Y[i] - MinY ) * coefY)) ;
			
			R_cont_abs(x,y); 
		      }
		  }
		
		/* Recursivite : cherche a afficher la courbe suivante */
	        R_reset_color(0,0,0,0); 
	
		Display_Drawing_Grass( C -> Suivante , MinX , MinY , coefX , coefY ,Hauteur);
		      
	  }
   
}

/* Affiche le graph dans la fenetre grass	*/
/* quit doit etre prealablement ouverte		*/
void Display_Graph_Grass( struct Graphique * Graph )
{
	int	Hauteur, Largeur ;
	int	i ;
	float	coefX,coefY ;
	int	*top,*bottom,*left,*right;
	char	legende[5],Titre[50] ;
	

	/* Cherche la hauteur et la largeur du cadre a afficher */
    Largeur =( R_screen_rite()-R_screen_left()-2*BORDURE_X) ;
    Hauteur =(R_screen_bot()-R_screen_top()-2*BORDURE_Y) ;
    
    if (Hauteur > (Largeur * COEF_HL) ) 
      {
	Hauteur = (int) (floor( (Largeur) * COEF_HL)) ;
      } 
    else 
      {
	Largeur = (int) (floor( (Hauteur) / COEF_HL)) ;
      }
	
    R_color(0);
    /* Affiche le titre */

    R_get_text_box(Graph->Titre,&top,&bottom,&left,&right);  
    R_move_abs( (Largeur-(2*(right-left))) >> 1 , BORDURE_Y >>1 ); 
    R_color(1); 
    R_text(Graph->Titre); 
    
	/* Affiche le cadre */
	R_color(1);
	Trace_Cadre();
	
	/* Affiche les graduations */
	R_color(1);
	R_text_size(8,8);

	for (i=0 ; i<=10 ; i++) 
	  {
		R_move_abs(BORDURE_X,(BORDURE_Y+Hauteur-
			(int) floor(Hauteur*i/10.0))) ;
		R_cont_rel(5,0);
		R_move_abs((BORDURE_X >> 2),(BORDURE_Y+Hauteur-
			(int) floor(Hauteur*i/10.0))) ;
		sprintf(legende,"%1.1f",
			(Graph->MaxY - Graph-> MinY)*i/10.0 +Graph-> MinY);
		R_text(legende);

		R_move_abs((BORDURE_X+(int) floor(Largeur*i/10.0)),
				(BORDURE_Y+Hauteur));
		R_cont_rel(0,-5);
		R_move_abs((BORDURE_X + (int) floor(Largeur*i/10.0)),
				(BORDURE_Y+Hauteur+(BORDURE_Y >> 1))) ;
		sprintf(legende,"%1.1f",
			(Graph->MaxX -Graph-> MinX)*i/10.0 +Graph-> MinX);
		R_text(legende);
	       
	  }
	/* Fin des graduations */
	
  

	coefX = ((float) Largeur ) / ((float)
			(Graph->MaxX - Graph-> MinX)) ;
	coefY = ((float) Hauteur ) / ((float)
			(Graph->MaxY - Graph->MinY)) ;

	Display_Drawing_Grass(Graph ->C,Graph->MinX,
		       Graph->MinY,
			coefX,
			coefY,
			Hauteur) ;

}


