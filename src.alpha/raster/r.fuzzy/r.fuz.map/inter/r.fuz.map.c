/* r.fuz.map */

#include "r.fuz.map.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char *buffer_Var=NULL;
char *buffer_Set=NULL;

/* Ouvre les fichiers entree et sortie, et lit le contenu du fichier flou */
void OuvreFichiers(
char *	Raster ,
char *	CellDest ,
int  *	CellIn ,
int  *	CellOut ,
classe_floue ** Floue )
{
  char reponse=0;
  char trash[256];

	/* Teste l'existence du fichier cell du Raster */
	if ( !G_find_file("cell",Raster,G_mapset())) 
	  {
	    printf("\n!!! Error : raster file not found\n");
	    exit(1);
	  }

	/* Charge la classe floue */
	*Floue = charge_fichier_flou(Raster) ;
       
	/* Teste la non existence du fichier destination */
	/* Et permet d'utiliser le meme nom */
	if ( G_find_file("cell",CellDest,G_mapset())) 
	  {
	    printf("\n--> %s already exists, ok to overwrite ?",CellDest);
	    while ((reponse!='y') || (reponse!='n'))
	      {	
		printf(" (y/n) ");
		reponse=getchar();
		
		if(reponse == 'y')
		  {
		    gets(trash);
		    break;
		  }
		else
		  {
		    if (reponse == 'n')
		      exit(1);
		  }
	       /* Retire le dernier caract entre */
		gets(trash);
	      }
		
	  }

       

        if ( (*CellIn= G_open_cell_old(Raster,G_mapset())) < 0 ) 
	  {
	    printf("\n!!! Error : unable to open input raster map\n");
	    exit(1);
	  }
 
	if ( (*CellOut = G_open_cell_new(CellDest)) < 0 ) 
	  {
	    printf("\n!!! Error : unable to open output raster map\n");
	    
	    /* Le G_close_cell() n'est pas forcement necessaire	*/
	    /* a cause du exit(), suivant ce que fait G_close_cell	*/
	    G_close_cell(CellIn) ; 
	    exit(1);
	  }
}

/* Extrait le nombre flou du nom "NomVarMu" de la classe Floue */
void TrouveNomMu(
classe_floue *	Floue ,
char *		NomVarMu ,
nb_flou *	NbFlou ,
char *		Force )
{
	int ok = 0 ;
	int i=0;
	int j=0;
	char cmp1[255] ;
	char cmp2[255] ;
	char reponse;
	char trash[256];
	node*Noeud;
	
	if (Force) 
	  {
	    i = IndexForce(Force,Floue) ;
	  }

	j=i;
	strcpy(cmp2,NomVarMu) ;
	/* lowercase(cmp2); */

/* Recherche de la fonction d'appartenance concernee */
	/* Si la variable n'est pas forcee, le premier nom trouve est	*/
	/* considere comme le bon (dans le cas de plusieurs noms	*/
	/* identiques) */

     	while (j < Floue -> nb && !ok) 
	  {
	   Noeud = ( Floue -> tab[j]) -> liste_elem ;
	    while (Noeud != NULL && !ok) 
	      {
		strcpy(cmp1,(Noeud -> nombre).name) ;	
		if(strcmp(cmp1,cmp2)==0 && j==i)
		  {
		    *NbFlou = Noeud -> nombre;
		    ok ++ ;
		  }
	   	Noeud = Noeud -> suiv ;
	      }
	    j++;
	  } 

	

 if(!ok && buffer_Set==NULL) 
	    {
	      buffer_Set=malloc(sizeof(char)*255); /* alloue de la memoire pour placer en attente le nom choisi */

	      printf("\n!!! Warning : selected membership function does not match ");
	      printf("\n    those included in the fuzzy variable file\n");	      

	      printf ("\n   list of available membership functions :\n");
	      while (i < Floue -> nb && !ok) 
		{
		  Noeud = ( Floue -> tab[i]) -> liste_elem ;
	   
		  while (Noeud != NULL) 
		    {
		      printf("%s \n",(Noeud -> nombre).name);
		      ok ++ ;
		      Noeud = Noeud -> suiv ;
		    }
		} 
	      ok=0;
	      j=i;
	      printf ("\n--> Select a new membership function ?");
	      while ((reponse!='y') || (reponse!='n'))
		{ 
		  printf(" (y/n) ");
		    reponse=getchar();
		    
		    if(reponse == 'y')
		      {
			gets(trash);

			/* Debut de la boucle pour verification du nom entre */

			while (strcmp(cmp1,cmp2)!=0)
			  {
			    printf("   select a name in the previous list :");
			    gets(cmp2);
			    fflush(stdin); /* Retire de stdin les caract parasites */
			    ok=0;
			    while (j < Floue -> nb && !ok) 
			      {
				node * Noeud = ( Floue -> tab[j]) -> liste_elem ;
				
				while (Noeud != NULL && !ok) 
				  {
				    strcpy(cmp1,(Noeud -> nombre).name) ;
				    if(strcmp(cmp1,cmp2)==0 && j==i)
				      {
					*NbFlou = Noeud -> nombre;
					strcpy(buffer_Set,cmp1);
					ok ++ ;
				      }
				    Noeud = Noeud -> suiv ;
				  }
				j++;
			      }
			    j=i;
			  }
			
			/* fin de la boucle */
			
			break;
		      }
		    
		    else
		      
		      {
			if (reponse == 'n')
			  exit(0);
		      } 
		    
		    /* Retire le dernier caract entre */
		    gets(trash);
		}
	    }
 /* Permet de ne pas avoir a rechoisir le nom lorsque plusieurs raster sont calculés */

	if ( !ok && buffer_Set!=NULL )
	    {
	      strcpy(cmp2,buffer_Set);
	       while (i < Floue -> nb && !ok) 
		 {
		   node * Noeud = ( Floue -> tab[i]) -> liste_elem ;
		   
		   while (Noeud != NULL && !ok) 
		     {
		       strcpy(cmp1,(Noeud -> nombre).name) ;
		       if(strcmp(cmp1,cmp2)==0)
			 {
			   strcpy(buffer_Set,cmp2);
			   *NbFlou = Noeud -> nombre;
			   ok ++ ;
			 }
		       Noeud = Noeud -> suiv ;
		     }
		   i++;
		 }
	    }
	


}


/* Retourne la valeur de Mu pour la valeur Cell dans la classe Floue	*/
/* en tenant compte de toutes les fonctions d'appartenance de la	*/
/* premiere variable floue de la classe					*/
double CalculMuTotal(
classe_floue *	Floue ,
CELL		Cell ,
int		iForce,
int drapeau )
{
  double    x;
  double mu=0;
  node*Noeud;
     
  if(drapeau==0)
    {  
      if (Cell!=0.0)
	{
	  x=(double)Cell;
	}
      else
	{
	  mu=0.0;
	  return mu;
	}
    }   
  else
    x=(double)Cell;

 Noeud=(Floue->tab[iForce])->liste_elem;
 
 while (Noeud != NULL) 
    {
      switch ((Noeud -> nombre).type) 
	{
	case trian :
	  {
	    mu=MAX(mu,appartient_triangle(&(Noeud->nombre),x));
	    
	    break ;
	  }
	case trap :
	  {
	    mu=MAX(mu,appartient_trapeze(&(Noeud->nombre),x));
	    

	    break ;
	  }
	  
	case lr :
	  {
	    mu=MAX(mu,appartient_LR(&(Noeud->nombre),x));

	    break ;
	  }
	
	case lrg :
	  {
	    mu=MAX(mu,appartient_LRG(&(Noeud->nombre),x));

	    break ;
	  } 
 
	default :
	  {
	    printf("\n!!! Error : unknown fuzzy set type, must be TFN, TRFN, LR or LRG\n");
		exit(1);
	  }
	
	}
 
      Noeud = Noeud -> suiv ;
    }
      
  return mu;
}

/* Retourne la valeur de Mu pour la valeur Cell dans le nombre NbFlou	*/
double CalculMuSimple( nb_flou	NbFlou ,
		       CELL	Cell,
		       int drapeau )
{
 double    x;
  double mu=0;
 
     
  if(drapeau==0)
    {  
      if (Cell!=0.0)
	{
	  x=(double)Cell;
	}
      else
	{
	  mu=0.0;
	  return mu;
	}
    }   
  else
    x=(double)Cell;

      switch (NbFlou.type) 
	{
	case trian :
	  {
	    mu=appartient_triangle(&(NbFlou),x);
	    
	    break ;
	  }
	case trap :
	  {
	    mu=appartient_trapeze(&(NbFlou),x);
	    

	    break ;
	  }
	  
	case lr :
	  {
	    mu=appartient_LR(&(NbFlou),x);

	    break ;
	  }
	    	
	case lrg :
	  {
	    mu=appartient_LRG(&(NbFlou),x);

	    break ;
	  }  

	default :
	  {
	    printf("\n!!! Error : unknown fuzzy set type, must be TFN, TRFN, LR or LRG\n");
		exit(1);
	  }
	
	}
 
    
      
  return mu;

}




/* Calcul la carte Mu des mu a partir de la carte Raster pour un	*/
/* nombre NomVarMu (si elle n'est pas precisee (NULL), le Mu est	*/
/* sur toute la premiere variable de la classe floue			*/
void CarteMu(char * Raster ,
	     char * Mu ,
	     char * NomVarMu ,
	     char * Force, 
	     int drapeau)
{
	int		CellIn ;	/* Ident. pour le fichier en entree	*/
	int		CellOut ;	/* Ident. pour le fichier en sortie	*/
	CELL *		Rang_Cell ;	/* Structure de chargement du cell	*/
	int		nrows ;		/* Nombre de rangs dans le cell		*/
	int		ncols ;		/* Nombre de colonnes dans le cell	*/
	int		i,j ;		/* Pour les boucles			*/
	classe_floue *	Floue ;
	nb_flou		NbFlou ;	/* Le nombre flou concerne */
	int		MuTotal = 0 ;	/* 1 pour le mu sur toutes les variables */
	int		iForce = 0 ;
   	
 
	OuvreFichiers(Raster,Mu,&CellIn,&CellOut,&Floue);

	if (NomVarMu==NULL)   
	  {
	    MuTotal ++ ;

	    printf("\n... opening mu raster map for all membership functions\n");
	    
	    if ( (Floue -> nb) > 1 && !Force) 
	      {
		printf("\n... founding several fuzzy variables,\n");
		printf("    selected variable is %s\n",(Floue -> tab[0]) -> name);
	      } 
	    else 
	      if (Floue -> nb == 0) 
		{
		  printf("\n!!! Error : no fuzzy variable found\n");
		  exit(1);
		} 
	      else 
		if (Force) 		  
		  {
		    iForce = IndexForce(Force,Floue);
		  }
	  } 
	  
	/* Alloue la memoire et recupere les caracteristiques */
	Rang_Cell = G_allocate_cell_buf() ;
	nrows = G_window_rows() ;
	ncols = G_window_cols() ;
  
	if (MuTotal==0) 
	  {
	    TrouveNomMu(Floue,NomVarMu,&NbFlou,Force) ;
	    printf("\n... opening mu raster map for %s\n",NbFlou);
	  }

	for (i = 0 ; i<nrows ; i++) 
	  {
	    
	    /* G_get_map_row et G_get_map_row_nomask s'occupe des messages d'erreurs */
	   
		G_get_map_row(CellIn,Rang_Cell,i) ;

	     for (j = 0 ; j<ncols ; j++) 
	      {
		double mu ;
		
		if (MuTotal) 
		  {
		     mu = CalculMuTotal(Floue,Rang_Cell[j],iForce,drapeau) ;
		  }

		else 
		  {
		    mu = CalculMuSimple(NbFlou,Rang_Cell[j],drapeau) ;
		  }
		      
		Rang_Cell[j] = floor (mu*100);	
	      }
	     G_put_map_row(CellOut,Rang_Cell); 
	  }
       
   	
	G_close_cell(CellIn) ;
	G_close_cell(CellOut);
	free(Rang_Cell); 
      
	
}



/* Calcul l'appartenance seuillee de Cell pour la premiere variable de	*/
/* de la classe "Flou" en fonction de SeuilF pour tous les set */

int CalculSeuilHTotal(
classe_floue *	Floue ,
CELL		Cell ,
double		SeuilF ,
int		iForce,
int drapeau)
{
  int result=0;
  int index=1;
  node * Noeud ;
  double    x;
  char d[2];
  
  Noeud = ( Floue -> tab[iForce]) -> liste_elem ;

  /* x va servir a balayer l'image et a recuperer les valeurs de chaque pixel */
  if(drapeau==0)
    {  
      if (Cell!=0.0)
	{
	  x=(double)Cell;
	}
      else
	{
	  result=0;
	  return result;
	}
    }   
  else
    x=(double)Cell; 
 
  while (Noeud != NULL) 
    {
      switch ((Noeud -> nombre).type) 
	{
	case trian :
	  {
	    if(appartient_triangle(&(Noeud->nombre),x)>=SeuilF)
	      {
		result=index;
		SeuilF = appartient_triangle(&(Noeud->nombre),x);
	      }
	    break ;
	  }
	case trap :
	  { 
	    if (appartient_trapeze(&(Noeud->nombre),x)>=SeuilF)
	      {
		result =index;
		SeuilF = appartient_trapeze(&(Noeud->nombre),x);
	      }
	    break ;
	  }
	  
	case lr :
	  {
	    if(appartient_LR(&(Noeud->nombre),x)>=SeuilF)
	      {	 
		result=index;
		SeuilF = appartient_LR(&(Noeud->nombre),x);
	      }
	    
	    break ;
	  }
	
	case lrg :
	  {
	    if(appartient_LRG(&(Noeud->nombre),x)>=SeuilF)
	      {	 
		result=index;
		SeuilF = appartient_LRG(&(Noeud->nombre),x);
	      }
	    
	    break ;
	  } 
	default :
	  {
	    printf("\n!!! Error : unknown fuzzy set type, must be TFN, TRFN, LR or LRG\n");
		exit(1);
	  }
       
	}
 
      Noeud = Noeud -> suiv ;
      index++;
    
    }
  return result ;
}
/* Calcul l'appartenance seuillee de Cell pour la premiere variable de	*/
/* de la classe "Flou" en fonction de SeuilF et du set defini */
int CalculSeuilHSimple(
nb_flou  NbFlou ,
CELL		Cell ,
double          SeuilF,
int drapeau )
{
 
  int result=0;
  int index=1;
  node * Noeud ;
  double    x;
  double mu;


  /* x va servir a balayer l'image et a recuperer les valeurs de chaque pixel */
   if(drapeau==0)
    {  
      if (Cell!=0.0)
	{
	  x=(double)Cell;
	}
      else
	{
	  result=0;
	  return result;
	}
    }   
  else 
      x=(double)Cell;
    
 
      switch (NbFlou.type) 
	{
	case trian :
	  {
	    if(appartient_triangle(&(NbFlou),x)>=SeuilF)
	      {
		result=index;
		SeuilF = appartient_triangle(&(NbFlou),x);
		index++;
	      }
	    break ;
	  }
	case trap :
	  { 
	    if (appartient_trapeze(&(NbFlou),x)>=SeuilF)
	      {
		result =index;
		SeuilF = appartient_trapeze(&(NbFlou),x);
		index++;
	      }
	    break ;
	  }
	  
	case lr :
	  {
	    if(appartient_LR(&(NbFlou),x)>=SeuilF)
	      {
		result=index;
		SeuilF = appartient_LR(&(NbFlou),x);
		index++;
	      }
	    
	    break ;
	  }

	case lrg :
	  {
	    if(appartient_LRG(&(NbFlou),x)>=SeuilF)
	      {
		result=index;
		SeuilF = appartient_LRG(&(NbFlou),x);
		index++;
	      }
	    
	    break ;
	  } 
	default :
	  {
	    printf("\n!!! Error : unknown fuzzy set type, must be TFN, TRFN, LR or LRG\n");
		exit(1);
	  }
	
	}
  return result ;
}

/* Cree la carte Class de classification seuillee a partir de Raster pour */
/* Si Seuil n'est pas precise (NULL), Seuil sera fixe a H_DEFAUT	*/
void CarteClass(
char * Raster ,
char * Class ,
char * Set,
char * Seuil ,
char * Force, 
int drapeau)
{
	int 		CellIn=0 ;	/* Ident. pour le fichier en entree	*/
	int		CellOut=0 ;	/* Ident. pour le fichier en sortie	*/
	CELL *		Rang_Cell ;	/* Structure de chargement du cell	*/
	int		nrows ;		/* Nombre de rangs dans le cell		*/
	int		ncols ;		/* Nombre de colonnes dans le cell	*/
	int		i,j ;		/* Pour les boucles			*/
      	classe_floue *	Floue ;
	double	        SeuilF ;
	nb_flou		NbFlou ;	/* Le nombre flou concerne */
	int		MuTotal = 0 ;	/* 1 pour le mu sur toutes les variables */
	struct Categories 	Cats ;	/* Eyes */
	node *		Noeud ;
	int		n ;
	int		iForce=0 ;
	char set[20];


	if (Seuil==0) 
	  {
	    SeuilF = (double) H_DEFAUT ;
	    printf("\n... threshold not defined : set to %f\n",SeuilF);
	  }
	else 
	  {
	    SeuilF = atof(Seuil) ;

       if ((SeuilF<0.0) || (SeuilF>1.0))
            {
                printf("\n!!! Error : threshold h must be set in the range [0.0-1.0] \n");
                exit(0);
            }

	  }
 
	OuvreFichiers(Raster,Class,&CellIn,&CellOut,&Floue);
	
	if (Set==NULL) 
	  {
	    MuTotal ++ ; 

	    printf("\n... opening class raster map for all membership functions\n");
	    
	    if ( (Floue -> nb) > 1 && !Force) 
	      {
		printf("\n... founding several fuzzy variables,\n");
		printf("    selected variable is %s\n",(Floue -> tab[0]) -> name);
	      } 
	    else 
	      if (Floue -> nb == 0) 
		{
		  printf("\n!!! Error : no fuzzy variable found\n");
		  exit(1);
		} 
	      else 
		if (Force) 		  
		  {
		    iForce = IndexForce(Force,Floue);
		  }
	  } 
	/* else 
	  {
	    printf("Carte Class pour : %s\n",Set);
	    } */
	if (MuTotal==0) 
	  {
	    TrouveNomMu(Floue,Set,&NbFlou,Force) ;
	    printf("\n... opening class raster map for %s\n",NbFlou);
	  } 

	printf("\n... threshold value h set to  %f\n",SeuilF);
	
	/* Alloue la memoire et recupere les caracteristiques */
	Rang_Cell = G_allocate_cell_buf() ;
	nrows = G_window_rows() ;
	ncols = G_window_cols() ;
	for (i = 0 ; i<nrows ; i++) {
     
	  /* G_get_map_row s'occupe des messages d'erreurs */
	  G_get_map_row(CellIn,Rang_Cell,i) ;

	  for (j = 0 ; j<ncols ; j++) 
	    {
	      int result; 
	      	if (MuTotal) 
		  {
		    result = CalculSeuilHTotal(Floue,Rang_Cell[j],SeuilF,iForce,drapeau); 
		  }

		else 
		  {
		    result = CalculSeuilHSimple(NbFlou,Rang_Cell[j],SeuilF,drapeau) ;
		  }
	   	
	      
	      Rang_Cell[j] =  result ; 
	    }
	  G_put_map_row(CellOut,Rang_Cell);
	  
	}
	
	G_close_cell(CellIn) ;
	G_close_cell(CellOut) ; 
    
/* Fabrication du fichier de categorie */ 
	    
	G_init_cats( 0,"Threshold classification",&Cats); 
	Noeud=(Floue->tab[iForce])->liste_elem;
	n = 1 ;
	
	if (Set!=NULL)
	  {
	    if (buffer_Set!=NULL)
	      strcpy(set,buffer_Set);
	    else
	      strcpy(set,Set);
	    G_set_cat( 0,"no data",&Cats);
	    G_set_cat( n ,set,&Cats);
	  }
	else
	  {

	    while(Noeud!=NULL)
	      {
		G_set_cat( 0,"no data",&Cats);
		G_set_cat( n ,(Noeud->nombre).name,&Cats);
		Noeud = Noeud -> suiv ;
		n++; 
	      } 
	  }
	if (G_write_cats(Class,&Cats)<0) 
	  {
	    printf("\n!!! Error in writting categories\n");
	  }
	G_free_cats(&Cats); 
}

/* Renvoie l'indice d'intersections de Cell dans la premiere variable	*/
/* de la classe "Flou", et ajoute la categorie de pCats			*/
int CalculInter(
		classe_floue *		Flou ,
		CELL			Cell ,
		struct Categories *	pCats ,
		int			iForce,
		int drapeau,
		int v)
{
	/* Recalculer le nom a chaque fois n'est pas forcement le bon moyen	*/
	/* voir d'autres algos							*/
  
  double x;
  int result = 0 ;
  int index = 0 ;
  char chaine[MAX_CUMUL_CHAINE] ;
  node * Noeud ;
  int i=2;
  int j;
  char *loc;
  char plus='+';

  Noeud = ( Flou -> tab[iForce]) -> liste_elem ;
  chaine[0]='\0';
 
  /* x va servir a balayer l'image et a recuperer les valeurs de chaque pixel */
  if(drapeau==0)
    {  
      if (Cell!=0.0)
	{
	  x=(double)Cell;
	}
      else
	{
	  result=0.0;
	  return result;
	}
    }   
  else
    x=(double)Cell;
    
  while (Noeud != NULL) 
    {
    
      switch ((Noeud -> nombre).type) 
	{
	case trian :
	  {
	    if(appartient_triangle(&(Noeud->nombre),x))
	      {
		result|=1<<index;
		strcat(chaine,(Noeud->nombre).name);
		strcat(chaine,"+");
		i++;
	      }
	    break ;
	  }
	case trap :
	  {		      
	    /* double appartient_trapeze( flou, x ) */
	    /* nb_flou * flou; */
	   if (appartient_trapeze(&(Noeud->nombre),x))
	     {
	       result|=1<<index;
	       strcat(chaine,(Noeud->nombre).name);
	       strcat(chaine,"+");
	       i++;
	     } 
	    break ;
	  }
	  
	case lr :
	  {
	    if(appartient_LR(&(Noeud->nombre),x))
	      {
		result|=1<<index;
		strcat(chaine,(Noeud->nombre).name);
		strcat(chaine,"+");
		i++;
	      }	
	    
	    break ;
	  }	  
	
	case lrg :
	  {
	    if(appartient_LRG(&(Noeud->nombre),x))
	      {
		result|=1<<index;
		strcat(chaine,(Noeud->nombre).name);
		strcat(chaine,"+");
		i++;
	      }	
	    
	    break ;
	  }
	      
	default :
	  {
	    printf("\n!!! Error : unknown fuzzy set type, must be TFN, TRFN, LR or LRG\n");
		exit(1);
	  }
	
	}
      Noeud = Noeud -> suiv ;
      index++;
    }

	if (chaine[0]!='\0') 
	  {
	    /* Coupe le dernier + si la chaine n'est pas vide */
	    chaine[strlen(chaine)-1]='\0';
	  } 	   
	
	loc=strchr(chaine,plus);
     
/* Traite toutes les fonctions d'appartenances et leurs intersections */      
	if (v==0)
	  {
	   G_set_cat(result,chaine,pCats) ;
	     return result ;
	  }	     	
   
/* Traite uniquement les intersections */
	if((v==1) && (loc==NULL)) /* Permet de sortir que les intersections */
	   {
	     result=0;
	     return result;
	   } 
	 else
	  {
	    G_set_cat(result,chaine,pCats) ;
	    return result ; 
	  }
	 
}

/* Cree la carte Inter des intersections a partir de Raster		*/
void CarteInter(
char * Raster ,
char * Inter ,
char * Force,
int drapeau,
int v )
{
	int		CellIn ;	/* Ident. pour le fichier en entree	*/
	int		CellOut ;	/* Ident. pour le fichier en sortie	*/
	CELL *		Rang_Cell ;	/* Structure de chargement du cell	*/
	int		nrows ;		/* Nombre de rangs dans le cell		*/
	int		ncols ;		/* Nombre de colonnes dans le cell	*/
	int		i,j ;		/* Pour les boucles			*/
	classe_floue *	Floue ;
	struct Categories	Cats ;	/* Eyes */
	node *		Noeud ;
	int		n ;
	int		iForce ;
	
	printf("\n... opening intersection raster map\n");
	OuvreFichiers(Raster,Inter,&CellIn,&CellOut,&Floue);
	iForce = IndexForce(Force,Floue) ;

	/* Alloue la memoire et recupere les caracteristiques */
	Rang_Cell = G_allocate_cell_buf() ;
	nrows = G_window_rows() ;
	ncols = G_window_cols() ;

	G_init_cats((CELL) 0,"Intersection map",&Cats);

	for (i = 0 ; i<nrows ; i++) {

		/* G_get_map_row s'occupe des messages d'erreurs */
	      
	      G_get_map_row(CellIn,Rang_Cell,i) ;
	
	  for (j = 0 ; j<ncols ; j++) 
	    {
	      int result ;
	
		  result = CalculInter(Floue,Rang_Cell[j],&Cats,iForce,drapeau,v) ;
		  Rang_Cell[j] = (CELL) result;
	    }
	  G_put_map_row(CellOut,Rang_Cell);
	}
	
	G_close_cell(CellIn) ;
	G_close_cell(CellOut) ;
	
	if (G_write_cats(Inter,&Cats)<0) 
	  {
		printf("\n!!! Error in writting categories\n");
	  }

	G_free_cats(&Cats);
}

/* Renvoie l'index de la variable dans la Classe_Floue */
int IndexForce(
char *		NomVar ,
classe_floue *	Floue )
{
	int i=0 ;
	int flag;
	char cmp1[255] ;
	char cmp2[255] ;
	char reponse;
	char trash[256];

	if (NomVar==0) 
	  {
	    printf("\n... fuzzy variable not defined : set to %s \n",(Floue -> tab[0]) -> name);
	    return 0 ;
	  }
	
	strcpy(cmp2,NomVar);
	
/* Debut verification du nom entre */
	
	for ( i = 0 ; i < (Floue -> nb) ; i++) 
	  {
	    strcpy(cmp1,(Floue -> tab[i]) -> name) ;
	    if(strcmp(cmp1,cmp2)==0)
	      {
		return i;
	      }
	    else
	      flag=0;
	  }

/* Changement du nom de la variable */

	  if ( flag==0 && buffer_Var==NULL)
	    {
	      buffer_Var= malloc (sizeof(char)*50);/* allocation memoire pour le buffer_Var qui va contenir le nom choisi */
	      
	      printf("\n!!! Error : selected fuzzy variable name does not match ");
	      printf("\n    those included in the fuzzy variable file\n");	      
	      printf("\n   List of available fuzzy variables :\n");	    
	      for ( i = 0 ; i < (Floue -> nb) ; i++) 
		{
		  strcpy(cmp1,(Floue -> tab[i]) -> name) ;
		  printf ("%s \n",cmp1);
		}
	      printf ("--> Select another variable name ?");
	      while ((reponse!='y') || (reponse!='n'))
		{	
		  printf(" (y/n) ");
		    reponse=getchar();
		    
		    if(reponse == 'y')
		      {
			gets(trash);
			while (strcmp(cmp1,cmp2)!=0)
			  {
			    printf("    select a name in the previous list : ");
			    gets(cmp2);
			    fflush(stdin); /* retire de stdin les caract parasites */
			    for ( i = 0 ; i < (Floue -> nb) ; i++) 
			    {
			      strcpy(cmp1,(Floue -> tab[i]) -> name) ;
			      if(strcmp(cmp1,cmp2)==0)
				{
				  strcpy(buffer_Var,cmp2);
				  return i ;
				}
			    }
			  }
			break;
		      }
		    else
		      {
			if (reponse == 'n')
			  exit(0);
		      } 
		    /* Retire le dernier caract entre */
		    gets(trash);
		}
	    }
	  if( flag==0 && buffer_Var!=NULL)
	    {
	      strcpy(cmp2,buffer_Var);
	      for ( i = 0 ; i < (Floue -> nb) ; i++) 
		{
		  strcpy(cmp1,(Floue -> tab[i]) -> name) ;
		  if(strcmp(cmp1,cmp2)==0)
		    {
		      return i ;
		    }
		}
	      
	    }
	  /* fin de verification */

}


main(
int argc ,
char * argv[])
{
        char *mapset;		
	struct Option * OptVar ;
	struct Option *	OptMu ;
	struct Option *	OptClass ;
	struct Option *	OptH ;
	struct Option *	OptInter ;
	struct Option * OptForce ;
	struct Flag *	Flag1 ;
	struct Flag *  Flag2;
	struct Flag *  Flag3;
	char nom[50];
	int k=0;
      


	/* Initialisation de Grass */
	G_gisinit(argv[0]);

	/* Initialisation des structures pour le parser de ligne de commande */
	
	    OptForce =	        G_define_option();
	    OptVar =	        G_define_option();
	    OptMu =		G_define_option();
	    OptClass =	        G_define_option();
	    OptH =		G_define_option(); 
	    OptInter =	        G_define_option();
	    Flag3 =             G_define_flag() ;
	    Flag1 =		G_define_flag() ;
	    Flag2 =             G_define_flag() ; 
	  
       /* Demande a l utilisateur d entrer le nom d un raster fuzzifié*/
	    mapset = G_ask_old("",nom,"fuzzy","raster");
	    
	    if(mapset==NULL)
	      exit(0);  
   

	    /* Definition des options */
	    OptVar -> key		= "set" ;
	    OptVar -> description	= "Name of a specific membership function (mu and class maps only)" ;
	    OptVar -> type		= TYPE_STRING ;
	    OptVar -> required	= NO ;
                                                                                                                          
	    OptMu -> key		= "mu" ;
	    OptMu -> description	= "Output raster map of membership function values" ;
	    OptMu -> type		= TYPE_STRING ;
	    OptMu -> required	= NO ;

	    OptClass -> key		= "class" ;
	    OptClass -> description	= "Output class raster map" ;
	    OptClass -> type	= TYPE_STRING ;
	    OptClass -> required	= NO ;

	    OptH -> key		= "h" ; 
	    OptH -> description	= "Threshold (class map only) [0.5] " ; 
	    OptH -> type		= TYPE_DOUBLE ;
	    OptH -> required	= NO ; 

	    OptInter -> key		= "inter" ;
	    OptInter -> description	= "Output intersection raster map" ;
	    OptInter -> type	= TYPE_STRING ;
	    OptInter -> required	= NO ;

	    OptForce -> key		= "var" ;
	    OptForce -> description	= "Name of a specific fuzzy variable" ;
	    OptForce -> type	= TYPE_STRING ;
	    OptForce -> required	= NO ;

	    Flag1 -> key		= 'p' ;
	    Flag1 -> description	= "Print content of fuzzy variable file " ;

	    Flag2 -> key                = 'z';
	    Flag2 -> description        = "Calculate with zero data values "; 
	    
	    Flag3 -> key                = 'i';
	    Flag3 -> description        = "Treat only intersection areas (intersection map only) ";

	    if (G_parser(argc,argv)) 
	      {
		exit(1) ;
	      }
	    
	    if (Flag1 -> answer) 
	      {
		classe_floue *	Floue ;
		Floue = charge_fichier_flou(nom) ;
		Print_FuzzyClass(Floue) ;
	      }
	    
	    if (OptMu -> answer) 
	      {
		CarteMu(nom,OptMu -> answer,OptVar -> answer,
			OptForce -> answer,Flag2->answer);
	      }

	    if (OptClass -> answer) 
	      {
		CarteClass(nom,OptClass -> answer,OptVar->answer,
			   OptH->answer, OptForce -> answer,Flag2->answer);
	      }


	    if (OptInter -> answer) 
	      {
		CarteInter(nom,OptInter -> answer,
			   OptForce -> answer,Flag2->answer,Flag3->answer) ;
	      }

	    free(buffer_Var);
	    free(buffer_Set);
     
}













