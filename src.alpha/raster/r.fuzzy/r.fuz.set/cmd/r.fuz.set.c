/* r.fuzzy.set */
/* if fuzzy is given, the file is copied to the database as the fuzzy	*/
/* associated to the raster given in map				*/

#include "r.fuz.set.h"
# include"classes_floues.h"
/*-------------------------------------------------------------------------*/
int Assoc_MapFuzzy(char * Raster ,char * Fuzzy )	
/* It should use const, but const is ANSI... */

{
	int FuzzyFile ;	/* File descriptor for Fuzzy file */
	int FuzzyDest ; /* File descriptor for destination Fuzzy file */
	char * DirFuzzy ; /* emplacement du repertoire fuzzy */
	DIR * Dir ; /* structure du repertoire */

	printf("\n");
	printf("Raster map name : %s\n",Raster); 
	printf("Fuzzy variable filename : %s\n",Fuzzy); 

	/*** Verifie l'existence du fichier raster ***/
	if(!G_find_file("cell",Raster, G_mapset())) 
	  {
	     printf("\n!!! Error : raster map not found\n");
	     exit(1);
	  } 
	else 
	  {
	     printf("\n... raster map found\n");
	  }

	/*** Verifie la validite du fichier Fuzzy en entree ***/
	if ( (FuzzyFile=open(Fuzzy,O_RDONLY))==-1) 
	  {
	     printf("\n!!! Error : fuzzy variable file not found\n");
	     exit(1);
	  } 
	else 
	  {
	     printf("\n... fuzzy variable file found\n");
	  }
	close(FuzzyFile) ;

	/*** Le fichier existe, il est copie dans le sous repertoire	***/
	/*** fuzzy du Mapset. On commence par ouvrir le fichier source	***/
	if ( (FuzzyFile=open(Fuzzy,O_RDONLY))==-1) 
	  {
	     printf("\n!!! Error in opening fuzzy variable file\n");
	     exit(1);
	  }

	/* Teste l'existence d'un sous-repertoire flou du raster */
	DirFuzzy = (char *) malloc(strlen(G_location_path())+1+strlen(G_mapset())+strlen("/fuzzy"));
	strcpy(DirFuzzy,G_location_path());
	strcat(DirFuzzy,"/");
	strcat(DirFuzzy,G_mapset());
	strcat(DirFuzzy,"/fuzzy");



	if (! (Dir = opendir(DirFuzzy)) ) 
	  {
	     printf("\n... creating directory fuzzy in current mapset\n");
	     if ( mkdir(DirFuzzy,S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH ) ) 
	       {
		 printf("\n!!! Error : unable to create directory fuzzy in current mapset\n");
		 exit(1);
	       }

	  } 
	else 
	  {
	     closedir(Dir) ;
	  }

	/*** Ouvre le fichier destination en ecriture, s'il existe il	***/
	/*** est ecrase							***/
	if ( (FuzzyDest=G_open_new("fuzzy",Raster))==-1) 
	  {
	      printf("\n!!! Error in creating fuzzy variable file\n") ;
	      exit(1) ;
	  }

	/*** On appelle la procedure de copie				***/
	if (!Copy_File(FuzzyFile,FuzzyDest)) 
	  {
	     printf("\n!!! Error in writing fuzzy variable file\n");
	     exit(1) ;
	  }
}

/* Inter permet d`avoir la liste des raster */

void inter (char *name)
{
  char* mapset;
  mapset = G_ask_in_mapset("",name,"cell","raster");
  if(mapset==NULL)
    exit(0);

} 

   

/*-------------------------------------------------------------------------*/
/* Affichage avec graph_grass, dans la fenetre graphique de Grass	***/


/*** Affichage d'une variable floue sous Grass ***/
void Display_FuzzyVar_Grass( varia_floue * FuzzyVar )
{
 
  /*  Noeud contient le premier des nombres flous de la variable */
	
       node * Noeud ;

   /* Allocation pour l'affichage des points */
	
       float * X, * Y ;

   /* La structure graphique pour l'affichage avec graph_grass */
	
       struct Graphique * Graph ;


   /* Initialisation de la structure Graphique -> On cree un nouveau graphique */
	if ( (Graph=Create_Graph(FuzzyVar -> name)) ) 
	  {

   /* On initialise pour un maximum de MAX_VAR_DISPLAY courbes (pour coherence avec GrafGKS */
		
	  struct Courbe * C[10] ;
	  struct Courbe * C1 ;
	  struct node *	Noeud ;
	  int		i = 0 ;
	  int		j ;
	  int		nb_pts ;
	

  /*  Noeud contient le premier des nombres flous de la variable */
		
	  Noeud = FuzzyVar -> liste_elem ;
	 
  /* On teste tous les noeuds */

	  while (Noeud != NULL) 
	    {
	      
	      switch ( (Noeud -> nombre).type ) 
		  {

		    /* allocation memoire pour la forme triangle */
		        case trian :
			  {  
			    X = (float *) malloc(3*sizeof(float)) ;
			    Y = (float *) malloc(3*sizeof(float)) ;
			    if (X==NULL || Y==NULL) 
			      {
	       			printf("\n!!! Error in allocating memory for TFN curve\n");
	       			exit(1);
			      }
			    
			    X[0] = (Noeud -> nombre).classe.tri[0];
			    X[1] = (Noeud -> nombre).classe.tri[1];	    
			    X[2] = (Noeud -> nombre).classe.tri[2];
			    Y[0] = Y[2] = 1 ;
			    Y[1] = 0 ;
			    nb_pts = 3 ;
			    break ;
			  }
		    /* allocation memoire pour la forme trapeze */    
			
		        case trap :
			  {
			    X = (float *) malloc(4*sizeof(float)) ;
			    Y = (float *) malloc(4*sizeof(float)) ;
			    if (X==NULL || Y==NULL) 
			      {
				printf("\n!!! Error in allocating memory for TRFN curve\n");
				exit(1);
			      }
			    X[0] = (Noeud -> nombre).classe.tra[0];
			    X[1] = (Noeud -> nombre).classe.tra[1];
			    X[2] = (Noeud -> nombre).classe.tra[2];
			    X[3] = (Noeud -> nombre).classe.tra[3];
			    Y[0] = Y[3] = 1 ;
			    Y[1] = Y[2] = 0 ;
			    nb_pts = 4 ;
			    
			    break ;
			  }
		    /* allocation memoire pour la forme LR */
		        case lr :
			  {
			    X = (float *) malloc(NB_POINTS_LR*sizeof(float)) ;
			    Y = (float *) malloc(NB_POINTS_LR*sizeof(float)) ;
			    if (X==NULL || Y==NULL) 
			      {
				printf("\n!!! Error in allocating memory for LR curve\n");
				exit(1);
			      }
			    
			    for ( j = 0 ; j < ((NB_POINTS_LR-1)>>1) ; j++ ) 
			      {
				X[j] = (Noeud -> nombre).classe.L[0]+
				  (float) j *((Noeud -> nombre).classe.L[1]-(Noeud ->nombre).classe.L[0])/((NB_POINTS_LR-1)>>1) ;
				Y[j] = 1-appartient_LR(&(Noeud -> nombre),X[j]);
			      }

			    X[(NB_POINTS_LR-1)>>1] = (Noeud -> nombre).classe.L[1] ;
			    Y[(NB_POINTS_LR-1)>>1] = 0.0 ;
			    
			    for ( j = 1 ; j <= ((NB_POINTS_LR-1)>>1) ; j++ ) 
			      {
				X[j+((NB_POINTS_LR-1)>>1)] =(Noeud -> nombre).classe.L[1]+(float) j *((Noeud -> nombre).classe.L[2]-(Noeud -> nombre).classe.L[1])/((NB_POINTS_LR-1)>>1) ;

				Y[j+((NB_POINTS_LR-1)>>1)] = 1 -appartient_LR(&(Noeud -> nombre),X[j+((NB_POINTS_LR-1)>>1)]);
			      }
			   
			    nb_pts =NB_POINTS_LR;

			    break ;
			  }

		    case lrg :
		      {
			X = (float *) malloc(NB_POINTS_LR*sizeof(float)) ;
			Y = (float *) malloc(NB_POINTS_LR*sizeof(float)) ;
			if (X==NULL || Y==NULL) 
			  {
			    printf("\n!!! Error in allocating memory for LRG curve\n");
			    exit(1);
			  }
			
			for ( j = 0 ; j < ((NB_POINTS_LR-1)>>1) ; j++ ) 
			  {
			    X[j] = (Noeud -> nombre).classe.Lrg[0]+
			      (float) j *((Noeud -> nombre).classe.Lrg[1]-(Noeud ->nombre).classe.Lrg[0])/((NB_POINTS_LR-1)>>1) ;
			    Y[j] = 1-appartient_LRG(&(Noeud -> nombre),X[j]);
			  }
			
			X[(NB_POINTS_LR-1)>>1] = (Noeud -> nombre).classe.Lrg[1];
			Y[(NB_POINTS_LR-1)>>1] = 0.0 ;
			X[((NB_POINTS_LR-1)>>1)+1] = (Noeud -> nombre).classe.Lrg[2] ;
			Y[((NB_POINTS_LR-1)>>1)+1] = 0.0 ;
			
			for ( j = 2 ; j <= ((NB_POINTS_LR-1)>>1) ; j++ ) 
			  {
			    X[j+((NB_POINTS_LR-1)>>1)] =(Noeud -> nombre).classe.Lrg[2]+(float) j *((Noeud -> nombre).classe.Lrg[3]-(Noeud -> nombre).classe.Lrg[2])/((NB_POINTS_LR-1)>>1) ;
			    
			    Y[j+((NB_POINTS_LR-1)>>1)] = 1 -appartient_LRG(&(Noeud -> nombre),X[j+((NB_POINTS_LR-1)>>1)]);
			  } 
			   
			nb_pts =NB_POINTS_LR;
			
			break ;
		      }
		      
		  default :
		    {
		      printf("\n!!! Error : unknown fuzzy set type, must be TFN, TRFN, LR or LRG\n");
		      exit(1);
		    }
		  }

			/* Initialisation d'une courbe */
		        /* C[i] = Create_Drawing devrait marcher maintenant */ 

		       C1 = Create_Drawing(X,Y,nb_pts,(Noeud -> nombre).name,i+2);
		 
			C[i] = C1 ; 
			/* On ajoute la courbe au graphique courant */
			 Graph = Add_To_Graph(Graph,C1);
		       

			if (Graph == NULL) 
			  {
			    /* La courbe n'a pas pu etre allouee */
			    printf("\n!!! Error : unable to draw the graph\n");
			  }
		
			/* On passe au noeud suivant */
		
			i++;
			Noeud = Noeud -> suiv ;

			free(X);
			free(Y);
		       
	    }

	  /* Affichage du graphique */
	  Display_Graph_Grass(Graph) ;

	  /* On libere la memoire utilisee par la structure */

	  Destroy_Graph(Graph) ; 
       
	  }
	else 
	  {
	     /* La structure graphique n'a pas ete cree */

	     printf("\n!!! Error : unable to create the graph\n");
	  }

}



/* ------------------------------------------------------------------------ */
/* Affichage d'une classe floue sur l'ecran graphique de Grass */
void Display_FuzzyClass_Grass(classe_floue * Floue )


{
 
    int nb = 0;
    int	i = Floue -> nb; 
    int	choix = 0; 
    
   	 /* Choix entre le COLOR_TABLE_FIXED et le COLOR_TABLE_FLOAT */

#ifdef COLOR_TABLE_FIXED
	R_color_table_fixed();
#else
	R_color_table_float();
#endif  
	R_reset_color(0,0,0,0);
	R_color(0);
	R_erase();


        choix = 1 ; 
	printf(" \n");
	printf("Left button to browse variables\n");
	printf("Right button to quit\n");
	while (choix!=0) 
	  {
	  int Mx,My,Button;
	  /* Puis affichage de la variable actuellement selectionnee */

	  Display_FuzzyVar_Grass(Floue -> tab[choix-1]) ;
	  
	  /* Scan de la souris */
	       
	     R_get_location_with_pointer(&Mx,&My,&Button);
      
	  /* Change la selection ou sort en fonction de la souris */
	
	     switch (Button) 
	       {

	       case 1 :
		 {
		   if (choix<=i)
		     {   
			  if (choix==i) 
			    {
			      choix=1;
			      R_color(0);
			      R_erase(); 
			      break;
			    }
			  else 
			    {
			      choix ++ ; 
			      R_color(0);
			      R_erase(); 
			      break ;
			  }
		     }
		 }
	       default : choix = 0 ;
		  break; 
	       }
	  }







}

/*-------------------------------------------------------------------------*/
/* Appelle les fonctions d'affichage ou de dessin pour une variable floue */

int Visu_Variables(char *Raster ,int Flags)

{
  /* Load the fuzzy file */

  FILE * FuzzyFile ;
  erreur err = 0 ;
  classe_floue * Floue ;
  

  /* charge_fichier_flou() defini dans commun.h */
 
  Floue = charge_fichier_flou(Raster) ;

  if (err) 
    {
       /* Il faudrait appeller le traitement de l'erreur de libflou */
       printf("\n!!! Error in reading fuzzy variable file\n");
       exit(1);
    }

  /* En fonction des flags, appelle les differentes fonctions	*/
  /* d'affichage des variables					*/

  if (Flags&4) Print_FuzzyClass(Floue) ;

  if (Flags&1) Display_FuzzyClass_Grass(Floue);
  

}
/* --------------------------------------------------------------------------- */
main(argc,argv,name)

int	argc ; 
char *	argv[];
char * name;
{
      struct Option * OptRaster ; 
      struct Option * OptFuzzy ;
      struct Flag *	Flag1 ;
      struct Flag *	Flag3 ;
      int TotalFlag = 0 ; 
        
      /* Initialisation de Grass */
      G_gisinit(argv[0]); 
      
      /* Communication avec le driver */
      R_open_driver();
      /* Effacement de l'ecran */

      R_color(0);
      R_erase();

      /* Initialistion des parametres de texte */
      R_font("romans");
      R_text_size(10,10);

      /* Initialise les couleurs pour l'affichage des courbes */
      R_reset_color(255,255,255,1);
      R_reset_color(000,000,255,2);
      R_reset_color(100,150,255,3);
      R_reset_color(000,255,000,4);
      R_reset_color(180,255,100,5);
      R_reset_color(255,000,000,6);
      R_reset_color(255,100,100,7);
      R_reset_color(255,255,255,8);
      R_reset_color(255,100,000,9);
      R_reset_color(100,255,000,10);
      R_reset_color(000,255,100,11);
      R_reset_color(000,100,255,12); 

      
      R_color(1); 



	  /* Initialisation des structures pour le parser de ligne de commande */
	  
	  OptRaster = G_define_option(); 
	  OptFuzzy = G_define_option();
	  Flag1 = G_define_flag() ;
	  Flag3 = G_define_flag() ; 

	  OptRaster -> key           = "map" ;
	  OptRaster -> description   = "Input raster map" ;
	  OptRaster -> type	     = TYPE_STRING ;
	  OptRaster -> required	     = YES ; 

	  OptFuzzy -> key         = "file" ;
	  OptFuzzy -> description = "Input fuzzy variable file" ;
	  OptFuzzy -> type	  = TYPE_STRING ;
	  OptFuzzy -> required    = NO ;

	  Flag1 -> key		= 'd' ;
	  Flag1 -> description	= "Display membership functions in grass current display " ;
	  Flag3 -> key		= 'p' ;
	  Flag3 -> description	= "Print content of fuzzy variable file " ;
 
	  if(G_parser(argc,argv)) 
	    {
	      exit(1) ;
	    }
	  
	  if (OptFuzzy -> answer) 
	    {
	      Assoc_MapFuzzy(OptRaster -> answer, OptFuzzy -> answer) ;
	      } 
    
    
	  /* TotalFlag contient en un entier les infos de tous les flags,*/
	  /* c'est plus simple a gerer */

	  if (Flag1 -> answer) TotalFlag += 1;
	  if (Flag3 -> answer) TotalFlag += 4 ;
  
	  if (TotalFlag!=0) 
	    {
	      Visu_Variables(OptRaster -> answer,TotalFlag) ;
	    } 
	
	/* Ferme le driver Grass */
	 
	  R_close_driver();  
}
