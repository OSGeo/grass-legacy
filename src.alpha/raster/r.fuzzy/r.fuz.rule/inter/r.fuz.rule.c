#include "r.fuz.rule.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>
#include "gis.h"
#include "fichiers.h"
#include "commun.h"
#include "fuzerreur.h"

/* Initialise une classe floue */
struct classe_floue * Init_CFloue()
{
	struct classe_floue * CFloue ;

	CFloue = (classe_floue *) malloc ( sizeof(classe_floue) ) ;
    
	if (!CFloue) 
	  {
	    /* Impossible d'allouer la memoire */
	    Erreur(2,0,NULL);
	    exit(1);
	  }

	CFloue -> nb = 0 ;
	CFloue -> regle = NULL ;
	CFloue -> var_sortie = NULL ;

	return CFloue ;

}

/* Lit les variables contenues dans la regles et les met dans un tableau */
void Lire_variables(char * Fichier_Regles,
		    char ** variables, 
		    int nb_rast)
{
  FILE * Fregles;
  char carac;
  int i,j;
  int nb_ligne;
  char * nom;
  erreur * err;
  char * var;
  char * VAR[20];
  
  if((Fregles= fopen(Fichier_Regles,"r"))==NULL)
    {
      /* Impossible d ouvrir le fichier des regles*/
      Erreur(4,0,Fichier_Regles);
      exit(1);
    }
  nb_ligne=1;
  carac=lire(Fregles);

  /* Saute les commentaires */
  while(carac=='#')
    {
	  nb_ligne++;
	  carac=lire(Fregles);
    }

  i=0;
  carac = lireblanc1(Fregles,carac,&nb_ligne);
  var=lirenom(Fregles,&carac,err);
  carac=lire(Fregles); 
  carac = lireblanc1(Fregles,carac,&nb_ligne);
  j=0;

  while(carac != '\n')
    {
      carac=lire(Fregles);
      if((carac!=' ') && (carac != '	') && (carac!=')') && (carac!='('))
	{
	  var=lirenom(Fregles,&carac,err);
	  if(strcmp(var,"AND") && strcmp(var,"OR") &&
	     strcmp(var,"XOR") && strcmp(var,"NOT") && strcmp(var,"THEN"))
	  { 
	    /* Recuperation des noms des varaibles */
	    variables[j]=(char*)malloc(sizeof(char)*(strlen(var)+1));
	    if(variables[j]==NULL)
	      {
		/* Probleme allocation memoire */
		Erreur(3,0,NULL);
		exit(1);
	      }
	    strcpy(variables[j],var);
	    j++;
	    
	  }
	}
 
    }
  if(j!=(nb_rast+1))
    {
      /* nombre de raster different du nombre de variable */ 
      Erreur(5,0,NULL);
   
      exit(1);
    }
}

/* Creer le fichier de variables communes */
void Lire_Fichier_Rasters( char ** Rasters ,
			   char * Sortie,
			   classe_floue * Floue, 
			   int nb_rast,
			   char ** var)
{
	FILE * FVarSortie ;
	FILE * FuzzyFile ;
	char * nom_var[10];
	int v=0;
	int w=0 ;
	int x=0;
	int j=0;
	int k=0;
	char Temp ;
	int err=0;
	struct classe_floue *  CRasters[MAX_OPEN_FILE];
	int n_ligne=0;

	while ((v<nb_rast)) 
	  {	   
	    printf("\n... opening fuzzy variable file %s \n ",Rasters[v]);   
	    /*	    printf("Ouverture des fichiers :\n"); */
	    /*	    printf("%s\n",Rasters[v]);            */
	    
	    if ( (FuzzyFile = G_fopen_old("fuzzy",Rasters[v],G_mapset())) == NULL ) 
	      {
		/* Impossible d ouvrir le fichier raster */
		Erreur(41,0,Rasters[v]);
		exit(1);
	      }
	    affiche_erreur(test_floue(FuzzyFile,&n_ligne),n_ligne,NULL);
	    fclose(FuzzyFile);
	    FuzzyFile = G_fopen_old("fuzzy",Rasters[v],G_mapset());
	    
	    CRasters[v]= lecture_nb_floues(FuzzyFile,&err) ;
	    affiche_erreur(err,0,NULL);
	    fclose(FuzzyFile) ;
	    v++;
	  }

	 if ( (FVarSortie = fopen(Sortie,"r"))==NULL ) 
	   {
	     /* Impossible d ouvrir le fichier de variable de sortie */
	     Erreur(42,0,Sortie);

	     exit(1);
	   }

	 affiche_erreur(test_floue(FVarSortie,&n_ligne),n_ligne,NULL);
	 fclose(FVarSortie);
	 
	 
	 FVarSortie = fopen(Sortie,"r");
	 CRasters[v] = lecture_nb_floues(FVarSortie,&err) ;

	 affiche_erreur(err,0,NULL);
	 

	/* Recupere toutes les informations sur les fichiers flous associes au Rasters et les place dans une seule varaible floue Floue de type classe_floue */

	 for(w=0;w<=nb_rast;w++)
	  {
	    for(x=0;x<CRasters[w]->nb;x++)
	      {
		nom_var[j]= (char*) malloc(sizeof(char)*strlen((CRasters[w]->tab[x])->name)+1);
	        if(nom_var[j]==NULL)
		  {
		    /* Probleme allocation memoire */
		    Erreur(3,0,NULL);
		    exit(1);
		  }
		if(j<(nb_rast+1)) /* evite de depasser le tableau de variable */
		  {
		    if(strcmp(var[j],(CRasters[w]->tab[x])->name)==0)
		      {
			strcpy(nom_var[j],((CRasters[w]->tab[x])->name));
			(Floue->tab[j])=cree_variable_floue(nom_var[j]);
			
			/* Associe a chaque varaibles floues ses fonctions d'appartenances */
			(Floue->tab[j])->liste_elem=(CRasters[w]->tab[x])->liste_elem;
			j++;
		      }
		  }
	      }
	  } 
	
	Floue->nb=j;
  
/* test pour savoir si les variables sont reinjectees */
	x=0;
	w=0;
	while(Floue->tab[w] != NULL)
	  {
	    while(Floue->tab[x] != NULL)
	      {
		if(strcmp((Floue->tab[w])->name,(Floue->tab[x])->name)==0 && x!=w)
		  {
		    /* Les fichiers de varibles floues associes aux rasters contenient les meme varaibles */
		    Erreur(6,0,NULL);
	
		    exit(0);
		  }
		x++;
	      }
	    x=0;
	    w++;
	  }

	fclose(FVarSortie) ;
}

/* Lit les variables utilisees et les met forme une pile avec les variables et les operateyrs booleens */
void  Lire_Regles( char * Fichier_Regles ,
		   classe_floue * classe,
		   pile * post,
		   list_regle ** regles)
{
	FILE * FRegles ;
	pile  pi;
	char * ch_err;
  	erreur  err;
	int n_ligne;
       

	ch_err=(char*) malloc(LONGMOT*sizeof(char));
	if(ch_err==NULL)
	  {
	    /* Probleme allocation memoire */
	    Erreur(1,0,NULL);
	    exit(1);
	  }
	
	if ( (FRegles = fopen(Fichier_Regles,"r"))==NULL ) 
	  {
	    /* Impossible d ouvrir le fichier de regles */
	    Erreur(4,0,Fichier_Regles);

	    exit(1);
	  }
	
	n_ligne=lecture_regle(FRegles,&pi,classe,regles,&err,&ch_err);
    
	affiche_erreur(err,n_ligne,ch_err);
	fclose(FRegles);
	free(ch_err);
	/* transforme la pile in-fixee en pile post-fixee pour le calcul du DOF */
	*post=inf_post(pi);
}

void Traitement_donnees(classe_floue * classe,
			pile * post,
			list_regle ** regles,
			char * p_Sortie,
			char ** Raster,
			int  config, 
			int  defuz,
			int comb,
			char * Mu,
			char * Dist,
			int drapeau)
{
  CELL * Rang_Cell[MAX_OPEN_FILE];
  CELL * Rang_Cell_Out;
  CELL * Rang_Cell_mu_max;
  CELL * Rang_Cell_dist;
  struct Cell_head region;
  char reponse;
  char trash[256];
  int RasterFile[MAX_OPEN_FILE];
  int j=0;
  int i=0;
  int nrows;
  int ncols;
  int x,y;
  int num_var_sortie=0;
  int nb_var=0;
  int Cell_Sortie=0;
  int Cell_mu_max=0;
  int Cell_dist=0;
  int var_debut,var_fin;
  int visu=0;
  int passage=0;
  int index_passage=0;
  double mu_sortie=0.0;
  double mu_max=0.0;
  double Indice=0.0;
  double coordx=0.0;
  double coordy=0.0;
  list_regle * pteur;
  sol * Result;
  discret tab_discret;
  discret cur_ptr=NULL;

  nb_var=classe->nb_var_entree;

  G_get_window(&region);
  
  /* Ouverture de tous les rasters */
  while (Raster[j] && j<nb_var)
    {
      printf("\n... opening raster map %s\n",Raster[j]);
      if((RasterFile[j]= G_open_cell_old(Raster[j],G_mapset()))<0)
	{
	  /* Impossible d ouvrir le raster */
	  Erreur(43,0,Raster[j]);
	  exit(1);
	}
      Rang_Cell[j] = G_allocate_cell_buf();
      j++;
    }


  /* Teste la non existence du fichier destination */
  /* Ou permet d'utiliser le meme nom */
  if ( G_find_file("cell",p_Sortie,G_mapset())) 
    {
      printf("\n %s - already exists,ok to overwrite? ",p_Sortie);
      while ((reponse!='y') || (reponse!='n'))
	{	
	  printf(" (y/n): ");
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
  if((Cell_Sortie=G_open_cell_new(p_Sortie))<0)
    {
      /* Impossible d ouvrir le fichier de sortie */
      Erreur(43,0,p_Sortie);
      exit(1);
    }

  if(Mu != NULL)
    {
      if((Cell_mu_max=G_open_cell_new(Mu))<0)
	{
	  /* Impossible d ouvrir le fichier ind1 de sortie */
	  Erreur(43,0,Mu);
	  exit(1);
	}
    }

  if(Dist != NULL)
    {
      if((Cell_dist=G_open_cell_new(Dist))<0)
	{
	  /* Impossible d ouvrir le fichier ind2 de sortie */
	  Erreur(43,0,Dist);
	  exit(1);
	}
    }

  /* Allocation memoire pour les buffers */
  Rang_Cell_Out = G_allocate_cell_buf();
  if(Mu != NULL)
  Rang_Cell_mu_max = G_allocate_cell_buf();
  if(Dist != NULL)
  Rang_Cell_dist = G_allocate_cell_buf();

  nrows=G_window_rows();
  ncols=G_window_cols();

  j=0;
  for(y=0;y<nrows;y++)
    {
      /* G_get_map_row s'occupe des messages d'erreurs */
      /* On charge un rang sur tous les fichiers ouverts */
      for(i=0;i<nb_var;i++)
	{
	  G_get_map_row(RasterFile[i],Rang_Cell[i],y);
	}
      for(x=0;x<ncols;x++)
	{
	  index_passage=0;
	  mu_max=0.0;
	  Indice=0;
	  for(i=0;i<nb_var;i++)
	    {
         
	      /* Place la valeur du pixel lu dans la variable val_sortie de chaque fonction d appartenace */
	      num_var_sortie=classe->ench_var[i]; 
	      classe->tab[num_var_sortie]->val_sortie=Rang_Cell[i][x];
	      pteur=(*regles);

	      if(drapeau==0 && Rang_Cell[i][x]!=0.0) /* Mode zero=no_data */
		{
		  while(pteur != NULL)
		    {
		      /* Place la valeur lu du raster dans la variable valeur de chaque fonction d apparteance en fonction de la variable et du raster associe*/
		      pteur->reg_act[i].valeur=Rang_Cell[i][x];
		      pteur=pteur->suiv;
		    }
		  passage=1;
		}
	      if(drapeau==0 && Rang_Cell[i][x]==0.0)
		{
		  passage=0;
		}

	      if (drapeau==1)/* Mode prise en compte des zero */
		{
		  while(pteur != NULL)
		    {
		      /* Place la valeur lu du raster dans la variable valeur de chaque fonction d apparteance en fonction de la variable et du raster associe*/
		      pteur->reg_act[i].valeur=Rang_Cell[i][x];
		      pteur=pteur->suiv;
		    } 
		  passage=1;
		}
	      /* Permet de savoir si un des pixels des rasters est nul */
	      /* index_passage est sup a 1 si aucun pixel est nul */
	      index_passage=index_passage+passage;
	    }
	  
	  if(index_passage == nb_var) /* Si aucun raster possede de no_data */
	    {
	     
	      Result = Calcul_DOF(post,(*regles),config,classe);
	      if (Result == NULL)
		{
		  coordy=G_row_to_northing((double)y+0.5,&region);
		  coordx=G_col_to_easting((double)x+0.5,&region);

		  printf("!!! Warning : no valid rule in point x=%f  y=%f (i=%d j=%d)\n",coordx,coordy,x,y);

		  /* printf("Aucune regles s applique sur pour le point de coordonnees:\n" );  */
		  /* printf("%f x, %f y  ",coordx,coordy);                                     */
		  /* printf(" (%d i, %d j) \n",x,y);                                           */
		  mu_sortie=0.0;
		}
	      else
		{
		  var_debut=classe->nb_var_entree;
		  var_fin=classe->nb_var_utilise;
		  for(i=var_debut;i<=var_fin;i++)
		    {
		    
		      mu_sortie=combi_deflou(Result,classe,defuz,comb,visu,i,&tab_discret);
		     
		      /* Calcul pour la mesure du flou les mu max */
		      /* Debut */
		      
		      if(Mu != NULL)
			{
			  cur_ptr=tab_discret;
			  while(cur_ptr != NULL)
			    {
			      /* Test si valeur sup a la precedente */
			      if(mu_max <= cur_ptr->y )
				{
				  mu_max = cur_ptr->y;
				}
			      cur_ptr=cur_ptr->suiv;
			    }

			}
		      /* fin */
		      
		      /* Calcul de l'indice du flou */
		      
		      if(Dist != NULL)
			{
			  Indice=Indice_flou(tab_discret);
			}

		      /* Liberation de la memoire */
		      LibereDiscret(tab_discret);
		      LibereSolution(Result);
		    } 
		}
	    }
	  else /* Si un des rasters possede des no_data */
	    {
	      mu_sortie=0.0;
	      mu_max=0.0;
	      Indice=0.0;
	    }

	  /* printf("%d x %d y %d x*y \n",x,y,x*y); */
	  /* Mise en buffer des resultats */

	  Rang_Cell_Out[x] = (int) (mu_sortie);

	  if(Mu != NULL)
	    Rang_Cell_mu_max[x] = (int) (mu_max*100);

  	  if(Dist != NULL)
	    Rang_Cell_dist[x] = (int)(Indice*Const); 
	  /* Const est defini dans r.fuz.rule.h et vaut 1 */
 
	}/* fin for x */

      /* Place ligne par ligne les valeurs dans les raster de sortie */
 
      G_put_map_row(Cell_Sortie,Rang_Cell_Out);

      if(Mu != NULL)
	G_put_map_row(Cell_mu_max,Rang_Cell_mu_max);

      if(Dist != NULL)
	G_put_map_row(Cell_dist,Rang_Cell_dist);
    }

/* Fermeture des rasters */

  for (i=0;i<nb_var;i++)
    G_close_cell(RasterFile[i]);
 
  G_close_cell(Cell_Sortie);

 if(Mu != NULL)
     G_close_cell(Cell_mu_max);

 if(Dist != NULL)
     G_close_cell(Cell_dist);

/* Liberation memoire pour les buffers image */

  for(j=0;j<nb_var;j++)
  free(Rang_Cell[j]);
  free(Rang_Cell_Out);

 if(Mu != NULL)
    free(Rang_Cell_mu_max);

 if(Dist != NULL)
    free(Rang_Cell_dist);

      
}

sol * Calcul_DOF(pile * post,
		 list_regle * regles,
		 int config,
		 classe_floue * classe)
{
  list_regle * pteurRegle=NULL;
  int rep=0;
  sol * solution;
  sol * nouv;
  int sauv=0;
  
  pteurRegle=regles;
  solution=NULL;
  while(pteurRegle != NULL)
    {
      sauv=(*post)->tete;
      rep=eval_bool((*post),pteurRegle->reg_act,classe);
      (*post)->tete=sauv;
      if(rep==1)
	{
	  nouv = (sol*) malloc(sizeof(sol));
	  /* realise une liste chainee */
	  nouv->suiv = solution;
	  strcpy(nouv->name,pteurRegle->reg_act[classe->nb_var_utilise-classe->nb_var_sortie+1].name);

	  sauv =(*post)->tete;
	  nouv->dof = Eval_dof(post,pteurRegle->reg_act,config,classe); 
	  nouv->num_regle=pteurRegle->num;
	  nouv->regle = pteurRegle;
	  (*post)->tete = sauv;
	  solution=nouv;
	}
      pteurRegle=pteurRegle->suiv;
    }
  return(solution);
}

double Eval_dof( pile * post,
		 info * reg_act,
		 int config,
		 classe_floue * classe)
{

  operateur op;
  double result=0.0;
  double result1=0.0;
  double retour=0.0;
  node * pteur;
  int rep;
  
  op=depiler(*post);
  switch(op)
    {
    case NOT: result = Eval_dof(post,reg_act,config,classe);
      return(0.0);
      break;
    case AND : 
      result = Eval_dof(post,reg_act,config,classe);
      result1 = Eval_dof(post,reg_act,config,classe);
      if(config==1)
	return(min(result,result1));
      else
	return(result*result1);
      break;
    case OR : result = Eval_dof(post,reg_act,config,classe);
      result1 = Eval_dof(post,reg_act,config,classe);
      if(config==1)
	return(max(result,result1));
      else
	return(result+result1-result*result1);
      break;
    case XOR :  result = Eval_dof(post,reg_act,config,classe);
      result1 = Eval_dof(post,reg_act,config,classe);
      if(config==1)
	return (max(min(1-result1,result),min(result1,1-result)));
      else
	return(result+result1-2*result*result1);
      break;
    default :
      pteur=classe->tab[reg_act[op-10].num_var_sortie]->liste_elem;
      
      rep=0;
      /* Calcul du mu pour chaque valeur des rasters */
      while((pteur != NULL) && (rep==0))
	{
	  if (strcmp(reg_act[op-10].name,pteur->nombre.name)==0)
	    {
	      rep=1;
	      /* Calcul des mus pour chaque valeur des rasters */
	      switch(pteur->nombre.type)
		{
		case 0 :
		  /* op-10 correspond au numero de la variable dans la pile */

		  retour=appartient_triangle(&(pteur->nombre),reg_act[op-10].valeur);

		  return(retour);
		case 1 :
		  /* op-10 correspond au numero de la variable dans la pile */

		  retour=appartient_trapeze(&(pteur->nombre),reg_act[op-10].valeur);

		  return(retour);
		case 2 :
		  /* op-10 correspond au numero de la variable dans la pile */

		  retour=appartient_LR(&(pteur->nombre),reg_act[op-10].valeur);

		  return(retour);
		case 3 : 
		  /* op-10 correspond au numero de la variable dans la pile */

		  retour=appartient_LRG(&(pteur->nombre),reg_act[op-10].valeur);

		  return(retour);
		}
	    }
	  pteur=pteur->suiv;
	}
      printf("\n!!! Error :  pointer on op %i did not achieve in the search \n",op-10);
      exit(0);
      break;
    }

}

double Indice_flou( discret tab_dis )
{
  double somdb=0.0;
  double som = 0.0;
  double sominter=0.0;
  double result = 0;
  discret ptr_tab; 
  int i=0;

  ptr_tab=tab_dis;
  while(ptr_tab != NULL)
    {
      if(ptr_tab->y>0.0)
	{
	  sominter=((ptr_tab->y)-(1.0-(ptr_tab->y))); /* Calcul de la somme des |mu(x)-(1-mu(x))| */

	  if(sominter<0)
	    sominter=sominter*-1;
	  else
	    sominter=sominter;
	  som=som+sominter;
	  i++;
	}
      ptr_tab=ptr_tab->suiv;
    }

  
  result=1.0-((1.0/((double)i))*som);/* Calcul de (1-((1/Card)*som(|mu(x)-(1-mu(x))|) */
  
  return(result);
  
}

main(argc , argv)
int argc ;
char * argv[] ;
{
  char * mapset;
  struct Option *       OptVar;
  struct Option *	OptResult ;
  struct Option *       OptRule ;
  struct Option *	OptDefuz ;
  struct Option *	OptComb ;
  struct Option *	OptLog ;
  struct Option *	OptVarSortie ;
  struct Option *       OptMu;
  struct Option *       OptDist;
  struct Flag   *       Flag1;
  char * p_nom[MAX_OPEN_FILE];
  char nom[MAX_OPEN_FILE];
  int nb_rast=0;
  int j;
  struct classe_floue * Floue;
  struct list_regle * regles;
  pile Post;
  int Opt_config;
  int Opt_defuz;
  int Opt_comb;
  double val=0.0;
  int num_var_sortie=0;
  char *var_utilise[MAX_OPEN_FILE];
  
  /* Initialisation de Grass */
  G_gisinit(argv[0]);

  /* Initialisation des structures pour le parser de ligne de commande */
  OptResult =	G_define_option();
  OptRule =	G_define_option();
  OptVarSortie =G_define_option();
  OptDefuz =	G_define_option();
  OptComb =	G_define_option();
  OptLog =	G_define_option();
  OptMu =       G_define_option();
  OptDist =     G_define_option();
  Flag1 =       G_define_flag();

/* Initialise les tableaux pour les nom des rasters et des variables floues */
  for (j=0;j<MAX_OPEN_FILE;j++)
    {
      p_nom[j]=NULL;
      var_utilise[j]=NULL;
    }
/* Demande a l'utilisateur d'enter le nom d'un raster */
  
  while(((mapset!=NULL) || (nb_rast==0)) && (nb_rast<=MAX_OPEN_FILE))
    {

	  p_nom[nb_rast]=(char*)malloc(strlen(nom)+1);
	  if(p_nom[nb_rast]==NULL)
	    {
	      Erreur(7,0,NULL);
	      exit(1);
	    }
	  mapset = G_ask_old("",p_nom[nb_rast],"fuzzy","raster");
	  if (mapset == NULL && nb_rast==0)
	    {
		exit(0);
		
	    }
	  nb_rast++;
    } 

  /* Permet de n avoir que les variables en entree */
  nb_rast=nb_rast-1;

  /* Definition des options */
    
  OptRule -> key		= "rule" ;
  OptRule -> description	= "Input rule file" ;
  OptRule -> type		= TYPE_STRING ;
  OptRule -> required	= YES ;

  OptResult -> key		= "output" ;
  OptResult -> description	= "Output raster map" ;
  OptResult -> type		= TYPE_STRING ;
  OptResult -> required		= YES ;
  
  OptDefuz -> key		= "defuz" ;
  OptDefuz -> description	= "Defuzzification method : max/med/[cdg]" ;
  OptDefuz -> type	= TYPE_STRING ;
  OptDefuz -> required	= NO ;
  
  OptComb -> key		= "comb" ;
  OptComb -> description	= "Rule combination method : min/mincrest/max/maxcrest/[weightsum]";
  OptComb -> type		= TYPE_STRING ;
  OptComb -> required	= NO ;
  
  OptLog -> key		= "log" ;
  OptLog -> description	= "Logical operators : [min]/mult" ;
  OptLog -> type		= TYPE_STRING ;
  OptLog -> required	= NO ;
  
  OptVarSortie -> key		= "file" ;
  OptVarSortie -> description	= "Fuzzy variable file for output map" ;
  OptVarSortie -> type		= TYPE_STRING ;
  OptVarSortie -> required	= YES ;
  
  OptMu -> key                  = "ind1";
  OptMu -> description          = "Output fuzzy index map : membership function maximum method ";
  OptMu -> type                 = TYPE_STRING ;
  OptMu -> required             = NO;

  OptDist -> key                = "ind2";
  OptDist -> description        = "Output fuzzy index map : fuzzy measure method ";
  OptDist -> type               = TYPE_STRING ;
  OptDist -> required           = NO;

  Flag1 -> key                    = 'z';
  Flag1 -> description            = "Calculate with zero data values ";

  if (G_parser(argc,argv)) 
    {
      exit(1) ;
    }
  
  /* Place les valeurs par defaut dans config,comb,defuz pour les options OptLog, OptComb et OptDefuz */ 
  if( (OptLog->answer) == NULL || (strcmp((OptLog->answer),"min")==0) )
    Opt_config=1;/* 1 pour min */
  else
    if((strcmp((OptLog->answer),"mult")==0))
      Opt_config=2; /* 2 pour multiplication */
    else
      {
	/* Choix impossible */
	Erreur(7,0,OptLog->answer);
      }

  if( (OptDefuz->answer) == NULL ||(strcmp((OptDefuz->answer),"cdg")==0))
    Opt_defuz=2; /* 2 pour centre de gravite */
  else 
    if((strcmp((OptDefuz->answer),"med")==0))
      Opt_defuz=3;/* 3 pour mediane */
  else
    if ((strcmp((OptDefuz->answer),"max")==0))
      Opt_defuz=1; /* 1 pour maximum */
    else
      {
	Erreur(7,0,OptDefuz->answer);
      }
  

  if( (OptComb->answer) == NULL ||(strcmp((OptComb->answer),"weightsum")==0) )
    Opt_comb=5; /* 5 pour somme ponderee */
  else
    if((strcmp((OptComb->answer),"min")==0))
      Opt_comb=1; /* 1 pour minimum */
  else
    if((strcmp((OptComb->answer),"mincrest")==0))
      Opt_comb=2; /* 2 pour minimum avec crete */
  else
    if((strcmp((OptComb->answer),"max")==0))
     Opt_comb=3; /* 3 pour maximum */
  else
    if ((strcmp((OptComb->answer),"maxcrest")==0))
      Opt_comb=4; /* 4 pour maximum avec crete */
    else
      {
	Erreur(7,0,OptComb->answer);
      } 



/* Initialisation */

  Post=initialiser();

  Floue=Init_CFloue();
  
  Lire_variables(OptRule->answer,var_utilise,nb_rast);
 
/* Place dans Floue les differents para de logique floue associés au fichiers Rasters */

  Lire_Fichier_Rasters(p_nom,OptVarSortie->answer,Floue,nb_rast,var_utilise);

  /* Renvoie Post et list_regle regles */

  Lire_Regles (OptRule -> answer,Floue,&Post,&regles);

  Traitement_donnees(Floue,&Post,&regles,OptResult->answer,p_nom,Opt_config,Opt_defuz,Opt_comb,OptMu->answer,OptDist->answer,Flag1->answer); 

/* Liberation memoire de la pile et du tableau de nom de varaible */
  for(j=0;j<nb_rast;j++)
    {
      free(p_nom[j]);
      free(var_utilise[j]);
    }

  free(Post);

}

