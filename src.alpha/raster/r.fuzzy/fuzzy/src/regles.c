#define MOD_regles

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include "regles.h"
#include "piles.h"
#include "classes_floues.h"
#include "maths.h"
#include "nb_flous.h"
 

/**************************************************************************/
/*                 definition des fonctions et procedures                 */
/**************************************************************************/

/* Morel L. Fonctions */
/**************************************************************************/
/******* Libere les pointeur de la solution *******************************/
/**************************************************************************/
void LibereSolution(sol * Sol)
/* Modif 6-10-97 */
/* Parcours la liste des solutions et les liberes un par un les cases memoire, par appel recursif */
{
  
  if (Sol->suiv==NULL) 
    {
      free(Sol);
      /* printf("DEBUG :: regle.c LibereSolution \n"); */
    }
  else
    {
      LibereSolution(Sol->suiv);
      Sol->suiv = NULL;
      LibereSolution(Sol);
    }     
}

/**************************************************************************/
/******* Affichage de la solution *****************************************/
/**************************************************************************/
void AfficheSolution( sol * Sol,
		      int nb_var)
/* Parcours la liste des solutions et l'affiche */
{ 
  if (Sol!=NULL) 
    {
      printf("		dof : %5.5lf",Sol->dof);
      
      printf(", num regle : %i %i",Sol->regle->num,Sol->num_regle);
      printf(", nom : %s\n",Sol->name);
      printf("		nom regle pte : %s\n",Sol->regle->reg_act[nb_var].name);
      printf("		nom node pte  : %s\n",Sol->regle->reg_act[nb_var].var_sortie->nombre.name);
      AfficheSolution(Sol->suiv, nb_var );
    }
}

/********************************************************************(*****/
/******* Affichage de la solution *****************************************/
/**************************************************************************/
void AfficheRegles( list_regle * Regles,int Nb_var) 
/* list_regle  Liste des regles a afficher */
/* Nb_var nombre de variable par regle */
/* Parcours la liste des regles et l'affiche */
{
  int i;
  if (Regles!=NULL)
    {
      printf("Num Regle : %i\n",Regles->num);
      for (i=0;i<=Nb_var;i++)
	{
	  printf("	indice : %i",i);
	  printf("	nom    : %s",Regles->reg_act[i].name);
	  printf("	valeur : %5.5lf",Regles->reg_act[i].valeur);
	  printf("	N var  : %i",Regles->reg_act[i].num_var_sortie);
	  printf("	nom ptr node : %s\n",Regles->reg_act[i].var_sortie->nombre.name);
	}
      AfficheRegles( Regles->suiv, Nb_var );
    }
}

/***********************************************************/
char * lit_var_sortie(char ** regle,
		      int  i,
		      int * nb_var_sortie)
{
  char * nom;
  char * th;
  int j;
  int k;
  int ifin;
  int ok;
  (* nb_var_sortie) = 0;
  
  ifin = i;
  /*
    printf("DEBUG :: lecture var sortie -> %s %i \n",regle[0],* nb_var_sortie);
  */
  (* regle)[i] = '.';      /* dans le cas ou il n'y a pas d'espace apres la variable de sortie */
  i--;                 /* pour pouvoir arrete le recopiage car il ne trouvera pas de point */
  /* je me deplace ds la regle et je verifie si le nom obtenu n'est pas THEN */

  th = (char *) malloc (LONGMOT*sizeof(char));
  ok = 1;
  while ((* regle)[i] == '.')
    i--;
  while (ok==1)
    {
      /* je me deplace devant le nom */
      while (((* regle)[i] != '.') && (i>0))
	i--;
      if (i>0)
	{ /* i est sur un caractere  . */
	  j = 0;
	  k = i+1; /* k est sur la premiere lettre du mot */
	  while ((* regle)[k] != '.') /* recopie le nom trouve */
	    {
	      th[j] = (* regle)[k];
	      j++;
	      k++;
	    }
	  th[j] = '\0';
	  /* k est sur le . apres le mot */

	  /* je compare pour savoir si c'est le THEN ou pas */
	  if (strcmp(th,"THEN") != 0)
	    { /* ce n'est pas THEN dc c' une variable de sortie */
	     (* nb_var_sortie)++; /* une variable de plus */
	       
	     /* je passe tous les points */
	     while ((* regle)[i] == '.')
	       i--;
	    }
	  else
	    {
	      /* le Then a ete trouve donc on arrete */
	      ok = 0;
	      /* on enleve de la regle tout ce qui se trouve apres la derniere 
		 variable d'entree et verifier si la parenthese fermante a bien ete mise
	      */
	      while ((* regle)[i] == '.')
		i--;
	      if ((* regle)[i] != ')')
		{
		  affiche_erreur(22,0,NULL);
		  /* ERREUR : Parenthese fermante de la regle oubliee. */
		}
	      i--;	      
	      /* on positionne i sur la derniere lettre de la variable ecrite juste avant le THEN */
	      while ((* regle)[i] == '.')
		i--;
	      /* on sectionne celle ci de facon a ce que regle ne contienne que ce 
		 qui est ecris entre le "IF (" et le ") THEN".*/
	      (* regle)[i+1] = '\0';

	      /* on replace i sur le . apres le THEN */
	      
	      i = k;
	    }
	  /*
	    printf("DEBUG :: Recherche du THEN %i %s_fin i-%i\n",(* nb_var_sortie),th,i);
	  */
	}
      else
	{
	  affiche_erreur(121,0,NULL);
	  /* ERREUR :: THEN a ete omis dans le regle.*/
	}	  

    }
  /* le i est place sur le . apres le THEN  */
    
  
  /* place sur la premiere var de sortie */
  while ((* regle)[i] == '.')
    i++;
  /*
    printf("DEBUG :: lecture nb var sortie ->  : %i %s\n",(* nb_var_sortie),regle[0]);
  */  
  nom = (char *) malloc (LONGMOT*sizeof(char));
  for ( j=0; j< ifin-i; j++ )
    nom[j] = (* regle)[j+i];
  nom[j] = '.'; /* il faut qu'il y ai un point pour que la lecture des var
		   de sorties puisse se faire correctement */
  nom[j+1] = '\0';
  
  free (th);
  return (nom); /* MOREL l. retourne le nom des variables de sortie */
}



/**************************************************************************/
/******* Transformation en pile d'execution *******************************/
/**************************************************************************/
 
erreur trans_pile(pile * pi,
		  char * regle,
		  int * var_act,
		  int * ench_var,
		  classe_floue * classe)
{
  int i;
  operateur op;
  char * nom;
  int nb_paro=0;
  int nb_parf=0;                 /* op_int = 0 >> operateurs logiques interdits sauf le non */ 
  int op_int=0;                  /* op_int = 1 >> non et variables interdites */
  erreur err;                    /* op_int = 2 >> operateurs logiques interdits non compris */

  i = 0;
  (* pi) = initialiser();
  while(regle[i] != '\0')
   {
     if (regle[i] != '.')
      {
        switch (regle[i])
         {
           case '(': op = paro;
                     nb_paro++;
                     i++;
                     break;
           case ')': op = parf;
                     nb_parf++;
                     i++;
                     break;
           default : nom = lit_av_point(regle,&i);
                     err = trouve_nom(&op,nom,&op_int,classe);
                     affiche_erreur(err,0,nom);
                     if ( op >= 10)
                      {
                        ench_var[(* var_act)] = op-10; 
                        (* var_act)++;
                      }
                     break;
         }
        empiler((* pi),op);
   }
     else
      i++;
   }
  


  if (nb_paro > nb_parf)
    return(22);
  else 
    if (nb_parf > nb_paro)
      return(23);

  /* DEBUG */
 /* printf("DEBUG :: regle.c : Trans_pile affichage pile pi");
  AffichePile((*pi));*/

  return(0);
}


/******************************************************/
char * lit_av_point(char * regle,
		    int * i)
{
  char * var;
  int j; 

  j = 0;
  var = (char *) malloc (20 * sizeof (char));
  if( var == NULL )
    affiche_erreur(7,0,'\0');
  while ((regle[* i] != '.') && (regle[* i] != '\0')
            && (regle[* i] != '(') && (regle[* i] != ')'))
   {
    var[j] = regle[* i];
    (* i) = (* i) + 1;
    j++;
   }
  var[j] = '\0'; /* MODIF 01-11-97 MOREL L ajout de la fin de chaine*/
  return(var);
}




/******************************************************/
erreur trouve_nom(operateur * op,
		  char * nom,
		  int * op_int,
		  classe_floue * classe)
{
  int i;

  (* op) = -1;                                  /* 31-10-97 MOREL L. initialisation de op	*/
  /* 31-10-97 MOREL L */
  /* ERREUR 24 ==> Erreur dans la regle : il y a deux operateurs logiques a la suite		*/
  /*		   ou un en debut de regle!!!							*/

  if (strcmp (nom,"AND") == 0)
   {
     if (((* op_int) == 0) || ((* op_int) == 2))
       return(24);
     (* op_int) = 0;
     (* op) = AND;
   }
  if (strcmp (nom,"OR") == 0)
   {
     if (((* op_int) == 0) || ((* op_int) == 2))
       return(24);
     (* op_int) = 0;
     (* op) = OR;
   }
  if (strcmp (nom,"NOT") == 0)
   {
     if (((* op_int) == 1) || ((* op_int) == 2))
       return(24);
     (* op_int) = 2;
     (* op) = NOT;
   }
  if (strcmp (nom,"XOR") == 0)
   {
     if (((* op_int) == 0) || ((* op_int) == 2))
       return(24);
     (* op_int) = 0;
     (* op) = XOR;
   }
  /* 31-10 97 MOREL L. Rajoute un teste pour accelerer la procedure si l'operateur est deja		*/
  /* affecter i.e != -1	on ne fait par le for								*/
  /* 31-10 97 MOREL L. detail erreur 26									*/
  /* Erreur dans la regle : il y a deux variables a la suite !!!					*/
  
  if ((* op) == -1)				/* Code rajouter le 31-10 97 MOREL L			*/	 
    {				                /* Code rajouter le 31-10 97 MOREL L			*/
      for(i=0;i<classe->nb;i++)                     /* on fait ce for que si op n'est pas encore		*/
	if (strcmp (nom,classe->tab[i]->name) == 0) /* affecter parcours des noms des variables		*/
	  {						/* stocker dans la classe et verifie si c'est == nom	*/
	    if ((* op_int) == 1)			/* lorsque lq variable est trouvee on prend son indice  */
	      return(26);				/* dans le tableau class->tab[i] et on ajoute 10	*/
	    (* op_int) = 1;
	    (* op) = i+10;
	  }
  }
						/* Code rajouter le 31-10 97 MOREL L			*/
if ((* op) == -1)
     return(25);
 return(0);  
}



/*************************************************************/
int priorite(operateur op)
{
  switch (op)
   {
     case NOT:return(2);
              break;
     case paro:return(0);
               break;
     case parf:return(0);
               break;
     default:  return(1);
               break;
   }
}

/*************************************************************/
int eval_bool(pile post,
	      info * reg_act,
	      classe_floue * classe)
/* reg_act est un tableau d'information */
{
  operateur op;
  int result;
  int result1;
  op = depiler(post);

  switch (op)
   {
     case NOT:result = eval_bool(post,reg_act,classe);
            if (result == 1)
              return (0);
            else
              return (1);
            break;
   case AND:result = eval_bool(post,reg_act,classe);
            result1 = eval_bool(post,reg_act,classe);
            if ((result == 1) && (result1 == 1))
              return (1);
            else
              return (0);
            break;
     case OR:result = eval_bool(post,reg_act,classe);
            result1 = eval_bool(post,reg_act,classe);
            if ((result == 1) || (result1 == 1))
              return (1);
            else
              return (0);
            break;
     case XOR:result = eval_bool(post,reg_act,classe);
            result1 = eval_bool(post,reg_act,classe);
            if ((result == 1) || (result1 == 1))
              return (1);
            else
              return (0);
            break;
     default:
             
 /* printf("DEBUG ::eval_bool verifie : op : %i , %s ,= %f  N var : %i %s => \n",op-10,
	  reg_act[op-10].name,
	  reg_act[op-10].valeur,
	  reg_act[op-10].num_var_sortie,
	  reg_act[op-10].var_sortie->nombre.name); */

/*	  MODIF 23-12-97 MOREL L
	  pour prendre en compte le num de var et nom le op-10 qui pose pb qud les 
	  var ne sont pas definie ds le meme ordre aue le fichier des regles

	  return (verifie(op-10,reg_act[op-10].name,reg_act[op-10].valeur,classe));
       */
       return (verifie(reg_act[op-10].num_var_sortie,reg_act[op-10].name,reg_act[op-10].valeur,classe));
             break;
   }
}

/**********************************************************/
double eval_dof(pile post,
		info * reg_act,
		int config[4],
		classe_floue * classe)
{
  operateur op;
  double result;
  double result1;
  double retour; /* Modif MOREL. */
  node * pteur;
  int rep;

  
  op = depiler(post);
  switch(op)
   {
     case NOT:result = eval_dof(post,reg_act,config,classe);
              return(0.);   /* a faire */
              break;
     case AND:result = eval_dof(post,reg_act,config,classe);
             result1 = eval_dof(post,reg_act,config,classe);
	     
/* -------------------------------------------------------------------------
    modif delclaux le 7-10-97 -- config[1] est en realite config[2] 
             if (config[1] == 0)
--------------------------------------------------------------------------*/
             if (config[2] == 0)
               return(min(result,result1));
             else
               return(result*result1);
             break;
     case OR:result = eval_dof(post,reg_act,config,classe);
             result1 = eval_dof(post,reg_act,config,classe);
/* -------------------------------------------------------------------------
    modif delclaux le 7-10-97 -- config[2] est en realite config[1] 
             if (config[2] == 0)
--------------------------------------------------------------------------*/
             if (config[1] == 0)
               return(max(result,result1));
             else
               return(result+result1-result*result1);
             break;
     case XOR:result = eval_dof(post,reg_act,config,classe);
               result1 = eval_dof(post,reg_act,config,classe);  /* result1 est le A */
               if (config[3] == 0)
                 return(max(min(1-result1,result),min(result1,1-result)));
               else
                 return(result+result1-2*result*result1);
               break;
     default:
           /* MODIF 23-12-97 MOREL L
	      pour prendre en compte le num de var et nom le op-10 qui pose pb qud les 
	      var ne sont pas definie ds le meme ordre aue le fichier des regles

	      pteur = classe->tab[op-10]->liste_elem;
	   */
       
            pteur = classe->tab[reg_act[op-10].num_var_sortie]->liste_elem;

            rep = 0;
             while ((pteur != NULL) && (rep == 0))
              {
                if (strcmp(reg_act[op-10].name,pteur->nombre.name) == 0)
                 {
                   rep = 1;
                   switch(pteur->nombre.type)
                    {
                      case 0:
			retour=appartient_triangle(&pteur->nombre,reg_act[op-10].valeur);
			/*printf("tri dof %5.5lf %s -> %s\n",retour,reg_act[op-10].name,
			  reg_act[classe->nb-1].name);*/
			return(retour);
			break;
                      case 1:
			retour=appartient_trapeze(&pteur->nombre,reg_act[op-10].valeur);
			/*printf("tra dof %5.5lf %s -> %s\n",retour,reg_act[op-10].name,
			  reg_act[classe->nb-1].name);*/
			return(retour);
			break;

                      case 2:
			retour=appartient_LR(&pteur->nombre,reg_act[op-10].valeur);
			/*printf(" LR %5.5lf %s -> %s\n",retour,reg_act[op-10].name,
			  reg_act[classe->nb-1].name);*/
			return(retour);
			break;
		    
		      case 3:
			retour=appartient_LRG(&pteur->nombre,reg_act[op-10].valeur);
			/* printf(" LR %5.5lf %s -> %s\n",retour,reg_act[op-10].name,
			  reg_act[classe->nb-1].name); */
			return(retour);
			break;
                    }
                 }
                pteur = pteur->suiv;
              }
	     /* probleme */
	     affiche_erreur(122,op-10,NULL);
	     /*ERREUR :: Le ptr sur l'op n'a pas peu aboutir ds la recherche
	     ds la table des variables a tab[op-10] pb lors de la def des var
	     definise vos variable ds l'ordre d'utilisation ds le fichiers des regles*/
	     exit(0);
             break;
   }
}



/**********************************************************/
int verifie(int num_var,
	    char classe_var[LONGMOT],
	    double valeur,
	    classe_floue * classe)
{
  node * pteur;
  int rep;

  rep = 0;
  
  pteur = classe->tab[num_var]->liste_elem;
 
  while((pteur != NULL) && (rep == 0)) /* MOREL L. recherche ds la classe la variable classe_var */
    {
      if (strcmp (classe_var,pteur->nombre.name) == 0)
	rep = 1;
      else
	pteur = pteur->suiv;
    }

  if (rep == 1) /* MOREL L. si la variable classe a ete trouver on verifie si elle est bien 
		  	   compris ds la var floue */ {
    switch(pteur->nombre.type)
      {
      case trian:
	if ((valeur > pteur->nombre.classe.tri[0])
	    && (valeur < pteur->nombre.classe.tri[2]))
	  {
	    /* printf("(1)\n"); */
	    return (1);
	  }
	else
	  {
	    /* printf("(0)\n"); */
	    return( 0 );
	  }
	break;
      case trap:
	if ((valeur > pteur->nombre.classe.tra[0])
	    && (valeur < pteur->nombre.classe.tra[3]))
	  {
	    /* printf("(1)\n"); */
	    return (1); 
	  }
	else
	  {
	   /* printf("(0)\n"); */
	    return( 0 );
	  }
	break;
      case lr:
	if ((valeur > pteur->nombre.classe.L[0])
	    && (valeur < pteur->nombre.classe.L[2]))
	  return(1);
	else
	  return( 0 );
	break;
      case lrg:
	if ((valeur > pteur->nombre.classe.Lrg[0])
	    && (valeur < pteur->nombre.classe.Lrg[3]))
	  {
	    /* printf("(1)\n"); */
	    return (1); 
	  }
	else
	  {
	   /* printf("(0)\n"); */
	    return( 0 );
	  }
	break;
	
      default:
	affiche_erreur(123,0,NULL);
	/* defaut type de nb flou non trouve */
	return (0);
	break;
     }
    }
  affiche_erreur(123,0,NULL);
  return (0);
}

/***********************************************************/
char * lit_var_sortie1(char ** regle,
		       int i,
		       int * nb_var_sortie)
{
  char * nom;
  char * th;
  int j;
  int k;
  * nb_var_sortie = 0;
  /*  
      printf("DEBUG :: lecture var sortie -> %s %i \n",regle[0],* nb_var_sortie);
  */
/* dans le cas ou il n'y a pas d'espace apres la variable de sortie */
  (* regle)[i] = '.';
  i--;/* pour pouvoir arrete le recopiage car il ne trouvera pas de point */
  while ((* regle)[i] == '.')
    i--;


  /*  lecture de la variable de sortie et recopie dans nom */
  while ((* regle)[i] != '.')
    i--;
  nom = (char *) malloc (LONGMOT*sizeof(char));
  j = 0;
  k = i+1;
  while ((* regle)[k] != '.')
   {
     nom[j] = (* regle)[k];
     j++;
     k++;
   }
  nom[j] = '\0';
  /* printf("DEBUG :: premier sortie %s \n",nom);
   */
  /*  lecture du then  */
  while ((* regle)[i] == '.')
    i--;
  while ((* regle)[i] != '.')
    i--;
  th = (char *) malloc (LONGMOT*sizeof(char));
  j = 0;
  k = i+1;
  while ((* regle)[k] != '.')
   {
     th[j] = (* regle)[k];
     j++;
     k++;
   }
  th[j] = '\0';
  if (strcmp(th,"THEN") != 0)
   {
     affiche_erreur(121,0,0);
    /* THEN a ete omis dans la regle.*/
   }
  /* Modif 11/97 MOREL L. la variable th n'a pas ete detruite */
  free (th); 

  /* 10/97 par MOREL l. verification de la prarenthese fermante dans classe->regle = regle ici.*/
  while ((* regle)[i] == '.')
    i--;
  if ((* regle)[i] != ')')
   {
     affiche_erreur(22,0,NULL);
    /* Parenthese fermante de la regle oubliee. */
   }
  i--;

  /* 10/97 par MOREL l. positionne surE la derniere lettre de la variable ecrite juste avant le THEN */
  while ((* regle)[i] == '.')
    i--;

  /* 10/97 par MOREL l. on sectionne celle ci de facon a ce que regle ne contienne que ce 
     qui est ecris entre le IF ( et le ) THEN.*/
  (* regle)[i+1] = '\0';
  return (nom); /* MOREL l. retourne le nom de la variable de sortie */
}


/***********************************************************/
int verifie_var(char * nom,
		int * var_act,
		int * ench_var,
		classe_floue * classe)
{
  node * pteur;

  pteur = classe->tab[ench_var[(* var_act)]]->liste_elem;
  while (pteur != NULL)
   {
     if( strcmp(pteur->nombre.name,nom) == 0 )
       return(0);
     pteur = pteur->suiv;
   }
  return(1);
}

/*************************************************************/
void saisie_valeur(list_regle ** regles,
		   classe_floue * classe)
{
  int i;
  double val;
  list_regle * pteur;

  for(i=0;i<classe->nb;i++)
    if (strcmp(classe->var_sortie,classe->tab[i]->name) != 0)
     {
       printf("Saisissez la valeur de la variable %s:",classe->tab[i]->name);
       scanf("%lf",&val);
       pteur = (* regles);
       while(pteur != NULL)
        {
          pteur->reg_act[i].valeur = val;
          pteur = pteur->suiv;
        }
     }
}



/****************************************************/
sol * test_regles(pile post,
		  list_regle * regles,
		  int config[4],
		  classe_floue * floues,
		  int fic_inter,
		  int fic_interm)
{
  list_regle * pteurRegle;
  int rep;
  sol * solution;
  sol * nouv;
  int sauv;
  /*   printf("DEBUG : test_regles\n"); */
  pteurRegle = regles;
  solution = NULL;
  while(pteurRegle != NULL) /* MOREL L. Parcours toutes les regles et on teste pour 
			       savoir si elle est valide */
   {
     sauv = post->tete;
     /*
       printf("DEBUG : Eval_Bool regle Num : %i, sortie -> %s\n",pteurRegle->num,
       pteurRegle->reg_act[floues->nb-1].name); */
     

     rep = eval_bool(post,pteurRegle->reg_act,floues);
     post->tete = sauv;
     if (rep == 1)
      {
        nouv = (sol *) malloc (sizeof(sol));
        nouv->suiv = solution;
	/* MODIF MOREL L. 27-11-97 
	   Affecte le nom de la var de sortie s'il y en a plusieur alors on met le permier nom 
	ancien code :
	strcpy(nouv->name,pteurRegle->reg_act[floues->nb-1].name);
	*/
        strcpy(nouv->name,pteurRegle->reg_act[floues->nb_var_utilise-floues->nb_var_sortie+1].name); 
	
        sauv = post->tete;
	/*
	  printf("\nDEBUG : Eval_Dof  regle Num : %i, sortie -> %s\n",pteurRegle->num,
	  pteurRegle->reg_act[floues->nb-1].name);
	*/
        nouv->dof = eval_dof(post,pteurRegle->reg_act,config,floues);
	nouv->num_regle = pteurRegle->num;
	nouv->regle = pteurRegle;
	/*
	  printf("DEBUG : Eval_Dof resultat apres combinaison %5.5lf %s\n\n",nouv->dof,nouv->name);
	*/
        if (fic_inter == 1)
          ecrit_fic_inter(fic_interm,nouv->name,nouv->dof,pteurRegle->reg_act,floues);
        post->tete = sauv;
        solution = nouv;
      }
     pteurRegle = pteurRegle->suiv;
   }
  
  return(solution);
}



/************************************************************/
double combi_deflou(sol * solution,
		    classe_floue * floues,
		    int deflou_type,
		    int combi_type,
		    int visu,
		    int num_var_sortie,
		    discret * ptr_dis)
{ /* Combi deflou */
  int rep;
  discret dis=NULL;
  discret dis1;
  node * ptrnode=NULL;
  sol * ptrsol1;
  sol * ptrsol2;
  double val;
  int passage;

  
  /* printf("DEBUG : regles.c combi_defou\n\nSolution :\n");
    AfficheSolution(solution,floues->nb_var_utilise); */
  
  passage = 0;
  rep = 0;
  ptrsol1 = solution;
  /* MODIF 17-11-97 MOREL L. verification de l'existance de la solution */
  if (ptrsol1==NULL) 
    {
      affiche_erreur(124,0,NULL);
      /* ERREUR :: Le pointeur vers la solution est vide */
    }
  /* Fin Modif 
     printf("	parcours liste element\n");
     Par defaut il selectionne la var de sortie en tant que derniere moi je vais chercher
     c' laquelle et lui affecter */
  /* MODIF MOREL L. 26-11-97 
     ptrnode = floues->tab[floues->nb-1]->liste_elem;*/

  ptrnode =  ptrsol1->regle->reg_act[num_var_sortie].var_sortie;
  
  /* MODIF MOREL L. 26-11-97 */
  /*   printf("DEBUG :: combi deflou pour pte %s avt %s \n",ptrnode->nombre.name,
     ptrsol1->regle->reg_act[num_var_sortie].name);*/
  
  /* Parcours des elements pour trouver la correspondance avec le nom la var 
     de sortie et le nom de celle contenu ds solution ptrsol1 */
  
    /*  printf("Deflou 1 ok pour ptrnode sur var flou %s et solution %s \n",
     ptrnode->nombre.name,
     ptrsol1->regle->reg_act[num_var_sortie].var_sortie->nombre.name); */
 
  /* MODIF MOREL L. 26-11-97 
     while ((ptrnode != NULL) && (rep == 0)) 
     {  			
   if (strcmp(ptrsol1->name,ptrnode->nombre.name) == 0)
   {       
   rep = 1;
   printf("Deflou ok pour ptrnode sur var flou %s et solution %s \n",
   ptrnode->nombre.name,
   ptrsol1->regle->reg_act[num_var_sortie].var_sortie->nombre.name);
   }
   else
   ptrnode = ptrnode->suiv;
   }
  */
  if ( ptrnode == NULL)
    {
      affiche_erreur(124,0,NULL);
      /* ERREUR : Le ptr vers la variable de sortie est vide, voir code Combi_deflou */
    }
  
  switch(ptrnode->nombre.type)
    { /* Discretisation en fonction du type de la sortie */
      case trian:dis = discretise_triangle(&ptrnode->nombre);
	break;
      case trap:dis = discretise_trapeze(&ptrnode->nombre);
	break;
      case lr:dis = discretise_LR(&ptrnode->nombre);
	break;
      case lrg:dis = discretise_LRG(&ptrnode->nombre);
	break;
    }

  /*printf("	ptr discretise\n");*/
  if(ptrsol1->suiv != NULL)
    {
      /* printf("	ptr1 discretise\n"); */
      ptrsol2 = ptrsol1->suiv;
      while (ptrsol2 != NULL)
	{
	  rep = 0;
	  /* MODIF MOREL L. 26-11-97 
	    ptrnode = floues->tab[floues->nb-1]->liste_elem; */
	  ptrnode =  ptrsol2->regle->reg_act[num_var_sortie].var_sortie;
	 
	  /* printf("Deflou 12 ok pour ptrnode sur var flou %s et solution %s \n",
	    ptrnode->nombre.name,
	    ptrsol2->regle->reg_act[num_var_sortie].var_sortie->nombre.name); */
	  
	  /* MODIF MOREL L. 26-11-97 
	    while ((ptrnode != NULL) && (rep == 0))
	     {
	     if (strcmp(ptrsol2->name,ptrnode->nombre.name) == 0)
	     {       
	     rep = 1;
	     printf("Deflou 2 ok pour ptrnode sur var flou %s et solution %s \n",
	     ptrnode->nombre.name,
	     ptrsol2->regle->reg_act[num_var_sortie].var_sortie->nombre.name); 
	     }
	     
	     else
	     ptrnode = ptrnode->suiv;
	     }
	  */
	
	  if ( ptrnode == NULL)
	    {
	      affiche_erreur(124,0,NULL);
	      /* ERREUR : Le ptr vers la variable de sortie est vide, voir code Combi_deflou */
	    }
	 
	  /* printf("DEBUG :: combi deflou 2 pour pte %s avt %s \n",ptrnode->nombre.name,
		 ptrsol1->regle->reg_act[num_var_sortie].name); */
	  
	  switch(ptrnode->nombre.type)
	    {
             case trian:dis1 = discretise_triangle(&ptrnode->nombre);
                        break;
             case trap:dis1 = discretise_trapeze(&ptrnode->nombre);
                        break;
             case lr:dis1 = discretise_LR(&ptrnode->nombre);
                        break;
	    case lrg:dis1 = discretise_LRG(&ptrnode->nombre);
	               break;
	    }
       	 
         switch(combi_type)
           {
             case 1:if (passage == 0)
                      dis = comb_minimum(dis,ptrsol1->dof,dis1,ptrsol2->dof);
                    else
                      dis = comb_minimum(dis,1.,dis1,ptrsol2->dof);
                    passage = 1;
                    break;
             case 2:if (passage == 0)
                      dis = comb_mini_crete(dis,ptrsol1->dof,dis1,ptrsol2->dof);
                    else
                      dis = comb_mini_crete(dis,1.,dis1,ptrsol2->dof);
                    passage = 1;
                    break;
             case 3:if (passage == 0)
                      dis = comb_maximum(dis,ptrsol1->dof,dis1,ptrsol2->dof);
                    else
                      dis = comb_maximum(dis,1.,dis1,ptrsol2->dof);
                    passage = 1;
                    break;
             case 4:if (passage == 0)
                      dis = comb_maxi_crete(dis,ptrsol1->dof,dis1,ptrsol2->dof);
                    else
                      dis = comb_maxi_crete(dis,1.,dis1,ptrsol2->dof);
                    passage = 1;
                    break;
             case 5:if (passage == 0)
	               dis = comb_somme_pond(dis,ptrsol1->dof,dis1,ptrsol2->dof);
	            else
		      dis = comb_somme_pond(dis,1.,dis1,ptrsol2->dof);
	            passage = 1;
	            break;
           }
	
         ptrsol1 = ptrsol1->suiv;
         ptrsol2 = ptrsol2->suiv;
 
	LibereDiscret(dis1);
       
       }

    }

  *ptr_dis=dis;

  /* printf("	deflou type\n"); */
  switch(deflou_type)
   {
     case 1:
       val = deflou_max(dis,visu);
       /* printf("DEBUG :: Combi_deflou max Liberation discret\n"); */
  
       break;
     case 2:
       val = deflou_gravite(dis,visu);
       /* printf("DEBUG :: Combi_deflou gravite Liberation discret\n"); */
      
       break;
     case 3:
       val = deflou_median(dis,0.01,visu);
       /* printf("DEBUG :: Combi_deflou median Liberation discret\n"); */
      
       break;
   }
  return(val);
}

