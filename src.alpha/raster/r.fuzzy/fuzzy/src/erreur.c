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
        /*printf(" No problem !!!! \n"); on affiche rien*/
        break;
      case 1:
        printf("\n!!! Inconsistent number of fuzzy variables on line # %d\n",nb_ligne); 
        break;
      case 2:
        printf("\n!!! Missing fuzzy variable on line # %d\n",nb_ligne);
        break;
      case 3:
        printf("\n!!! Problem in reading a word on line # %d\n",nb_ligne);
        break;
      case 4:
        printf("\n!!! No fuzzy number for fuzzy variable defined on line # %d\n",nb_ligne-1);
        break;
      case 5:
        printf("\n!!! Inconsistent character read on line # %d\n",nb_ligne);
        break;
      case 6:
        printf("\n!!! Too long word read on line # %d  (LONGMOT characters max.)\n",nb_ligne);
        printf("    LONGMOT is defined in include/regles.h\n");
        break;
      case 7:
        printf("\n!!! Problem in allocating memory for a string\n");
        break;
      case 8:
        printf("\n!!! Characteristics of fuzzy number on line # %d not arranged\n",nb_ligne );
	break;
      case 9:
        printf("\n!!! Non-existent fuzzy number type on line # %d\n",nb_ligne);
        break;
      case 10:
        printf("\n!!! Problem in allocating memory for node1\n");
        break;
      case 11:
        printf("\n!!! Name of fuzzy number/variable already used on line # %d\n",nb_ligne);
        break;
      case 12:
        printf("\n!!! Problem in allocating memory for fuzzy class\n");
        break;
      case 13:
        printf("\n!!! Problem in allocating memory for a fuzzy varaiable\n");
        break;
      case 14:
        printf("\n!!! Problem in allocating memory for a fuzzy number\n");
        break;
      case 15:
        printf("\n!!! Error in rule file on line # %d : IF omitted ?\n",nb_ligne);
        break;
      case 16:
        printf("\n!!! Error in rule file on line # %d : ( omitted ?\n",nb_ligne);
        break;
      case 17:
        printf("\n!!! Error in rule file on line # %d : the variable %s is not\n",nb_ligne,ch_err);
        printf("    one of the possible classes at this place\n");
        break;
      case 18:
        printf("\n!!! Error in rule file on line # %d : too many variables\n",nb_ligne);
        break;
      case 19:
        printf("\n!!! Problem in allocating memory for a  list_regle\n");
        break;
      case 20:
        printf("\n!!! Problem in allocating memory for an integer tableau\n");
        break;
      case 21:
        printf("\n!!! Problem in allocating memory for an info\n");
        break;
      case 22:
        printf("\n!!! Error in the rule : missing one or more )\n");
        break;
      case 23:
        printf("\n!!! Error in the rule : missing one or more (\n");
        break;
      case 24:
        printf("\n!!! Error in the rule : 2 logical operators follow close\n");
        printf("    or are at the beginning of the rule\n");
        break;
      case 25:
        printf("\n!!! Error in the rule : the string %s is unknown\n",ch_err);
        break;
      case 26:
        printf("\n!!! Error in the rule : two variables follow close\n");
        break;
      case 27:
        printf("\n!!! Problem in allocating memory for a stack\n");
        break;
      case 28:
        printf("\n!!! Error : unable to unstack an empty stack\n");
        break;
      case 29:
        printf("\n!!! Error : unable to unstack an empty stack\n");
        printf("    unstacking is from bottom\n");
        break;
      case 30:
        printf("\n!!! Error : unable to stack ... because stack is full\n");
        break;
      case 100:
        printf("\n!!! Problem in the parameter file :\n");
        printf("    invalid fuzzy variables file name\n");
        break;
      case 101:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne);
        printf("    invalid rule file name\n");
        break;
      case 102:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne);
        printf("    invalid index for calibration/validation/prevision option\n");
        break;
      case 103:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne);
        printf("    invalid index for validation coefficient option\n");
        break;
      case 104:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne);
        printf("    invalid data file name\n");
        break;
      case 105:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne);
        printf("    unable to open results file\n" );
        break;
      case 106:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne); 
        printf("    unable to open intermediate file\n");
        break;
      case 107:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne); 
        printf("    invalid index for rule agregation option\n" );
        break;
      case 108:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne); 
        printf("   invalid index for defuzzification option\n");
        break;
      case 109:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne); 
        printf("    invalid index for (t-norm, t-conorm) option\n");
        break;
      case 110:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne); 
        printf("    invalid index for visualization option\n");
        break;
      case 111:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne); 
        printf("    invalid file name\n");
        break;
      case 112:
        printf("\n!!! Problem in the parameter file on line # %d :\n",nb_ligne); 
        printf("    display is wanted for undefined variables\n");
        break;
      case 113:
        printf("\n!!! Problem in the fuzzy variables file on line # %d :\n",nb_ligne);
        printf("    the fuzzy number has a vertical part : change one of the limits\n");
        break;
      case 114: 
        printf("\n!!! Problem in the fuzzy variables file on line # %d :\n",nb_ligne);
        printf("    the trapeze is a triangle ( b = c )\n");
        break;
      case 115:
        printf("\n!!! Problem in the fuzzy variables file on line # %d :\n",nb_ligne);
        printf("    the variable index is inconsistent with the number of variables\n");  
	break;
    case 116:
        printf("\n!!! Problem : file can't be opened\n");
        break;
    case 117:
        printf("\n!!! Error on output variable %s already re-injected :\n",ch_err);
	printf("    unable to have the same variable re-injected more one time\n");
	break;
    case 118:
        printf("\n!!! Error in reading rule :\n");
	printf("    no output variable %s\n",ch_err);
	break;
    case 119 :
        printf("\n!!! Error : parameter file has been omitted\n");
	break;
    case 120:
        printf("\n!!! Problem: empty stack -> no rule\n");
	break;
    case 121:
        printf("\n!!! Error in the rule : THEN is missing\n");
	break;
    case 122:
       printf("\n!!! Error : pointer on op %d n'a pas peu aboutir ds la recherche\n",nb_ligne);
       printf("    ds la table des variables a tab[op-10] pb lors de la def des var\n");
       printf("    definise vos variable ds l'ordre d'utilisation ds le fichiers des regles\n");
       break;
    case 123:
      printf("\n!!! Fuzzy number can't be found\n");
      break;
    case 124:
      printf("\n!!! Error : pointer is empty\n");
      break;
    }
  if ( err != 0 )
    exit(0);
}
  
