#include "gis.h"  
#define MAXCHOICE 8
extern int stop;
extern int prev_choice;
static char *x = " X ";
static char *star = " * ";
static char *arrow = "*->";
int
usermenu(val,stat)
char *stat;
int val;
{
/* Present the user with menu */
	int i,choice;
	char show[9][5],line[80];
	system("clear");

	for(i=1;i<MAXCHOICE;i++){
		if(i <= val)
			strcpy(show[i],star);
		else
			strcpy(show[i],x);
	}

	strcpy(show[val+1],arrow);

	if(prev_choice == 5){
		strcpy(show[6],star);
		strcpy(show[7],arrow);
	}

	fprintf(stderr,"Choose from the menu:\n");
	fprintf(stderr,"\n\n\n\t%s  1. Process steps without breaks.\n",show[1]);
	fprintf(stderr,"\n\t%s  2. Select basins for simulation.\n",show[2]);
	fprintf(stderr,"\n\t%s  3. Extract topographical data.\n",show[3]);
	fprintf(stderr,"\n\t%s  4. Select hydraulic parameters and simulation time.\n",show[4]);
	fprintf(stderr,"\n\t%s  5. Basin simulation.\n",show[5]);
	fprintf(stderr,"\n\t%s  6. Simulate any particular basin.\n",show[6]);
	fprintf(stderr,"\n\t%s  7. Channel routing of basin hydrographs.\n",show[7]);
	fprintf(stderr,"\n\t%s  8. Stop.\n",star);
	fprintf(stderr,"\n%s\n\nChoice:",stat);
	if(!gets(line)) exit(3);
	sscanf(line,"%d",&choice);
    	if(choice < 1 || choice > MAXCHOICE ){
		return (1);
	}else if(prev_choice == 5 && choice > prev_choice){
		system("clear");
		choice_handler(choice); 
	}else if(choice > (prev_choice + 1) && choice != MAXCHOICE){
		return (1);	
	}else{
		system("clear");
		choice_handler(choice); 
	}
	return (0);
}

void
status(val,stat)
int val;
char *stat;
{
	switch(val){
	    case 1:
				strcpy(stat,"You are starting from the beginning.\n");  
				break;
		case 2:
				strcpy(stat,"You stopped after selecting basins.\n");
				break;
		case 4:
				strcpy(stat,"You stopped after selecting hydraulic\nparameters and simulation time.\n");
				break;
		case 3:
				strcpy(stat,"You stopped after extracting topographical data.\n");
				break;
		case 5:
				strcpy(stat,"You stopped after basin simulation.\n");
				break;
		case 6:
				strcpy(stat,"You stopped after step 6.\n");
				break;
		case 7:
				strcpy(stat,"Done.\n");
				break;
		default:
				strcpy(stat,"No status.\n");
				break;
	}
}
