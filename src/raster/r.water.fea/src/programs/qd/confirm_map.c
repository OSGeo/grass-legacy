/* Program to querry and confirm map */
#include "gis.h"
void
confirm_map(typemap,mapname) 
char *typemap;
char *mapname;
{
	char quest[50];
	strcpy(quest,"Enter the name of ");    
	strcat(quest,typemap);
	if(G_ask_cell_old(quest,mapname) == NULL){
		fprintf(stderr,"Cannot proceed further without name.\n");
		exit(2);
	}
}
