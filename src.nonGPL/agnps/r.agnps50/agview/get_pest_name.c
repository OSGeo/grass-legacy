/************************************************************** 
   The purpose of this file is to get the name of the pesticide
   from the *.nps file for displaying it on the maps 
***************************************************************/
    
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
 
char get_pest_name(char *input_file, int num, int resolution)
{
 
	FILE *infile;	
	char line[100];
	char cmd[100];
	char pesticide[30];
        char check;
	int a1;
	
	if((infile = fopen(input_file,"r")) == NULL) {
       fprintf(stderr,"Can't open file: %s\n",input_file);
       sprintf(cmd,"$GISBASE/etc/agnps50/Bad_dir_message.sh %s.nps\n",input_file); 
       system(cmd); 
       return;
       }
    fclose(infile);
       
    infile = fopen(input_file,"r");
    while((fgets(line,80,infile)) != NULL) {
       if((strncmp(line,"PESTICIDE",8)) ==0) {
	      fgets(line,80,infile);	
	      sscanf(line,"%d %s", &a1, pesticide);
	      }
	    }
	/* Next line checks to see if string is valid */    
	if(pesticide[0]<65 || pesticide[0]>122){
		 system("clear");
	   do {
	     fprintf (stderr,"\nPesticide data not found\n");
         fprintf (stderr,"Press return to continue...");
         scanf("%c", &check);
        }while(check != '\n');    
      }
    else {    
		if (num==9)
	 	  sprintf(cmd,"$GISBASE/etc/agnps50/show_PRUNOFF.csh %d %d %s",num,resolution,pesticide);
    	else if (num==10)
     	  sprintf(cmd,"$GISBASE/etc/agnps50/show_PSED.csh %d %d %s",num,resolution,pesticide);
    	else if (num==11)
          sprintf(cmd,"$GISBASE/etc/agnps50/show_PPERC.csh %d %d %s",num,resolution,pesticide);
    	system(cmd);
	  }
}
