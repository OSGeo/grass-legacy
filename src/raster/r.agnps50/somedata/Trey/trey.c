#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
 
void find(char *filename, int place)
{ 
     
     FILE *ifile;
     char temp[80];
     int i;
     int a1;
     char check;
    ifile = fopen(filename,"r");
	 while((fgets(temp,80,ifile)) != NULL) {
        if((strncmp(temp,"PESTICIDE",8))==0) {
        fgets(temp,80,ifile);
        for(i=1;i<=50;i++) {
            fgets(temp,80,ifile);       
            sscanf(temp,"%d",&a1);
            fgets(temp,80,ifile);
            if(place==a1) {
               printf("Match: %d - %d\n",place,a1);
               place = i; 
               break;  }
            else if(place!=a1 && a1==50) {
              printf("No cell data for cell number %d\n",place);
              printf("Using starting cell as default\n");
              fflush(stdin);
              printf("Press return to continue...");
              scanf("%c",&check);
              scanf("%c",&check);
              break;
              }
          }
        }
      }      
	fclose(ifile);
}
