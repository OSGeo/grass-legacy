/***************************************************
show_pest()

This function does the following:
1) Gets in the filename from user
2) Checks to see if file is there, reads in number
	of first cell to be shown
3) Counts how many cells in pesticides
4) displays cell pesticide information and prompts
   the user for additional cells to be shown or
   quits

				By:
			Trey Askew
			 June 1996

***************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>

/* Function Declaration  */
int display(int cellNumber, char pestType[], float drain, float run, float sed, float per, int end);

void show_pest(char *input_nps)
{
    FILE *infile;      /*pointer to handle files*/
	
    /*******Variables for table output************* 
          cell_num      cell number
          drain_area    drain area 
          runoff        cell mass in runoff
          sediment      outlet mass in sediment
          percolation   mass in percolate
          pesticide     name of pesticide
   **********************************************/	
	
    float drain_area, runoff, sediment, percolation;
    int cell_num; 
    char pesticide[30]; 
    
    float a3, a4, a5, a6, a7, a8, a9, a10;
    int a1, a2;
    
    char line[100];           /* holds lines read in from file           */
    char cmd[100];            /* for commands to the system              */
    int i;                    /* variable for loops                      */
    int start_cell;           /* holds cell number for initial table     */    
    int hold;                 /* holds the next cell number to be shown  */
    char check;               /* holds character to be checked for count */
    int total=0;              /* holds total number of cells             */
  	
    
    /*This section checks to varify the file and if the file exists
      then the starting cell information for the table is extracted.
    */ 
    if((infile = fopen(input_nps,"r")) == NULL) {
       fprintf(stderr,"Can't open file: %s\n",input_nps);
       sprintf(cmd,"Bad_dir_message.sh %s.nps\n",input_nps); 
       system(cmd); 
       return;
       }
    
    for(i=0;i<5;i++) {
    fgets(line,80,infile);
    }
    sscanf(line,"%f %f %f %f %d", &a1, &a2, &a3, &a4, &start_cell);
    fclose(infile);     
    
	/* This section checks for the total number of cells */
	infile = fopen(input_nps,"r");
	while((fgets(line,80,infile)) != NULL) {
        if((strncmp(line,"PESTICIDE",8)) ==0) {
        fgets(line,80,infile);
        for(i=0;;i++) {
        	fgets(line,80,infile);
        	sscanf(line,"%c", &check);
	        if(check=='*') break;
	        else {
	           fgets(line,80,infile);
	           total++;  }
	       }
	    }  
	 }
	 fclose(infile);
	
	/* This section first checks to see if there is data, then 
	   corrects for agnps files that are skip cells in the output */
	
    if(total!=0){
	infile = fopen(input_nps,"r");
	while((fgets(line,80,infile)) != NULL) {
        if((strncmp(line,"PESTICIDE",8)) ==0) {
        fgets(line,80,infile);
        for(i=1;;i++) {
            fgets(line,80,infile);       
            sscanf(line,"%d",&a1);
            if(start_cell==a1) {
               start_cell = i; 
               break;  }
            else 
              fgets(line,80,infile); 
          }
        }
      }      
	fclose(infile);
    }	
	
	/* clear the users screen and give hold the first cell to display */
	
	system("clear");
    hold = start_cell; 
	
	/* This section gets the information in for the starting cell and 
	   prints it out to the screen, then it prompts the user for the 
	   next cell to be displayed 
	*/
	
	/* checks to see if there is data contained, will not print
	   out anything if the starting cell is greater than the 
	   number of cells counted
	*/
	
	if(total==0) {
	   system("clear");
	   do {
	     printf("\nPesticide data not found\n");
         printf("Press return to continue...");
         scanf("%c", &check);
        }while(check != '\n');    
       }
	
	else {
	 do {
	   infile = fopen(input_nps,"r");
	   while((fgets(line,80,infile)) != NULL) {
         if((strncmp(line,"PESTICIDE",8)) ==0) {
	       fgets(line,80,infile);
	       sscanf(line,"%d %s", &a1, pesticide);     
	       for(i=1; i<hold; i++){
	          fgets(line,80,infile);
	          fgets(line,80,infile);
	          }  
	       fgets(line,80,infile);
	       sscanf(line, "%d %d %f %f %f %f %f %f", &a1, &a2, &a3, &a4, &a5, &a6, &a7, &a8);
	       cell_num = a1;
	       drain_area = a3;
	       runoff = a7;
	       fgets(line,80,infile);
	       sscanf(line, "%f %f %f %f %f %f %f", &a3, &a4, &a5, &a6, &a7, &a8, &a9); 
           sediment = a6;
           percolation = a8;
           }
       }    
         system("clear");
         hold = display(cell_num, pesticide, drain_area, runoff, sediment, percolation,total);
         fclose(infile);
         }while(hold != 0); 
      } 
      
}

int display(int cellNumber, char pestType[], float drain, float run, float sed, float per, int end)
{
	int temp;
	printf("Summary of Pesticide Yields at Outlet Cell:\n\n");
	printf("                             (   cell outlet   )\n");
	printf(" cell	drain  pesticide    mass in	 mass in    mass in\n");
	printf("number  area                runoff      sediment    percolate\n");
	printf("        (ac)                (lb/ac)     (lb/ac)     (lb/ac)\n");
	printf("-------------------------------------------------------------\n");
	printf("  %d     %1.2f   %s       %1.2f       %1.2f        %1.2f\n", cellNumber, drain, pestType, run, sed, per);
	printf("\nTo see output from another cell, enter the cell number [1-%d],\n",end);
	printf("or enter 0 when done viewing cells ==========================> ");
	scanf("%d",&temp);
	while(temp!=0 && (temp<1 || temp>end)) {
	  system("clear");
	  printf("\nThe cell number entered was not within the valid range\n");
	  printf("\nTo see output from another cell, enter the cell number [1-%d],\n",end);
	  printf("or enter 0 when done viewing cells ==========================> ");
	  scanf("%d",&temp);
	 }   
	return temp;
}
