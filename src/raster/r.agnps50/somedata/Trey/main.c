#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>

void main()
{
        char temp[30];
        char check;
        int a; 	
        do {
        	system("clear");
        	system("ls *.nps");
        	printf("\nEnter in filename: ");
        	scanf("%s", temp);
        	strcat(temp,".nps"); 
       	    /* printf("Enter in number: ");
       	    scanf("%d",&a);
       	    find(temp,a); */ 
       	    show_pest(temp);
       	    fflush(stdin);
       	    printf("\n\nAgain? ");
       	    scanf("%c", &check);
       	    scanf("%c", &check);   
		}while(check != 'n');
		system("clear");
}

