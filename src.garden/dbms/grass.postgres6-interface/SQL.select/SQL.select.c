/* Select database, table other stuff */


#include <stdio.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include "sql.h"

void display_current(post)

struct Postgres *post;
{
   printf("\n SQL.select current settings \n");
  printf(" Database -- %s\n",post->database);
  printf(" Table -- %s\n",post->def_table);
  printf(" Postgres host %s\n",post->host);
  printf(" Postgres port %d\n\n",post->portnum);

  printf("1.\tEnter new database\n2.\tEnter new Table to use\n");
  printf("3.\tEnter new Postgres Host\n4.\tEnter new port\n5.\tExit\n");
}

void main()
{
  int option;
  char line[40];
    
  struct Postgres postgres;
  get_post_head(&postgres);

  printf("\n SQL.select current settings \n");
  printf(" Database -- %s\n",postgres.database);
  printf(" Table -- %s\n",postgres.def_table);
  printf(" Postgres host %s\n",postgres.host);
  printf(" Postgres port %s\n\n",postgres.portnum);

  printf("1.\tEnter new database\n2.\tEnter new Table to use\n");
  printf("3.\tEnter new Postgres Host\n4.\tEnter new port\n5.\tExit\n");
  printf(">");
  while(option != '5') {
    
    option=getchar();
    
    switch (option) {
      
    case '1': {
      printf("New Database name or <Enter> to list databases:");
      fgets(line,sizeof(line),stdin);
      fgets(line,sizeof(line),stdin);
      line[strlen(line)-1]='\0';
      strcpy(postgres.database,line);
      display_current(&postgres);
      break;
      
    }
    
    case '2': {
      printf("New Table name: ");
      fgets(line,sizeof(line),stdin);
      fgets(line,sizeof(line),stdin);
      line[strlen(line)-1]='\0';
      strcpy(postgres.def_table,line);
      display_current(&postgres);
      break;
    }
    
    case '3' : {
      printf("New Postgres host: ");
      fgets(line,sizeof(line),stdin);
      fgets(line,sizeof(line),stdin);
      line[strlen(line)-1]='\0';
      strcpy(postgres.host,line);
      display_current(&postgres);
    }

    case '4' : {
      printf("New Port (normally 5432): ");
      fgets(line,sizeof(line),stdin);
      fgets(line,sizeof(line),stdin);
      line[strlen(line)-1]='\0';
      strcpy(postgres.portnum,line);
      display_current(&postgres);
    }
     
    

    }

  }
  /* Write results to file --
     future, check data */

write_post_file(&postgres);

}

  
  
