                   /* Read option file .pg-grc 
		      Options - database location 
		                default table     */
#include <stdio.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include "sql.h"

void get_post_head(post)
     struct Postgres *post;
{
 
  
  char line[80];
  struct passwd *pw ;
  uid_t uid;
  gid_t gid;
  
 
  
  FILE *post_init;
  char input[30];
  char arg[30];
  char val[30];
  char *ch;
  
  uid=getuid();
  pw=getpwuid(uid);
  sprintf(line,"%s/.pg-grc",pw->pw_dir);

  post_init=fopen(line,"r");
  if (post_init==NULL) {puts("Arghh");exit(8);}
  while(ch != EOF){
    ch=fgets(input,sizeof(input),post_init);
    if (ch==NULL) break;
    sscanf(input,"%[^:]:%s",arg,val);
    if (strcmp(arg,"host") == 0) {strcpy(post->host,val); }
    if (strcmp(arg,"port") == 0) {strcpy(post->portnum,val);}
    if (strcmp(arg,"database") == 0) {strcpy(post->database,val);}
    if (strcmp(arg,"table") == 0 ){strcpy(post->def_table,val);}
    
  }
  
  fclose(line);
}
