#include <stdio.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include "sql.h"


void write_post_file(post)
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

  post_init=fopen(line,"w");
  if (post_init==NULL) {puts("Arghh");exit(8);}

  /* Write Structure Postgres in for label:value */

  fprintf(post_init,"host:%s\n",post->host);
  fprintf(post_init,"port:%d\n",post->portnum);
  fprintf(post_init,"database:%s\n",post->database);
  fprintf(post_init,"table:%s\n",post->def_table);
  fclose(post_init);

}
