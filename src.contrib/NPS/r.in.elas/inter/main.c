#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"

int int_prompt(int *, char *, char *, char *, char *);
int ch_prompt(char *, char *, char *, char *, char *);

int main( int argc, char *argv[])
 {
  int exit_status;
  char flag[3];
  int channel;
  int zone;
  int proj;
  char input[1024];
  char output[1024];
  char command[2048];
  int default_int;
  char default_ch[16];
  char question[1024];
  char variable[16];
  char warn_msg[512];

  G_gisinit (argv[0]);

  fprintf(stderr,"\n");
  fprintf(stderr,"INFORMATION:\n");
  fprintf(stderr,"\
%s: To input the values for the OPTIONAL variables: nbpe, x-spot-size,\n\
and y-spot-size, use the non-interactive command:\n\
  r.in.elas [-h] [channel=value] [zone=value] [proj=value] [nbpe=value]\n\
    [x-spot-size=value] [y-spot-size=value] input=name output=name\n\n",
   G_program_name() );
  sprintf(question,"%s","Create histogram file? ");
  default_int = 0;
  exit_status = G_yes(question,default_int);
  if (exit_status==1)
   {
    strcpy(flag,"-h");
   }
  else
   {
    strcpy(flag,"");
   }

  /* Prompt for ELAS channel number. */
  strcpy(default_ch,"1");
  sprintf(variable,"channel");
  sprintf(question,"Enter ELAS channel number. [%s] ",default_ch); 
  sprintf(warn_msg,"is an incorrect value for an ELAS channel number." ); 
  int_prompt(&channel,default_ch,variable,question,warn_msg);

  /* Prompt for UTM zone number. */
  strcpy(default_ch,"0");
  sprintf(variable,"zone");
  sprintf(question,"Enter UTM zone number. [%s] ",default_ch); 
  sprintf(warn_msg,"is an incorrect value for an UTM zone number.");
  int_prompt(&zone,default_ch,variable,question,warn_msg);

  /* Prompt for GRASS "proj" value. */
  strcpy(default_ch,"1");
  sprintf(variable,"proj");
  sprintf(question,"Enter GRASS \"proj\" value (0 for XY and 1 for UTM)\
. [%s] ",default_ch); 
  sprintf(warn_msg,"is an incorrect value for a GRASS \
 \"proj\" value.\n\"proj\" value is 0 for XY and 1 for UTM.");
  int_prompt(&proj,default_ch,variable,question,warn_msg);

  /* Prompt ELAS input raster file name. */
  strcpy(default_ch,"");
  sprintf(variable,"input");
  sprintf(question,"Enter ELAS input raster file name. ");
  sprintf(warn_msg,"You have failed to provide any file name \
for the ELAS input raster file.");
  ch_prompt(input,default_ch,variable,question,warn_msg);

  /* Prompt for GRASS output raster file name. */
  strcpy(default_ch,"");
  sprintf(variable,"output");
  sprintf(question,"Enter GRASS output cell file name. ");
  sprintf(warn_msg,"You have failed to provide any file name \
for the GRASS output cell file.");
  ch_prompt(output,default_ch,variable,question,warn_msg);

  sprintf(command,"%s/bin/%s %s channel=%d zone=%d proj=%d input=%s output=%s",
  G_gisbase(), G_program_name(), flag, channel, zone, proj, input, output);
  system(command);
  exit(0);
 }


int 
int_prompt (int *answer_int, char *default_ch, char *variable, char *question, char *warn_msg)
 {
  int loop;
  int scan_int;
  char verify[16];
  char answer[512];

  loop = 1;
  do 
   {
    fprintf(stderr,"\n");
    fprintf(stderr,"%s",question); 
    fgets(answer,510,stdin);
    if (strcmp(answer,"")==0)
      strcpy(answer,default_ch);
    scan_int=sscanf(answer,"%d",answer_int);
    if (scan_int != 1)
     {
      fprintf(stderr,"WARNING:\n%s: \"%s\" %s\n",
              G_program_name(),answer,warn_msg );
      loop = 1;
     }
    else
     {
      fprintf(stderr,"\n");
      fprintf(stderr,"You have chosen:\n  %s=%d\nIs this correct? (y/n)\
 [y] ",variable,*answer_int);
      fgets(answer,510,stdin);
      if (strcmp(answer,"")==0)
        strcpy(answer,"y");
      strcpy(verify,answer);
      if ( (strcmp(verify,"y")==0) ||
           (strcmp(verify,"Y")==0) ||
           (strcmp(verify,"yes")==0) ||
           (strcmp(verify,"YES")==0) ||
           (strcmp(verify,"Yes")==0)   )
       {
        loop = 0;  /* Exit from "do while" loop.  */
       }
      else 
       {
        loop = 1;  /* Continue looping in "do while" loop. */
       }
     }
   }
  while (loop == 1);
  return(0);
 }


int 
ch_prompt (char *answer_ch, char *default_ch, char *variable, char *question, char *warn_msg)
 {
  int loop;
  char verify[16];
  char answer[512];

  loop = 1;
  do 
   {
    fprintf(stderr,"\n");
    fprintf(stderr,"%s",question); 
    fgets(answer,510,stdin);
    if (strcmp(answer,"")==0)
      strcpy(answer,default_ch);
    /* "strcmp" will be equal to 0 only if a NULL value has been */
    /* initialized in variable "default_ch".                     */ 
    if (strcmp(answer,"")==0)
     {
      fprintf(stderr,"WARNING:\n%s:\n%s\n",
              G_program_name(),warn_msg );
      strcpy(answer_ch,"");
      loop = 1;
     }
    else
     {
      strcpy(answer_ch,answer);
      fprintf(stderr,"\n");
      fprintf(stderr,"You have chosen:\n  %s=%s\nIs this correct? (y/n)\
 [y] ",variable,answer_ch);
      fgets(answer,510,stdin);
      if (strcmp(answer,"")==0)
        strcpy(answer,"y");
      strcpy(verify,answer);
      if ( (strcmp(verify,"y")==0) ||
           (strcmp(verify,"Y")==0) ||
           (strcmp(verify,"yes")==0) ||
           (strcmp(verify,"YES")==0) ||
           (strcmp(verify,"Yes")==0)   )
       {
        loop = 0;  /* Exit from "do while" loop.  */
       }
      else 
       {
        loop = 1;  /* Continue looping in "do while" loop. */
       }
     }
   }
  while (loop == 1);
  return(0);
 }
