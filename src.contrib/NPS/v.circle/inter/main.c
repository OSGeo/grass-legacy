
/* Exit status of 0 indicates program was successful.     */
/* Exit status of 1 indicates program was not successful. */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

static int double_prompt(double *, char *, char *, char *, char *);
static int ch_prompt(char *, char *, char *, char *, char *);

int main( int argc, char *argv[])
 {
  char sitefile[1024];
  char output[1024];
  char command[2048];
  char default_ch[16];
  char question[1024];
  char variable[16];
  char warn_msg[512];
  char r_or_a[16];
  double radius;
  char radius_uom[16];
  double area;
  char area_uom[16];
  char prompt[1024];
  char *site_mapset;
  char *vect_mapset;
  int exit_status;
  char flag[3];
  int default_int;


  G_gisinit (argv[0]);

/* Make sure that the current projection is UTM or   */
/* Unreferenced XY projection.                       */
  if ((G_projection() != 0)&&(G_projection() != 1))
   {
    char msg[256];      
    sprintf(msg,"%s:  Projection must be either Unreferenced XY (value 0) or \
UTM (value 1).  Change the value \"proj\" in file \"WIND\" to either \
0 or 1 and then re-executed \"%s\".",
    G_program_name(), G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Prompt for "-s" flag to determine whether to run "v.support". */
  sprintf(question,"%s",
"Automatically run \"v.support\" on newly created vector file? ");
  default_int = 0;
  exit_status = G_yes(question,default_int);
  if (exit_status==1)
   {
    strcpy(flag,"-s");
   }
  else
   {
    strcpy(flag,"");
   }

  /* Ask whether to use "radius" or "area" values for calculating the */
  /* circles.                                                         */
  strcpy(default_ch,"r");
  sprintf(variable,"answer");
  sprintf(question,"Please indicate whether radius or area values will be \
used for calculating the circles.  Enter (\"r\" or \"a\"): ");
  sprintf(warn_msg,"You have failed to enter an \"r\" or \"a\", \
\"r\" selected by default.");
  ch_prompt(r_or_a,default_ch,variable,question,warn_msg);
  if ((strcmp(r_or_a,"r")!=0)&&(strcmp(r_or_a,"a")!=0))
   { 
    strcpy(r_or_a,"r");
    fprintf(stderr,"WARNING:\n%s: \"%s\" selected by default.\n",
            G_program_name(), r_or_a );
   }

  if ( strcmp(r_or_a,"r")==0 ) 
   {
    /* Prompt for "radius" value. */
    strcpy(default_ch,"0.0");
    sprintf(variable,"radius");
    sprintf(question,"Enter radius value for circle(s):  [%s] ",default_ch); 
    sprintf(warn_msg,"is an incorrect value for radius value.");
    double_prompt(&radius,default_ch,variable,question,warn_msg);
    /* Make sure that radius is not less than or equal to 0.0 . */
    if (radius <= 0.0)
     {  
      char msg[256];
      sprintf(msg,"%s: \"%f\" is an incorrect value for radius.\n",
              G_program_name(), radius );
      G_fatal_error (msg);
      exit(1);
     }  
    /* Prompt for "radius_uom". */
    strcpy(default_ch,"m");
    sprintf(variable,"radius_uom");
    sprintf(question,"Enter radius unit of measure, ie. m(meters), ft(feet), \
mi(miles):  [%s] ",default_ch);
    sprintf(warn_msg,"is an incorrect value for a radius unit of measure.");
    ch_prompt(radius_uom,default_ch,variable,question,warn_msg);
    if ( (strcmp(radius_uom,"m")!=0)&&(strcmp(radius_uom,"ft")!=0)
          &&(strcmp(radius_uom,"mi")!=0) )
     {
      char msg[256];
      sprintf(msg,"%s: \"%s\" is an incorrect unit of measure for radius.  \
Please use one these units of measure:  \"m\" (for meters), or \"ft\" \
(for feet), or \"mi\" (for miles)",
              G_program_name(), radius_uom);
      G_fatal_error (msg);
      exit(1);
     }
   }
  else
   {
    /* Prompt for "area" value. */
    strcpy(default_ch,"0.0");
    sprintf(variable,"area");
    sprintf(question,"Enter area value for circle(s):  [%s] ",default_ch); 
    sprintf(warn_msg,"is an incorrect value for area value.");
    double_prompt(&area,default_ch,variable,question,warn_msg);
    /* Make sure that area is not less than or equal to 0.0 . */
    if (area <= 0.0)
     {  
      char msg[256];
      sprintf(msg,"%s: \"%f\" is an incorrect value for area.\n",
              G_program_name(), area);
      G_fatal_error (msg);
      exit(1);
     }  
    /* Prompt for "area_uom". */
    strcpy(default_ch,"sqm");
    sprintf(variable,"area_uom");
    sprintf(question,"Enter area unit of measure, ie. sqm(square meters), \
ac(acres), sqmi(square miles), hec(hectares): [%s] ",default_ch);
    sprintf(warn_msg,"is an incorrect value for a area unit of measure.");
    ch_prompt(area_uom,default_ch,variable,question,warn_msg);
     if ( (strcmp(area_uom,"sqm")!=0)&&(strcmp(area_uom,"sqmi")!=0)&&
           (strcmp(area_uom,"ac") !=0)&&(strcmp(area_uom,"hec") !=0)   )
       {
        char msg[256];     
        sprintf(msg,"%s: \"%s\" is an incorrect unit of measure for area.  \
Please use one  these units of measure:  \"sqm\" (for square meters), or \
\"sqmi\" (for square miles),  or \"ac\" (for acres), or \"hec\" (for hectares)",
                G_program_name(), area_uom );
        G_fatal_error (msg);
        exit(1);
       }
   }

  /* Prompt for site_lists file name. */
  strcpy(prompt,"");
  site_mapset = G_ask_sites_old(prompt,sitefile);
  if (site_mapset == NULL)
   {
    char msg[256];      
    sprintf(msg,"%s: Unable to obtain site_lists file: <%s>.", 
            G_program_name(), sitefile );
    G_fatal_error (msg);
    exit(1);
   }

  /* Prompt for output file name (vector file). */
  strcpy(prompt,
"Enter vector file to be created (output)");
  vect_mapset = G_ask_vector_new(prompt,output);
  if (vect_mapset == NULL)
   {
    char msg[256];      
    sprintf(msg,"%s: Unable to obtain vector file: <%s>.", 
            G_program_name(), output );
    G_fatal_error (msg);
    exit(1);
   }

  /* Create "command" line. */
  if ( strcmp(r_or_a,"r")==0 ) 
   {
  sprintf(command,"%s/bin/%s %s radius=%f radius_uom=%s sitefile=%s output=%s",
  G_gisbase(), G_program_name(), flag, radius, radius_uom, sitefile, output);
   }
  else
   {
    sprintf(command,"%s/bin/%s %s area=%f area_uom=%s sitefile=%s output=%s",
    G_gisbase(), G_program_name(), flag, area, area_uom, sitefile, output);
   }
  system(command);
  exit(0);
 }


static int double_prompt (double *answer_double, char *default_ch,
  char *variable, char *question, char *warn_msg)
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
    *strchr(answer,'\n')=0;
    if (strcmp(answer,"")==0)
      strcpy(answer,default_ch);
    /* "scan_int" will not be equal to 1 only if "default_ch" has been */
    /* initialized with a non-numeric character string value.          */
    scan_int=sscanf(answer,"%lf",answer_double);
    if (scan_int != 1)
     {
      fprintf(stderr,"WARNING:\n%s: \"%s\" %s\n",
              G_program_name(),answer,warn_msg );
      loop = 1;
     }
    else
     {
      fprintf(stderr,"\n");
      fprintf(stderr,"You have chosen:\n  %s=%f\nIs this correct? (y/n)\
 [y] ",variable,*answer_double);
      fgets(answer,510,stdin);
      *strchr(answer,'\n')=0;
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


static int ch_prompt (char *answer_ch, char *default_ch,
  char *variable, char *question, char *warn_msg)
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
    *strchr(answer,'\n')=0;
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
      *strchr(answer,'\n')=0;
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
