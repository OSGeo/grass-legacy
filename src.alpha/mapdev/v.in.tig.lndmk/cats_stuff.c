#include "gis.h"
#include "globals.h"
#include "cats.h"

static int nrec, count, n_areas=0, n_points=0, n_lines=0;
static int n_line_cat[MAXNCFCC], n_point_cat[MAXNCFCC], n_area_cat[MAXNCFCC];

reset_cats()
{
int i;
  NCFCC = sizeof(cfcc)/sizeof(struct cc);
  for (i=0; i<NCFCC; i++) {
/*    *cat[i] = '\0'; */
    *want[i] = '\0'; }
  count = 0; nrec = 0; n_areas = 0; n_points = 0; n_lines = 0;
}

#define PPG 19
ask_cfcc_list()  /* from vask */
{
int i,n,k;
char line[25][80];
char reset_yes[2],done_yes[2];

restart:
for (i=0;i<=NCFCC;i++) want[i][0]='\0';
show_list("!"); /* get existing A and P CFCC's */

restart1:
for (k=0; k<3; k++){
  if (k==0 && *feature_type[1]!='P') continue;
  if (k==1 && *feature_type[2]!='A') continue;
  if (k==2 && *feature_type[3]!='L') continue;
  i = 0;
  while (i<NCFCC) {
    *reset_yes = '\0';  *done_yes = '\0';
    if (k==2)
      strcpy(line[0], "             Type 1 CFCC SELECTION MENU");
    else
      strcpy(line[0], " LANDMARK FEATURE CATEGORY SELECTION MENU: ");
    if (k==0) strcat(line[0],"POINT LANDMARKS");
    if (k==1) strcat(line[0],"AREA LANDMARKS");
 
    V_clear();
    V_line(0,line[0]);

    n = 2;
    while (n <= PPG) {
      if (++i == (NCFCC-1)) {i++; break;}
      if ((k==0 && cat[i][2]=='P') || (k==1 && cat[i][1]=='A') ||
          (k==2 && cat[i][0]=='*') ){
        sprintf(line[n],"%3s   %s",cfcc[i].code,cfcc[i].desc);
        V_line(n,line[n]);
        V_ques(want[i],'s',n,4,1);
        n++;
      }
    }
    V_line(++n,"Done:    Start over:");
    V_ques(done_yes,'s',n,6,1);
    V_ques(reset_yes,'s',n,21,1);

    /* Show screen and get choice */
    V_intrpt_ok();		/* allow for a bailout */
    if (! V_call() ) return (-1);

    if (*done_yes)  break;
    if (*reset_yes) goto restart;
  }
}
  sub_actual_cat();
return 1;
} /* end of ask_cfcc_list() */

sub_actual_cat() /* put the actual 3 letter CFCC in want array */
{
int i;

  for (i=1; i<NCFCC; i++) 
   if (*cat[i] == 'x' || *want[i] == 'x') 
     strcpy(want[i],cfcc[i].code);
   else *want[i] = '\0';
}


show_list(f_nme) /* f_nme ="!" to just make list */
char *f_nme;
{
register int i;
char *file_out, list_yes[2],file_yes[20], buf[300], cmd[300];
FILE *fp1;

if (*feature_type[3]=='L')
  if (n_lines==0) {
  for (i=0;i<MAXNCFCC;i++) n_line_cat[i] = 0;
  reset_cats();
  }
  else {
    nrec = 0; count = 0;
    for (i=0;i<MAXNCFCC;i++)
      if (n_line_cat[i]) {nrec += n_line_cat[i]; count++; }
  }
if (*feature_type[2]=='A' || *feature_type[1]=='P')
  if (n_areas==0 || n_points==0) {
    for (i=0;i<MAXNCFCC;i++) {n_area_cat[i] = 0; n_point_cat[i] = 0;}
    reset_cats();
  }
  else {
    nrec = 0; count = 0;
    for (i=0;i<MAXNCFCC;i++)
      if (n_area_cat[i] || n_point_cat[i])
        { nrec += (n_line_cat[i] + n_point_cat[i]); count++; }
  }

if (feature_type[3][0]=='L' && n_lines==0) { /* Do Type 1 records */
  if (*f_nme != '!')
    fprintf(stderr,"\nScanning Type 1 file ... Be patient ...");
  tig_open('1');
  while (get_tiger_record(tiger[1].fp,'1',buf+1) != EOF) {
    nrec++;
    for (i=1; i<NCFCC; i++) {
      if (! strncmp(buf+56, cfcc[i].code, 3) ){ 
        if (*cat[i] == '\0') { /* if first time */
          count++;             /* count it */
          strcpy (cat[i],"*  ");/* initialize it */
        }
        n_line_cat[i]++;   /* count this category */
        n_lines++;
      }
    }       
  }
  tig_rewind('1');
}
else 
 if((*feature_type[2]=='A' && n_areas==0) || 
    (*feature_type[1]=='P' && n_points==0)) {
  if (*f_nme != '!')
    fprintf(stderr,"\nScanning Type 7 file ...");
  tig_open('7');
  while (get_tiger_record(tiger[7].fp,'7',buf+1) != EOF) {
    nrec++;
    for (i=1; i<NCFCC; i++) {
      if (! strncmp(buf+22, cfcc[i].code, 3) ){ 
        if (*cat[i] == '\0') { /* if first time */
          count++;             /* count it */
          strcpy (cat[i],"*  ");/* initialize it */
        }
        if (buf[59]==' ') {
          cat[i][1] = 'A';      /* mark as area record */
          n_area_cat[i]++;   /* count this category */
          n_areas++;
        }
        else {
          cat[i][2] = 'P';      /* mark as point landmark */
          n_point_cat[i]++;   /* count this category */
          n_points++;
        }
        break;
      }
    }
  }
  tig_rewind('7');
}
if (*f_nme == '!') return (count);

while(1){
  if (feature_type[3][0]=='L')  /* Do Type 1 records */
    sprintf(buf,"TIGER Type 1 file <%s>",tiger[1].name);
  else
    sprintf(buf,"TIGER Type 7 file <%s>",tiger[7].name);
  sprintf( cmd,"examined.  %d unique CFCC codes found in %d records.",
      count, nrec);
  *list_yes = '\0'; *file_yes = '\0';
  V_clear();
  V_line (1, buf);
  V_line (2, cmd);
  V_line (4,"Mark 'x' to see list on screen");
  V_line (6,"Enter file name (home directory) to save list to file");
  V_line (15,"Categories found will be included in CFCC Selection Menu");
  V_line (16,"unless you use CTRL-C to exit from this screen");

  V_ques(list_yes,'s',4,32,1);
  V_ques(file_yes,'s',6,55,14);
    /* Show screen and get choice */
  if (! *f_nme) {
    V_intrpt_ok(); /* allow for a bailout */
    if (! V_call() ) return (-1);
  }

  if (*file_yes) file_out = file_yes;
    else file_out = f_nme;
  if (*file_out) {
    G_squeeze(file_out);
    if (*file_out =='/') strcpy(cmd,file_out);
    else
      sprintf(cmd,"%s/%s",G_home(),file_out);
    if ((fp1=fopen(cmd,"w"))==NULL) {
      strcat(cmd,"\nRequested CFCC list output file could not be opened.");
      G_warning(cmd);
      *file_yes = '\0';
    }
    else {
      fprintf(fp1,"%s\n",buf);
      if (feature_type[3][0]=='L') {  /* Do Type 1 records */
        fprintf(fp1,"Number of Each Type 1 Line CFCC");
        for (i=1; i<NCFCC; i++) 
          if (n_line_cat[i])
            fprintf(fp1,"\n%6d %3s %s",
              n_line_cat[i],cfcc[i].code,cfcc[i].desc);
      }
      else {
        fprintf(fp1,"Number of Each Area Landmark CFCC");
      for (i=1; i<NCFCC; i++) 
        if (n_area_cat[i])
          fprintf(fp1,"\n%6d %3s %s",
            n_area_cat[i],cfcc[i].code,cfcc[i].desc);
      fprintf(fp1,"\nTotal Area Records: %d\n", n_areas);
      fprintf(fp1,"\nNumber of Each Point Landmark CFCC",buf);
      for (i=1; i<NCFCC; i++) 
        if (n_point_cat[i])
          fprintf(fp1,"\n%6d %3s %s",
            n_point_cat[i],cfcc[i].code,cfcc[i].desc);
      fprintf(fp1,"\nTotal Point Records: %d\n", n_points);
      }
      fclose(fp1);
      printf ("  CFCC's in\n%s\nwritten to <%s>\n\n", buf, cmd);
      sleep(3);
    }
    if (*f_nme) return (1);
  }
  if (*list_yes) {
      fp1=fopen(file_out=G_tempfile(),"w");
      fprintf(fp1,"%s\n",buf);
      if (feature_type[3][0]=='L') {  /* Do Type 1 records */
        fprintf(fp1,"Number of Each Type1 Line CFCC");
        for (i=1; i<NCFCC; i++) 
          if (n_line_cat[i])
            fprintf(fp1,"\n%6d %3s %s",
              n_line_cat[i],cfcc[i].code,cfcc[i].desc);
      }
      else {
        fprintf(fp1,"Number of Each Area Landmark CFCC",buf);
      for (i=1; i<NCFCC; i++) 
        if (n_area_cat[i])
          fprintf(fp1,"\n%6d %3s %s",
            n_area_cat[i],cfcc[i].code,cfcc[i].desc);
      fprintf(fp1,"\nTotal Area Records: %d\n", n_areas);
      fprintf(fp1,"\nNumber of Each Point Landmark CFCC",buf);
      for (i=1; i<NCFCC; i++) 
        if (n_point_cat[i])
          fprintf(fp1,"\n%6d %3s %s",
            n_point_cat[i],cfcc[i].code,cfcc[i].desc);
      fprintf(fp1,"\nTotal Point Records: %d\n", n_points);
      }
      fclose(fp1);
      sprintf(cmd,"cat %s | more",file_out);
      G_system(cmd);
      printf("\nEnter RETURN to continue...");
      gets(cmd);
      unlink(file_out);
  }
  if (*list_yes == '\0' && *file_yes == '\0') return (1);
} /* end of while */
}

decode(p)  /* read line for Dxx strings */
char *p;
{
int i, ncode;
char *skip_to_CC();

  while ((p=skip_to_CC(p))!= NULL && (sscanf(p+1,"%2d",&ncode)==1) ) {
    for (i=1; i<NCFCC; i++) {
      if (!strncmp(p,cfcc[i].code,3)) { /* got a match */
        want[i][0] = 'x'; want[i][1] = '\0';
        break;
      }        
    }
    p += 3;
  }
}

char *
skip_to_CC(s)
char *s;
{
  while (*s){
    if (*s >= 'A' && *s <= 'Z') return s;
    else s++;
  }
  return ((char *) NULL);
}
