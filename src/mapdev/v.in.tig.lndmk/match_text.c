#ifndef INTERPRO
#include <strings.h>
#endif
#include "gis.h"
#include "globals.h"

vask_matches()
{
int i, n;
char check_yes[2], rep_file[60], buf[100];

reset_m_strings();
while (1) {
  *check_yes = '\0';
  *rep_file  = '\0';
  n = 0;
  V_clear();
  V_line (++n,"  Step 1C  Enter text(s) to match in finding Landmark Features.");
  V_line (++n,"           (Use ! as first char to require exact case match.)");
  ++n;
  for (i=0; i<NMATCHES; i++)
    V_ques (match_string[i],'s', ++n, 20, 31);
  ++n; ++n;
  V_line (++n,"           Check for any Landmark Records that match your");
  V_line (++n,"           text strings before proceeding to next step?");
  V_ques (check_yes,'s', n++, 57, 1);
  V_line (++n,"           File to save matches to (omit for screen display)");
  V_ques (rep_file,'s',++n, 12, 50);
  V_intrpt_msg ("ERASE AND START OVER");
  V_intrpt_ok();

  if (! V_call() ) {
    reset_m_strings();
    continue;
  }
  for (i=0; i<NMATCHES; i++)
    G_strip(match_string[i]);
  G_strip(rep_file);
  G_tolcase(check_yes);
  if (*check_yes=='x' || *check_yes=='y') {
    for (i=0; i<NMATCHES; i++)
      if (*match_string[i]) {
        report_matches("");
        if (*rep_file) report_matches(rep_file);
        break;
      }
    if (i>=NMATCHES){
      G_warning("\nYou didn't enter any strings!  <RETURN> to continue...");
      gets(buf);
    }
  }
  else break;
}
if (both_conds() == 3) {
  G_strcpy(check_yes,"n");
  n = 3;
  V_clear();
  V_line(n++,
  "Step 1C: You have selected both CFCC codes and Landmark name strings");
  V_line(++n,
  "         Do you require that a Landmark record match both a selected CFCC");
  V_line(++n,
  "         and one of the specified strings to be selected?");
  V_ques(check_yes,'s',n,49,1);
  n++;
  V_line(++n,
  "         (Otherwise, a match to either will select a record [default].)");
  V_call();
  G_tolcase(check_yes);
  if (*check_yes=='y') both_conds_req = 1;
  else both_conds_req = 0;
}
}

report_matches(ofile)
char *ofile;
{
int pass, num, land;
char buf[100], s[32];
FILE *fp1;

if (*ofile) {
  if (*ofile=='/') G_strcpy(buf,ofile);
  else sprintf(buf,"%s/%s", G_home(),ofile);
  if ((fp1=fopen(buf,"w"))==NULL)
    G_fatal_error("Can't open output file for strings matching list.");
}
else fp1 = stdout;
tig_open('7');
pass = 0;
num = 0;
is_a_match("~!~start!","");
while (get_tiger_record(tiger[7].fp,'7',buf+1) != EOF) {
  pass++;
  G_strncpy(s,buf+25,30);
  G_strip(s);
  if (is_a_match(s,buf+22)) {
    sscanf(buf+11,"%10d", &land);
    if (++num == 1)
      fprintf(fp1,"\nLandmark#  Type  CFCC  Landmark Name\n");
    fprintf(fp1,  "\n%-9d  %.5s %.3s   %s",
      land, *(buf+59)==' '?"AREA ":"POINT", buf+22, s);
  }
}
fprintf(fp1,"\n\n%5d Landmark Records (Type 7) scanned.",pass);
fprintf(fp1,"\n%5d Landmark Records matched your text strings.",num);

if (! *ofile && isatty(fileno(stdin))) {
  printf("\n<RETURN> to continue...");
  gets(buf);
}
if (*ofile) fclose(fp1);
tig_close ('7');
}

is_a_match(s,buff) /* buff is start of CFCC field in input record */
char *s,*buff;
{
register int i;
char *p, *p1, *mcp, stemp[60];
static char mtemp[NMATCHES][32],match_cat[NMATCHES][4];

  if (!strcmp(s, "~!~start!") ) {
    for (i=0; i<NMATCHES; i++) {
      *mtemp[i] = '\0'; *match_cat[i] ='\0';
      G_strncpy(mtemp[i],match_string[i],31);
      if(sscanf(match_string[i],"%[^:]:%s",match_cat[i],stemp)==2) {
        G_strncpy(mtemp[i],stemp,31);
        G_strncpy(match_string[i],mtemp[i],31);
        if (strlen(match_cat[i]) > 3) {
          sprintf(stemp,"String %d ignored, too many characters before :\n",i);
          G_warning(stemp);
          *match_string[i] = '\0';
        }
      }
      else *match_cat[i] = '\0';
      G_tolcase(mtemp[i]);
    }
    return 0;
  }
  if (! *s) return 0;
  G_strcpy(stemp,s);
  G_tolcase(stemp);
  for (i=0; i<NMATCHES; i++) {
    mcp = match_cat[i];
    p1 = match_string[i];
    if (! *p1) continue;   /* skip blank strings */
    if (*p1=='!') {              /* exact case match required */
      p1++;
      if (G_strstr(s,p1)!=NULL){
        if (!*mcp) return 1;    /* p1 substring of s ? */
        else if (!strncmp(mcp,buff,strlen(mcp))) return 1;
      }
        
    }
    else                        /* exact case not req'd, use lc strings */
      if (G_strstr(stemp,mtemp[i]) != NULL) {
        if (!*mcp) return 1;    /* p1 substring of s ? */
        else if (!strncmp(mcp,buff,strlen(mcp))) return 1;
      }
  }
  return 0;  
}

reset_m_strings()
{
int i;
   for (i=0; i<NMATCHES; i++)
     *match_string[i] = '\0';
}

get_m_strings(outname)
char *outname;
{
int i;
char buf[512];

  reset_m_strings();
  if (isatty(fileno(stdin))) {
    printf("\nEnter up to 10 lines of 30 characters as the substrings");
    printf("\nto match to Landmark Names (LANAME).  End with '.end'");
    printf("\n(Use '!' as first character to force exact case match.)");
    printf("\n\n");
  }
  for (i=0;i<=NMATCHES;i++) {
    if (isatty(fileno(stdin)))
      printf("String %2d: ",i+1);
    if (gets(buf) == NULL) break;
    G_strip(buf);
    if (!strncmp(buf,".end",2)) break;
    if (i == NMATCHES) break; /* to allow '.end' after 10th line */
    G_strncpy(match_string[i],buf,30);
  }
  if (isatty(fileno(stdin)))
    if(G_yes("Do you want to see the matching strings before proceeding?",1))
      report_matches("");
  if (*outname) {
    report_matches(outname);
    if (isatty(fileno(stdin)))
      printf("\nMatching Landmark names written to %s\n", outname);
  }
 
}

both_conds() /* see if cats and strings were specified */
{
int i,s,c;
  for (c=0,i=1;i<NCFCC;i++)    if (*want[i])        {c = 1;break;}
  for (s=0,i=0;i<NMATCHES;i++) if (*match_string[i]){s = 2;break;}
return (s+c); /* returns 1 for cats, 2 for strings, 3 for both */
}

good_one(kind,buf,both)
char kind, *buf;
int both;
{
int i, cat_match, string_match;
int offset1;  /* position of CFCC in record */
int offset2;  /* position of FENAME in record */
char strng[40];

  offset1 = 22; offset2 = 25;
  if (kind ==  'L') { offset1 = 56; offset2 = 20; }

  cat_match = 0; string_match = 0;
  if (both==1 || both==3)
    for (i=1; i<NCFCC; i++)
      if (!strncmp(want[i],buf+offset1,3)) { cat_match = 1; break; }
  if (both==2 || both==3) {
    G_strncpy(strng,buf+offset2,30);
    G_strip(strng);
    string_match = is_a_match(strng, buf+offset1);
  }
  if (both==3 &&  both_conds_req && cat_match && string_match)
    return 2;
  if ((both<3 || !both_conds_req)&&(cat_match || string_match))
    return 1;
  return 0;
}
