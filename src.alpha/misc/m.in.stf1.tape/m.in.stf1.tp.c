/* extract records from an input file by matching column contents */

#include <stdio.h>
#include "id_section.h"
#define RECLEN 4805
#define MAXLINE 10000
#define NPARS 100

main(argc,argv)
int argc; char *argv[];
{
int actual_len, in_len, count, i, j, k, count_flag, outstyle;
int column[NPARS], not_flag[NPARS], str_len[NPARS];
char id_name[70],buf[MAXLINE],*str[NPARS],*p;

  if (argc < 2 || argc > NPARS-1
          || !strcmp("help",argv[1]) || !strncmp("-help",argv[1],2) ||
		!strcmp("h",argv[1]) )
     usage(argv[0]);

  outstyle = 0;   /* default for full output */

  for (k=1; k < argc; k++) { /* parse the flags */
    if (!strncmp(argv[k],"-",1)) {
      column[k] = 0;
      i = 1;
      while (argv[k][i]) {
        if (argv[k][i] = 'n') outstyle = 1;
        if (argv[k][i] = 'f'){print_idents(); exit(0); }
        i++;
      }
    continue;
    }

    if (sscanf(argv[k],"N%d=%s",&column[k],buf) == 2 ||
        sscanf(argv[k],"%d!=%s",&column[k],buf) == 2 )  not_flag[k]=0;
    else
      if (sscanf(argv[k],"%d=%s",&column[k],buf) == 2 ) not_flag[k]=1;
    else {
      j = -1;
      if       (sscanf(argv[k],"%[A-Z0-9]=%s",id_name,buf) == 2) j = 1;
        else if(sscanf(argv[k],"%[A-Z0-9]!=%s",id_name,buf) == 2) j = 0;
      if (j >= 0) {
        for (i=0; id[i].col != 0; i++)
          if (!strcmp(id_name,id[i].name)) {
            column[k] = id[i].col;
            not_flag[k] = j;
            break;
          }
          if (id[i].col == 0) {
            fprintf(stderr,"\nNo ID SECTION match for name <%s>",id_name);
            exit(1);
          }
      }
      else
        usage(argv[0]);
    }
    if (column[k] < 1 || column[k] > RECLEN*2)  usage(argv[0]);

    p = argv[k]; /* find and save start of compare text */
    while (*p != '=') p++;
    str[k] = ++p;
    str_len[k] = strlen(p);
  } /* end of k loop */

  count_flag = 0;
  count = 0;
               /* process the input lines */
  while ((in_len=get_input_record(stdin,buf+1)) != EOF) {
    count_flag++;
    for (k=1; k < argc; k++){
      if (column[k] > in_len) continue; /* out of range column is ok */
      if (mycmp(str[k],buf+(column[k]),str_len[k],not_flag[k]))
        goto bad;
    } 
    count++;
    if (outstyle==1) {
      printf("%6d %6.6s\n",count_flag-1,buf+19);
      continue;
    }
    actual_len = strlen(buf+1);
    if (!strncmp(buf+1,"STF1",4)) { /* pad STF1 files if necessary */
      while (actual_len < RECLEN) {
        *(buf+actual_len+1) = '0'; actual_len++;
      }
    }
    *(buf+actual_len+1)='\n'; *(buf+actual_len+2)='\0';
    fputs(buf+1, stdout);
  bad: continue;
  }
fprintf(stderr,"\n  %d lines extracted.\n", count);
}

/* compare string p with s for n chars long. Return 0 for good. */
/* f=1 for a match being good; f=0 for a non-match being good */
mycmp(p,s,n,f)
char *p, *s;
int n,f;
{
int k;

  for (k=0; k<n; k++) {
    if (*(p+k) == '?' ) continue;
    if (*(p+k) != *(s+k) ) return f;
  }
  return (f==0? 1 : 0);
}

usage(p)
char *p;
{
char msg[2000];

  sprintf(msg,"\nUSAGE: %s sc=str [ sc=str . . . . ] < infile > outfile\n",p);
  strcat(msg,"Where sc  is a starting column number in each input line,\n");
  strcat(msg,"  (or sc  is a STF1 file IDENTIFICATION SECTION field name),\n");
  strcat(msg,"  and str is a string to match starting at column sc or the field.\n");
  strcat(msg,"Note: sc=str may be repeated to perform multiple tests;\n");
  strcat(msg,"      the multiple tests are 'anded' into a single test.\n");
  strcat(msg,"      '?' may be used as a single character wild card in str.\n");
  strcat(msg,"      When using '?', put sc=scr in quotes\n");
  strcat(msg,"      Preceeding sc by '!' or 'N' reverses sense of the test.");
  strcat(msg,"\n\nExamples:\n");
  strcat(msg,p);
  strcat(msg," SUMLEV=140 < infile > outfile\n");
  strcat(msg," 1=T450 '7=Bu??s' < infile > outfile\n");
  strcat(msg,p);
  strcat(msg," 51=tract N37=9753 < infile > outfile\n");
  strcat(msg,"\nThis program must be run in command mode only.\n\n");
  
  fprintf(stderr,msg);
  exit (1);
}

get_input_record(fp,buffer)
FILE *fp;
char *buffer;
{
register int c,j;
int found_size;
int i,rec_size;

  /* skip <LF> and possible <CR>  and get first char */
  /* anything less than 'space' is terminator char */
  j = 0;
  while (1){
    c = getc(fp);
    if (c == 0) continue; /* ignore nulls */
    if (c == EOF) return (EOF); 
    if (c >= (int) ' '){
      buffer[j++] = c;
      if (j >= MAXLINE) {
        fprintf(stderr,"\nInput line too long (>10000 chars).\n");
        exit (1);
      }
    }
    else if (j > 0) break;
  } /* end of while */


  buffer[j] = '\0';    /* terminate with null */

  return (j-1);
} /* end of get_input_record() */

print_idents()
{
int i;

printf("Identificatio Section:\n");
printf("Data dictionary   Field   Starting\n");
printf("reference name    size    column\n");
printf("\n");

for(i=0; id[i].col != 0; i++)
printf("  %8.8s          %4d      %4d\n",id[i].name,id[i].len,id[i].col);

printf("\n");
printf("Matrix Section follows, beginning in column 301.\n");
}

