
#include "gis.h"
#include "globals.h"

/*
   Parse input from file then stdin (if stdinflag set)
   Interpret the following keywords:
    .points [site=site_map]
       lines containing requested CFCC Codes
    .end

    .areas [vect=vect_map]
       lines containing requested CFCC Codes
    .end

    .lines [vect=vect_map]
       lines containing requested CFCC Codes
    .end

    .strings [file=output_file]
       up to 10 lines of 30 chars to match with Landmark names
  
    .codes [p|a|l=output_file]

    .exit

 For each points, areas, lines, set up cat array and match_string array
 and process to make a map.
*/

process(fp) /* fp = stdin, otherwise file input */
FILE *fp;
{
int fun,stop,n;
char com[50],f_type[50],f_name[50];
char buf[512];

  *feature_type[0] = '\0';
  *feature_type[1] = '\0';
  *feature_type[2] = '\0';
  *feature_type[3] = '\0';
  *vect_name = '\0';
  *site_name = '\0';

  reset_m_strings();
  reset_cats();
  both_conds_req = 0;
  fun = 0;
  stop = 0;

  while (1) {
    if (fp != stdin) {
      if (fgets(buf, 512, fp)==NULL) return 0;
      buf[strlen(buf)-1] = '\0';
    }
    else {
      if (isatty(fileno(stdin)))
        if (!fun) {
          printf(".p(oints)  .a(reas)     .l(ines) .c(ode)list\n");
          printf(".s(trings) .m(atch)both .e(nd)   .ex(it) ");
        }
        else printf("CFCC");
      printf(">>");
      if (gets(buf)==NULL) return 0;
    }
    G_strip(buf);
    if (*buf == '!') { G_system(buf+1); continue; }
    G_squeeze(buf);
    if (! *buf) continue;  /* blank line */
    if (*buf == '.') {
/* decode strings of the form: .word key=string or: .word string */
/*   .word deposited in com, string in f_name, key in f_type  */
      *com = *f_type = *f_name = '\0';
      if (2 == sscanf(buf,"%s %[^=]=%s",com,f_type,f_name))
      { /* key omitted */
        strcpy(f_name,f_type);
        *f_type = '\0';
      }
      G_tolcase(com);
      G_tolcase(f_type);

      if (!strncmp(com,".ex",3)) return (0);;

      if (!strncmp(com,".c",2) ) { /* show list of codes */
        n = 0;
        if (*f_type=='\0') {strcpy(f_type,f_name); *f_name='\0';}
        G_toucase(f_type);
        if (*f_type=='P') { *feature_type[1]='P'; n++; }
        if (*f_type=='A') { *feature_type[2]='A'; n++; }
        if (*f_type=='L') { *feature_type[3]='L'; n++; }
        if (!n) { *feature_type[1]='P'; n++; *f_name = '\0'; }
        show_list(f_name);
        *feature_type[1] = '\0';
        *feature_type[2] = '\0';
        *feature_type[3] = '\0';
        continue;
      }
      if (!strncmp(com,".m",2) ) { /* set for codes and strings matched */
        both_conds_req = 1;
        G_tolcase(f_name);
        if (*f_name=='n') both_conds_req = 0;
        if (isatty(fileno(stdin))) {
         printf("Matches to both CFCC codes and specified strings (if any)\n");
         printf("will be required to select a Landmark. [Turn off with .m no]\n");
        }
        continue;
      }
      if (!strncmp(com,".p",2)) { /* points */
        if (*f_name)
          strcpy(site_name,f_name);
        fun = 1;stop=0;continue;
      }
      if (!strncmp(com,".a",2)) { /* areas */
        if (*f_name) strcpy(vect_name,f_name);
        fun = 2;stop=0;continue;
      }
      if (!strncmp(com,".l",2)) { /* lines  */
        if (*f_name) strcpy(vect_name,f_name);
        fun = 3;stop=0;continue;
      }
      if (!strncmp(com,".s",2)) { /* strings for matching  */
        get_m_strings(f_name);stop=0;continue;
      }
      if (!strncmp(com,".e",2)) stop = 1;
    }
    else  if(fun) {decode(buf); continue;} /* extract any CFCC's */
    if (stop) {
      sub_actual_cat();
      switch(fun) {
        case 0: continue;
        case 1: if (! *site_name)
                  if (opts->answer != NULL) 
                    G_strncpy(site_name,opts->answer,49);
                if (! *site_name)
                  G_fatal_error("Must give site=map for .points map");
                *feature_type[1] = 'P';
                break;
        case 2: if (! *vect_name)
                  if (optv->answer != NULL)
                    G_strncpy(vect_name,optv->answer,49);
                if (! *vect_name)
                  G_fatal_error("Must give vect=map for .area map");
                *feature_type[2] = 'A';
                break;
        case 3: if (! *vect_name)
                  if (optv->answer != NULL)
                    G_strncpy(vect_name,optv->answer,49);
                if (! *vect_name)
                  G_fatal_error("Must give vector=map for .line map");
                *feature_type[3] = 'L';
                break;
      }
      return 1;
    }
  }
}

