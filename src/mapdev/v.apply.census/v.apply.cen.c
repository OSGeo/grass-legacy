/*  This program:
        Opens file in first parameter for reading (stf1 file, or subset)
	If necessary, reads stdin for record numbers to process.
	extracts "east" and "north" coordinates,
	extracts contents of field named in second parameter (record),
	reads the two line stf record in buffer (buf),
	reads remaining parameters and concatenates them into
		a string (formula),
	substitutes fields in formula with values from buf,
	sends coordinates and formula to bc -l | paste - - - > file or stdout
	loops for all standard input or stf1 records

    Can be called from s.db.rim (after a query) by:
	.out |s.in.stf1
          in=stf.file id=record_field out=att|sites|-|table:Lnnnn name=formula
	.p -a
	.out
     or by sending output to a file and later cat file | . . . . .

     For LL to UTM conversions, the spheroid specified by the SPHEROID
        environment variable is used; if no SPHEROID env. variable is
        defined, "clark66" is used.  The zone used is that returned
        by G_zone() (the default zone for the LOCATION), unless a
        UTM_ZONE env. variable is set.  In looking for these environment
        variables, the current environment is searched first, then
        the list in ~/.grassrc is searched.
*/
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include "gis.h"
#include "CC.h"
#include "id_section.h"
#include "matrix.h"
#include "pl94_matrix.h"

#define STD 0
#define ATTS 1
#define SITE 2
#define TABLE 3
#define NONSTF 4
#define MATRIX_REC 4807
#define TFIELDS 100
struct Flag *flag1,*flag2,*flag3,*flag4;
struct Option *opt1,*opt2,*opt3,*opt4,*opt5,*optz,*optsph;

static int record;
static int count_all;
static int stf1_flag = 0;
static int pl94_flag = 0;
static char *argv0;

int pmt(char *);
int set_otype(char *, int *, int *);
int get_stf_record(FILE *, char *);
int extract_value(char *, char *);
int verify_tmpfile(char *, char *, int);
int sub_for_mtx(char *);
int sub_case(char *);
int verify_formula(char *);
int get_utms(long, long, double *, double *, int *, char *);
int usage(char *);
int parse_input(int, char *[]);

int main (int argc, char *argv[])
{
  struct GModule *module;
  FILE *fp_stf, *bc_pipe, *table_fp, *if_fp;
  long lat, lon;
  int output_type,i,column,zone;
  int branch;
  int table_cars[TFIELDS], table_column[TFIELDS];
  double e,n;
  char buf[10000];
  char line[520], bc_string[520];
  char field[20], formula[600];
  char table_string[9*TFIELDS];
  char filename[20], e_n_fmt[20];
  char *fpoint, *p, *s, *tmp_file, *tmp_file1, *tmp_file2;
  char c;

  G_gisinit (argv0=argv[0]);

  module = G_define_module();
  module->description =
	"Calculate/Import Demographics from Census STF1 Files.";

    /* Print TABLES  and exit */
    /*
  if (argc>1 && !strcmp(argv[1],"-d")) pmt("stf1_id");
  if (argc>1 && !strcmp(argv[1],"-p")) pmt("stf1_matrix");
  if (argc>1 && !strcmp(argv[1],"-l")) pmt("pl94_matrix");
  if (argc>1 && !strcmp(argv[1],"-u")) pmt("stf1_sumlev");
  */

  parse_input(argc,argv);

    /* Print TABLE and exit */
  if (flag1->answer) pmt("stf1_id");
  if (flag2->answer) pmt("stf1_matrix");
  if (flag3->answer) pmt("pl94_matrix");
  if (flag4->answer) pmt("stf1_sumlev");

  bc_pipe = NULL;
  fp_stf  = NULL;
  if_fp   = NULL;
  output_type = 0;

  /* Decode input parameters */

  if (opt3->answer == NULL && opt4->answer == NULL)
    usage("No \"map=formula\" or expression input file given.");
  if (opt3->answer != NULL && opt4->answer != NULL)
    usage("Can't give both ef=file and formula=\"map=expression\".");

  if (G_projection()==PROJECTION_UTM)
    sscanf(optz->answer,"%d",&zone);
  else
    zone = 0;

    /* open stf input text file and read to set flags and rec1 and rec2 */
  if (opt1->answer==NULL) usage("No STF1 file specified.");
  if ((fp_stf = fopen(opt1->answer,"r")) == NULL)
    usage("Can't open input STF1 file.");
  get_stf_record(fp_stf,buf);

  /* Get field name for stdin record number */
  strcpy(field,opt5->answer);
  if (strlen(field)>16 || *field == '\0')
    usage("Bad record_field name.");
  G_tolcase(field);

  /* Decode output parameter */
  output_type = set_otype(opt2->answer,table_cars,table_column);

  /* Get formula from command line */
  if (opt4->answer != NULL) {
    strcpy(formula,opt4->answer);
    if (strlen(formula) >= 500)
       usage("Formula too long; 500 character limit");
    /* Extract the first 14 chars, up to = (as the filename) */
    G_squeeze(formula);
    for (*filename=0,i=0; i<=14; i++) {
      if (formula[i]== '=' || formula[i]== ' ') break;
      filename[i] = formula[i];
      filename[i+1] = '\0';
    }
    if (i>=14) usage("Formula label too long.");
    while (formula[i] != '=' && i<20) i++;
    if (i==20) usage("Didn't find = in formula on command line.");
    fpoint = formula+i+1; /* point to start of real formula  */
  }

  if (opt3->answer != NULL)  /* open file to read formula(s) */
    if((if_fp=fopen(opt3->answer,"r"))==NULL)
      usage("Couldn't open map formula input file.");

cont_reading:
  if (if_fp != NULL) {        /* read formula from file */
    *buf = '\0';
    while (fgets(bc_string,510,if_fp)!=NULL) {
      if ((i=strlen(bc_string))==0) continue; /* skip blank lines */
      bc_string[i] = '\0'; /* remove nl */
      G_strip(bc_string);
      if (*bc_string == '#')
        if (*buf) break;           /* stop on # */
        else continue;
      if (*buf) strcat(buf," ");   /* add a space */
      strcat(buf,bc_string);
    }
    G_squeeze(buf);
    if (*buf) {
      /* check for out=choice */
      p = buf;
      if (!strncmp(buf,"out=",4)) {
        output_type = set_otype(buf+4,table_cars,table_column);
        while (*p != ' ') p++; /* scan past out=choice */
        p++;
      }
      strcpy(formula,p);
    }
    else {
      fclose(fp_stf);
      exit (0);  /* quit when input file empty */
    }

    /* Extract the first 14 chars, up to = (as the filename) */
    for (*filename=0,i=0; i<=14; i++) {
      if (formula[i]== '=' || formula[i]== ' ') break;
      filename[i] = formula[i];
      filename[i+1] = '\0';
    }
    if (i>=14) usage("Formula label too long.");
    while (formula[i] != '=' && i<20) i++;
    if (i==20) usage("Didn't find = in formula.");
    fpoint = formula+i+1; /* point to start of real formula  */
  }
  fprintf (stdout,"%s = %s\n",filename,fpoint);

  verify_formula(fpoint); /* Check matching () */
  sub_for_mtx(fpoint);

  tmp_file = G_tempfile();  /* for bc input strings */
  tmp_file1= G_tempfile();  /* for verfied bc output */

  tmp_file2= G_tempfile();  /* for table: entries */
  if ((table_fp = fopen(tmp_file2,"w")) == NULL)
    G_fatal_error("Could not open tmp_file2"); 

  sprintf(buf,"bc -l > %s",tmp_file);  /* Start bc -l process */
  if ((bc_pipe = popen(buf,"w")) == NULL)
    G_fatal_error("Could not open pipe to bc -l"); 

  count_all = 0;  /* reset for get_stf_record() */
  rewind(fp_stf);

  /* Read and process stdin lines.  This is the big loop */
  while(1){
    if (*field == '-')
      record = ++count_all;
    else {
      if (fgets(line,520,stdin) == NULL) break; /*exit big loop */
      if (extract_value(line,field) != 0) continue;
    }
    branch = get_stf_record(fp_stf, buf);
    if (branch < 0) break;
    if (branch == 0) continue; 

    /* Substitute into formula while copying to bc_string */
    s = bc_string;
    p = fpoint;
    while (*p) {
      /* Check a lot of conditions! */
      if (sscanf(p,"%c%4d",&c,&column)==2 && isdigit(*(p+1)) && (isupper(c)))
      {
        for (i=column;i<(column+(c-'A'+1));i++)
          if(*(buf+i)==' ') *s++ = '0';   /* sub zero for spaces in data */
          else *s++ = *(buf+i);
        while (isdigit(*(++p))) ;        /* advance formula pointer */
      }
      else
        *s++ = *p++ ;	/* Just copy the char */
    }
    *s = '\0';

    if ((pl94_flag || stf1_flag) && (output_type != TABLE)) {
      /* extract the lat and long */
      sscanf(buf+269,"%9ld",&lat);
      sscanf(buf+278,"%10ld",&lon);
      /* compute utms */
      if (G_projection() == PROJECTION_UTM) {
        get_utms(lon,lat,&e,&n,&zone,optsph->answer);
	fprintf(table_fp,"%12.2f %12.2f \n",e,n); /*Send the lat,lon coords*/
      }
      else {
        get_utms(lon,lat,&e,&n,&zone,"");
	fprintf(table_fp,"%12.6f %12.6f \n",e,n); /*Send the lat,lon coords*/
      }
    }
    else
      if (output_type == TABLE) {
        *table_string = '\0';
        for (i=0;i<TFIELDS && table_column[i]>0; i++) {
        if (table_column[i]==192 && table_cars[i]==26)
          table_cars[i] = 66;
        strncat(table_string, buf+table_column[i], table_cars[i]);
        strcat (table_string, ":");
        }
      fprintf(table_fp, "%s\n", table_string);
      }
    fprintf(bc_pipe,"%s\n",bc_string); /* Send formula */    
  } /* end of big loop */

  /* Close files & pipe */
  fclose(table_fp);
  if (bc_pipe != NULL){ pclose(bc_pipe); sync();}

  /* check tempfile for error messages */
  if (verify_tmpfile(tmp_file,tmp_file1,output_type) < 0)
    fprintf(stderr,"\nDue to error you should manually check results.\n");

  unlink(tmp_file);
  tmp_file = tmp_file1;

  if ((!stf1_flag && !pl94_flag) && output_type!=TABLE) 
    output_type = NONSTF;

  switch (output_type) {
  case ATTS: sprintf(buf,
	"paste -d\" \" %s %s | sed -e 's/^/A /' > %s/%s/dig_att/%s",
		  tmp_file2,tmp_file,G_location_path(),G_mapset(),filename);
    break;
  case SITE: sprintf(buf,
     "paste -d\" \" %s %s|awk '{print $1\"|\"$2\"|#\"$3}'>%s/%s/site_lists/%s",
		  tmp_file2,tmp_file,G_location_path(),G_mapset(),filename);
    break;
  case TABLE:
  case STD : sprintf(buf," paste -d\" \" %s %s",tmp_file2, tmp_file);
             break;
  case NONSTF:
  default:    sprintf(buf, "cat %s" ,tmp_file);
    break;
  } 
  G_system(buf);
  unlink(tmp_file);
  unlink(tmp_file2);
  if (if_fp != NULL) goto cont_reading ;
  fclose(fp_stf);
  exit(0);
}				/* end of main() */

int pmt (char *s)
{
  char b[300];
    sprintf(b,"cat %s/etc/census.docs/%s",G_gisbase(),s);
  G_system(b);
  exit (0);
}

int set_otype (char *p, int *t_cars, int *t_column)
{
      int i;
      char c,*tf,tfs[9*TFIELDS];

      if (!strncmp(p ,"att",3)) {
        G__make_mapset_element("dig_att");
        return (ATTS);
      }
      else if (!strncmp(p ,"sit",3)) {
        G__make_mapset_element("site_lists");
        return (SITE);
      }
      else if (!strncmp(p ,"-",1)) {
        return (STD);
      }
      else if (!strncmp(p ,"table:",6)) {
        tf = tfs;
        strcpy(tf,p+5);
        sub_for_mtx(tf);
        i = 0;
        while (sscanf(tf,":%c%d",&c,t_column+i)==2 &&
            isdigit(*(tf+2)) && (isupper(c)) ) {
	  *(t_cars+i) = c - 'A' + 1;
          i++;
          tf++;
          while (*tf && *tf!=':' && *tf!=' ') tf++;
        }
        *(t_cars+i) = 0; *(t_column+i) = 0;
        if (i>0) return (TABLE);
          else usage("No out=table: colums specified.");
        }
        else
          usage("Bad \"table field\" in out=table:.");

      usage("Bad \"out=\" choice.");

      return 0;
}

/* Read an input record -- usually STF type */
int get_stf_record (FILE *fp, char *buf)
{
    static int first_time = 1;
    static int rec1, rec2;

    int i;
    int lrpn1, lrpn2;
    char *p,c;
    char msg[200];

  if (first_time) {  /* determine actual segment lengths */
    rec2 = 0;
    pl94_flag = stf1_flag = 1; /* assume an STF1 or PL94 record */
    for (rec1=1; rec1<=MATRIX_REC; rec1++){
      if ((c=fgetc(fp)) == '\n')  break;
      if (rec1==1 && c!='S') stf1_flag = 0; /* cancel STF1 record type */
      if (rec1==2 && c!='T') stf1_flag = 0;
      if (rec1==3 && c!='F') stf1_flag = 0;
      if (rec1==4 && c!='1') stf1_flag = 0;

      if (rec1==1 && c!='P') pl94_flag = 0; /* cancel PL94 record type */
      if (rec1==2 && c!='L') pl94_flag = 0;
      if (rec1==3 && c!='9') pl94_flag = 0;
      if (rec1==4 && c!='4') pl94_flag = 0;
    }
    if (rec1 > MATRIX_REC) rec1 = MATRIX_REC;
    if (rec1 < 4700) stf1_flag = 0; /* not a real STF1 record */
    if (stf1_flag) {
      for (rec2=1; rec2<=MATRIX_REC; rec2++)
        if (fgetc(fp) == '\n')  break;
      if (rec2 > MATRIX_REC) rec2 = MATRIX_REC;
    }
    fprintf(stderr,"rec1=%d  rec2=%d\n",rec1,rec2);
    rewind(fp);
    first_time = 0;
    return 0;
  }

  if (stf1_flag) {
    if (fseek(fp,(long) ((record-1)*(rec1+rec2)), 0) < 0) {
      if (count_all > 0) return (-1); /* exit gracefully */
      sprintf(buf,"Can't seek to recnum %d in STF1A file.",record);
      G_fatal_error(buf);
    }
  }
        /* read a line of input from in=file */
  if (fgets(buf+1,rec1+1,fp)==NULL){
    if (count_all > 0) return (-1); /* exit gracefully */
     else {
      sprintf(buf,"Can't seek to recnum %d in input file.",record);
      G_fatal_error(buf);
      }
  }
  
  if (stf1_flag) {
    if (fgets(buf+1+4805,rec2+1,fp)==NULL || *(buf+1+4805) == '\n' ){
    if (count_all > 0) return (-1); /* exit gracefully */
       else G_fatal_error("Second STF line not found.");
    }
  /* check for a complete pair of STF records */
    lrpn1 = lrpn2 = 0;
    sscanf(buf+25,"%4d",&lrpn1);
    sscanf(buf+4805+25,"%4d",&lrpn2);
    if (strncmp(buf+1,buf+4805+1,13)  /* first 13 chars should be same */
     || strncmp(buf+19,buf+4805+19,6) /* logrecnu's should be same */
     || ( (lrpn2-lrpn1) != 1))        /* logrecpn's should be 1 different */
    {
    strncpy(msg,buf+1,24);
    strcat(msg,"\n");
    strncat(msg,buf+4805+1,24);
    sprintf(msg+49,"\n(Part %d and Part %d)", lrpn1,lrpn2);
    strcat(msg,
       "\nThe 2 parts of a complete STF1 record appear unrelated\n");
    G_fatal_error(msg);
    }
  }
  else
    if ( *(buf+1)=='\n') return 0; /* skip if blank line */

  return 1;
}


/* Decode input lines looking for key strings; ignore others */
/* returns 0 when done with a record */
int extract_value (char *s, char *f_name)
{
  char label[100],value[100];

  G_squeeze(s);
    
  if (strlen(s) < 3) return 1;
  sscanf(s,"%s %s",label,value);

  if (! strncmp(label,f_name,strlen(f_name))) {
         sscanf(value,"%d",&record);
         return 0;
       }

  return 1;
}

int verify_tmpfile (char *in, char *out, int out_type)
{
   FILE *fp_in, *fp_out;
   int i, err;
   char  val[100];
   char *p;

  if ((fp_in = fopen(in,"r")) == NULL)   /* open files */
	G_fatal_error("tmp_file didn't open for reading");
  if ((fp_out = fopen(out,"w")) == NULL)
	G_fatal_error("tmp_file1 didn't open for writing");

  i = 0; err=0;

  while (fgets(val,100,fp_in) != NULL)    /* and next line */
  {
	i++;
	for (p=val; *p; p++) {
	    if (islower(*p)){  /* this means error msg */
		err--;       /* set error flag */
		fprintf(stderr,
		"Note: Output record %d had bc compute error: %s", i, val);
		fprintf(stderr,"  Zero (0) used as result of expression\n");
		fgets(val,90,fp_in);     /* skip to next string */
		strcpy(val,"0\n");       /* substitute 0 for result */
		break;
	    }
	}
	fputs(val,fp_out);
  }
  fclose(fp_in);
  fflush(fp_out);
  fclose(fp_out);
  return (err);
}

int sub_for_mtx (  /* substitute Lxxx for MATRIX or ID field name */
    char *s
)
{
  register int i;
  int j, modified;
  char *s_start, *q, buf[500], fmt[100];
  s_start = s;
  q = buf;
  modified = 0;
  while (*s) {
    switch (sub_case(s)) {

    case 0:    /* nothing special, copy the character */
      *q++ = *s++;
      break;
            
    case 1:    /* is a matrix sec. name for STF1 or PL94 record */
      for (i=0; i<NPL94MATRIX; i++) {
        if (!strncmp(pl94matrix[i].field,s,8)) {
          j = pl94matrix[i].col;
          sprintf(q,"%c%d|",'A' -1 + pl94matrix[i+1].col - j, j);
          while (*q != '|') q++;
          s += 8;
          modified = 1;
          break;
        }
      }
      if (i>=NPL94MATRIX) {
        sprintf(buf,"Unknown MATRIX field <%8.8s> in formula",s);
        usage(buf);    /* exit */
      }
      break;
            
    case 2:    /* is a matrix sec. name for STF1 record */
      for (i=0; i<NMATRIX; i++) {
        if (!strncmp(matrix[i].field,s,8)) {
          j = matrix[i].col;
          sprintf(q,"%c%d|",'A' -1 + matrix[i+1].col - j, j);
          while (*q != '|') q++;
          s += 8;
          modified = 1;
          break;
        }
      }
      if (i>=NMATRIX) {
        sprintf(buf,"Unknown MATRIX field <%8.8s> in formula",s);
        usage(buf);    /* exit */
      }
      break;
             
    case 3:   /* is an identification sec. name  */
      for (i=0; i<NID; i++) {
        j = strlen(id[i].name);
        if (! strncmp(id[i].name,s,j)) {
          if (! strncmp(id[i].name,"ANPSADPI",8) )
            sprintf(q,"Z192|");
          else
            sprintf(q,"%c%d|", 'A' -1 + id[i].len, id[i].col);
          while (*q != '|') q++;
          s += j;
          modified = 1;
          break;
        }
      }
      if (i>=NID) {
        sprintf(fmt,"Unknown IDENTIFICATION field <%%%d.%ds> in formula",j,j);
        sprintf(buf,fmt,s);
        usage(buf);    /* exit */
      }
      break;
    default: usage("Bad switch from sub_case()");
    }  /* end of switch */
  }
  *q = '\0';             /* terminate string for sure */
  if (modified) strcpy(s_start,buf);   /* return modified string */
  return 0;
}

int sub_case (char *s)
{
  if ((*s=='P' || *s=='H') && isdigit(*(s+1)) && isdigit(*(s+7))) {
    if (pl94_flag) return 1; /* PL94 matrix field name */
    if (stf1_flag) return 2; /* STF1 matrix field name */
  }
  else
    if (isupper(*s) && isupper(*(s+1)) )
      return 3; /* IDENTIFICATION section field name */
  return 0;
}

int verify_formula (char *s)
{
  int k;

  k=0;
  while (*s) {
   if (*s == '(' ) k++;
   if (*s == ')' ) k--;
   s++;
  }
  if (k) usage("Unbalanced ( ) in formula");
  return 0;
}

int get_utms (long lon, long lat,
   double *east, double *north, int *zone, char *sphere)
{
  static int first_utm=0;
  double lt, ln;

  *north = ((double) lat)/1000000.0;
  *east  = ((double) lon)/1000000.0;

  if (G_projection()==PROJECTION_UTM){
    if (first_utm==0){
     if ((CC_u2ll_spheroid(sphere)!=1))
       G_fatal_error(
        "\nBad spheroid: See m.gc2ll GRASS Manual Page for choices.");
    first_utm = 1;
    }
  lt = (*north) * 3600.0;
  ln = (*east)  * 3600.0;
  if (ln < 0.0) ln = -ln;
  CC_ll2u( lt, ln, east, north, zone);
  }
  return 0;
}

int usage (char *s)
{
char msg[2000];

sprintf(msg,
  "USAGE:\n %s ",argv0);
strcat(msg,"[-d] [-p] [-l] [-u] ");
strcat(msg,"in=STF.file [id=recnum_field] [out=att|site|-|table:Lccc]\n");
strcat(msg,"    formula=map=expr | if=path/file\n");
strcat(msg,"Where: STF.file is STF1 record file being used.\n\n");
strcat(msg,"       recnum_field is Keyword in stdin stream preceeding\n");
strcat(msg,"           number of STF1A record to read from STF.file.\n");
strcat(msg,"           If '-a' is used, or this parameter is omitted,\n");
strcat(msg,"           all records in STF.file will be processed.\n\n");
strcat(msg,"       -    results to standard out; this is the default\n");
strcat(msg,"       att  results to vector map attribute file\n");
strcat(msg,"       site results to site list\n");
strcat(msg,"       table: results in table to standard out with data field\n");
strcat(msg,"             the letter-column combination as first column\n");
strcat(msg,"             instead of easting and northing\n");
strcat(msg,"       map  is name of vector or site map to make\n\n");
strcat(msg,"       formula is computation with STF1A columns,\n");
strcat(msg,"               constants and bc operators\n");
fprintf(stderr,"%s", msg);
G_fatal_error(s);
}

int parse_input (int argc, char *argv[])
{
    int i;
    static char b[20];
/* Define the different options */

    opt1 = G_define_option() ;
    opt1->key        = "input_stf1";
    opt1->type       = TYPE_STRING;
    opt1->required   = NO;
    opt1->answer     = NULL;
    opt1->description= "Path/name of STF1 or PL94 input file";

    opt2 = G_define_option() ;
    opt2->key        = "out";
    opt2->type       = TYPE_STRING;
    opt2->required   = NO;
    opt2->answer     = "-";
    opt2->description= "Type of output: site | atts | table:Lxxx | - (stdout)";

    opt3 = G_define_option() ;
    opt3->key        = "ef";
    opt3->type       = TYPE_STRING;
    opt3->required   = NO;
    opt3->answer     = NULL;
    opt3->description= "Path/name of text file with formula expression(s)";

    opt4 = G_define_option() ;
    opt4->key        = "formula";
    opt4->type       = TYPE_STRING;
    opt4->required   = NO;
    opt4->answer     = NULL;
    opt4->description= "map=expression";

    opt5 = G_define_option() ;
    opt5->key        = "name_field";
    opt5->type       = TYPE_STRING;
    opt5->required   = NO;
    opt5->answer     = "-a";
    opt5->description= "field name for parsing stdin lines (-a to ignore)";

  if (G_projection() == PROJECTION_UTM){
    sprintf(b,"%d",G_zone());
    optz = G_define_option() ;
    optz->key        = "zone";
    optz->type       = TYPE_INTEGER;
    optz->required   = NO;
    optz->answer     = b;
    optz->options    = "1-60";
    optz->description= "UTM zone number; default is location zone";

    optsph = G_define_option() ;
    optsph->key        = "spheroid";
    optsph->type       = TYPE_STRING;
    optsph->required   = NO;
    optsph->answer     = "clark66";
    optsph->description= "Spheroid for LL to UTM conversion; see m.gc.ll";
  }

    flag1 = G_define_flag();
    flag1->key         = 'd';
    flag1->description = "Output IDENTIFICATION SECTION to stdout (20 pages)";

    flag2 = G_define_flag();
    flag2->key         = 'p';
    flag2->description = "Output STF1 MATRIX TABLE to stdout (30 pages)";

    flag3 = G_define_flag();
    flag3->key         = 'l';
    flag3->description = "Output PL94-171 MATRIX TABLE to stdout (1 page)";

    flag4 = G_define_flag();
    flag4->key         = 'u';
    flag4->description = "Output STF1 SUMLEV TABLE to stdout (4 pages)";

  if (G_parser(argc, argv) < 0)
	exit(-1);

   return 0;
} /* end of parse_input() */

