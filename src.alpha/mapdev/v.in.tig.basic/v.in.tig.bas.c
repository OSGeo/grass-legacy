
/* Build GRASS dig file from Tiger Types 1 and 2 data and optionally
   update the rep. point(X,Y), etc., in the v.db.rim data base 
   Open Type 1, Type2, v.db.rim and dig files first
   1. Count the lines in Type 2 and
   2. Build Type 2 pointer table (TLID, position and RTSQ),
       in memory or on disk depending on size of Type 2 file and
       value of block2.  If -p, try to use existing file.
   3. Sort table by TLID in Tiger (by sections if disk file needed)
   4. Read each Type 1 record and associated Type 2 records (if any)
   5. If UTM, convert coords to UTM
   6. Calculate representative point for line
   7. Write line coordinates to dig (vector) file (as AREA boundary)
   8. Write representative point to dig_att (attribute) file
   9. Write mods file for v.db.rim data base, if requested
  10. Run v.support, if requested
*/

#include "gis.h"
#include "Vect.h"

#define REC1_LEN 229
#define RECNUM 6
#define REC1_X 191
#define REC1_Y 201
#define REC1_X_LEN 10
#define REC1_Y_LEN 9

#define REC2_LEN 209
#define SEQ 16
#define SEQ_LEN 3
#define REC2_X1 19
#define REC2_Y1 29
#define REC2_X_LEN 10
#define REC2_Y_LEN 9

#define BLKL 183
#define BLKR 187

#define ARRAY_MAX 4000

int zone,proj;
char sphere[100];
extern long strtol();
extern double atof();

struct r2 {
	int tlid;
	int rs;
	long pos;
	}
	*rec2,*p1,*p2,*pstop,*pstart ;

/* for qsort of type 2 records by tlid  and rtsq*/
cmp_type2(q1,q2)
struct r2 *q1, *q2;
{
int diff;
  if (diff = (q1->tlid - q2->tlid) )
    return (diff) ;
  else
    return ( q1->rs - q2->rs );
}

usage(p)
char *p;
{
char msg[300];
switch(proj){
  case PROJECTION_LL:
    sprintf(msg, "USAGE: %s t1=Tiger1_file t2=Tiger2_file out=vect_map\n",p);
    break;
  case PROJECTION_UTM:
    sprintf(msg,
    "USAGE: %s t1=Tiger1_path t2=Tiger2_path out=vect_map z=utm_zone s=spheriod\n",p);
    break;
  default:
    strcpy(msg,
    "You are not in a UTM or Lat-Long location.  Don't use this program.");
    break;
  }
G_fatal_error(msg);
}

double
mscale(v)
double v;
{
  return v/1000000.0;
}

main(argc,argv)
int argc; char *argv[];
{
struct Map_info new_map;
struct line_pnts *lpts;
struct dig_head head;

FILE *tiger1;
FILE *tiger2; 
FILE *v_db;
FILE *fp_att;
FILE *tig2_tab_fp;
FILE *fp1;
FILE *fp_tlid;

char *vdbrim_file,*tlid_file,*sp;
char dig_name[30],sortfile[250],t1buf[250],t2buf[250];
int vdbrim_out,quiet,topology,dont_keep_pf; /* flags */
int type1_count,type2_count,type2_data[2],block2;
int in_mem,found,record,now_done,rtsq;
int i,ic,j,ip;
int leftbg, rightbg;
double max_x,max_y,min_x,min_y;
double xtmp[12],ytmp[12];
double rep_x,rep_y,d, xarray[ARRAY_MAX],yarray[ARRAY_MAX];
long offset, Vect_write_line();

struct Flag *flag1 ;
struct Flag *flag2 ;
struct Flag *flag3 ;
struct Flag *flag4 ;
struct Option *opt1 ;
struct Option *opt2 ;
struct Option *opt3 ;
struct Option *opt4 ;
struct Option *opt5 ;
struct Option *opt6 ;

G_gisinit (argv[0]);

proj = G_projection();
if (proj != PROJECTION_LL && proj != PROJECTION_UTM) usage(argv[0]);

/* Define the different options */

    opt1 = G_define_option() ;
    opt1->key        = "t1";
    opt1->type       = TYPE_STRING;
    opt1->required   = YES;
    opt1->description= "TIGER Type 1 file path/name" ;

    opt2 = G_define_option() ;
    opt2->key        = "t2";
    opt2->type       = TYPE_STRING;
    opt2->required   = YES;
    opt2->description= "TIGER Type 2 file path/name" ;

    opt3 = G_define_option() ;
    opt3->key        = "out";
    opt3->type       = TYPE_STRING;
    opt3->required   = YES;
    opt3->description= "Name of vector map to create";

  if (proj == PROJECTION_UTM){
    sprintf(t1buf,"%d",G_zone() );
    opt4 = G_define_option() ;
    opt4->key        = "zone";
    opt4->type       = TYPE_INTEGER;
    opt4->required   = NO;
    opt4->answer     = t1buf;
    opt4->options    = "1-60";
    opt4->description= "UTM zone number; default is location zone";

    opt5 = G_define_option() ;
    opt5->key        = "spheroid";
    opt5->type       = TYPE_STRING;
    opt5->required   = NO;
    opt5->answer     = "clark66";
    opt5->description= "Spheroid for LL to UTM conversion; see m.gc.ll";
  }

    opt6 = G_define_option() ;
    opt6->key        = "tlid";
    opt6->type       = TYPE_STRING;
    opt6->required   = NO;
    opt6->answer     = NULL;
    opt6->description= "Path/file with list of TLID numbers to extract";

/* Define the different flags */

    flag1 = G_define_flag() ;
    flag1->key         = 'q' ;
    flag1->description = "Perform functions quietly" ;

    flag2 = G_define_flag() ;
    flag2->key         = 't' ;
    flag2->description = "Build topology (dig_plus) file when done" ;

    flag3 = G_define_flag() ;
    flag3->key         = 'v' ;
    flag3->description = "Write update file for v.db.rim Tiger database" ;

    flag4 = G_define_flag() ;
    flag4->key        = 'p' ;
    flag4->description="Create Type 2 pointer file each time";

    if (G_parser(argc, argv) < 0)
	exit(-1);

    quiet        = flag1->answer;
    topology     = flag2->answer;
    vdbrim_out   = flag3->answer;
    dont_keep_pf = flag4->answer;

    v_db = NULL;

if (proj == PROJECTION_UTM) {
  zone = (int) strtol(opt4->answer, (char **)NULL, 10);
  strcpy(sphere,opt5->answer);
}

strcpy(dig_name,opt3->answer);

 /* open new map and files */
if (1 != Vect_open_new(&new_map,dig_name))
	G_fatal_error("Can't open the new binary dig file");

if ((fp_att = G_fopen_new("dig_att",dig_name)) == NULL)
	G_fatal_error("Can't open dig_att file for Master binary map");

if ((tiger1 = fopen(opt1->answer,"r")) == NULL)
	G_fatal_error("Can't open Tiger Type 1 file.");

if ((tiger2 = fopen(opt2->answer,"r")) == NULL)
	G_fatal_error("Can't open Tiger Type 2 file.");

fp_tlid = NULL;
if ((tlid_file=opt6->answer)!=NULL)
  if ((fp_tlid = fopen(opt6->answer,"r"))== NULL)
	G_fatal_error("Can't open TLID number file.");

if (vdbrim_out)
  if ((v_db = fopen(vdbrim_file=G_tempfile(),"w")) == NULL)
	G_fatal_error("Can't open v.db.rim temporary file.");

if (!quiet) fprintf(stderr,"Working . . .\n");
in_mem = 0;
type2_count = 0;

/* make sortfile  -  name of pointer file in tmp dir */
make_sort_name(sortfile,tiger2,0);

/* see if old pointer file needed and present */
if ( !dont_keep_pf && ((tig2_tab_fp=fopen(sortfile,"r")) != NULL) ) {
  /* if so, read first number */
  fread((char *)type2_data,sizeof(type2_data),1,tig2_tab_fp);
  type2_count = type2_data[0];
  block2      = type2_data[1];
  rec2=(struct r2 *) calloc(block2+1,sizeof(struct r2));
  if (rec2 == NULL){
    G_warning("Could not allocate space to use old pointer file, making new");
    goto makenewone;
  }
  if (type2_count <= block2) { /*  do in_mem stuff if small enough */
    in_mem = 1;
    if(type2_count !=
       fread((char *)rec2,sizeof(struct r2),type2_count,tig2_tab_fp))
         G_fatal_error("Pointer read count wrong.");
  }
  fclose(tig2_tab_fp); tig2_tab_fp = NULL;
}

else { /* create a new pointer file */
makenewone:
  /* allocate as much space as possible */
  block2 = type2_number(tiger2);
  while (NULL == (rec2=(struct r2 *) calloc (block2+1,sizeof(struct r2))) )
      block2 /= 2;

  tig2_tab_fp = fopen(sortfile,"w");
  if (tig2_tab_fp == NULL)
    G_fatal_error("Can't open temp Tiger Type 2 pointer file.");
  /* reserve space */
  fwrite((char *)type2_data,sizeof(type2_data),1,tig2_tab_fp);
  /* count type 2 records and build disk table of pointers */
  pstop = rec2 + block2;
  while (1) {
    for (p1=rec2; p1 < pstop; p1++) {
        p1->pos = ftell(tiger2);  /* get position in file */
        i = get_tiger_record(tiger2,'2',t2buf);
        if (i == EOF)  break;
	p1->tlid = my_atoi(t2buf+RECNUM-1,10);
	p1->rs   = my_atoi(t2buf+RECNUM+10-1,3);
	type2_count++;
    }
    if (p1 > rec2) {
      j = p1 - rec2;
      qsort(rec2,j,sizeof(struct r2),cmp_type2);
      if (i==EOF && type2_count<=block2)
        in_mem = 1;
      fwrite((char *) rec2,sizeof(struct r2),j,tig2_tab_fp);
    }
    if (i == EOF) break;
  }
  rewind(tig2_tab_fp); /* write actual count and allocate block size */
  type2_data[0] = type2_count;
  type2_data[1] = block2;
  fwrite((char *)type2_data,sizeof(type2_data),1,tig2_tab_fp);
  fflush(tig2_tab_fp);
  fclose(tig2_tab_fp); tig2_tab_fp = NULL;
}

if (!in_mem) {
  tig2_tab_fp = fopen (sortfile,"r");
  if (tig2_tab_fp == NULL)
    G_fatal_error("Can't reopen Type 2 temporary pointer file.");
}

if (!quiet){
  fprintf(stderr,"Type 2 pointer file: %s\n\nUsing ",sortfile);
  if (in_mem) fprintf(stderr,"memory copy of");
  else        fprintf(stderr,"disk (%d blocks)",(type2_count-1)/block2 +1);
fprintf(stderr," pointer list for %d type 2 records\n",type2_count);
}

  /* set default max and min coords */
max_x = max_y = -5.0E+9;
min_x = min_y = 1.0E+10;

/* zero xarray first time */
for (i=1;i < ARRAY_MAX-1; i += 10) xarray[i] = 0.0;

pstop = rec2 + (type2_count-1);
pstart=rec2;
type1_count = 0;
lpts = Vect_new_line_struct();
/* The BIG LOOP */
while ((get_tiger_record(tiger1,'1',t1buf)) != EOF) /* get a type 1 record */
{
	  /* get record number */
	record = my_atoi(t1buf+RECNUM-1,10);
	if (tlid_file != NULL && !check_wanted(record,fp_tlid)) continue;

	type1_count++ ; /* count them */

	 /* get starting node */
	ll_from_str(t1buf+REC1_X-1,xarray,yarray);

	if (!quiet){
	  if (!(record%50))
	    fprintf(stderr,"\nTLID=%d ",record);
	  else fprintf(stderr,".");
	}

	  /* extract the shape records -- the big job */
	  /* handles memory or disk pointer table */
        if (!in_mem) /* position file past first number */
	  fseek(tig2_tab_fp,(long) sizeof(type2_data),0);
	   
        while (1){
          if (!in_mem) {
	     j = fread((char *)rec2,sizeof(struct r2),block2,tig2_tab_fp);
             if (j == 0) goto tailend;
	     pstart=rec2;
             pstop = rec2 + (j-1);
	     if ((pstop->tlid < record) || (rec2->tlid > record)) continue;
                    /* can't be in this block! */
	  }
	     /* skip if no more pointers in memory */
	  if (pstart>=pstop && pstop->tlid==0 && in_mem) goto tailend;
	  for (p1=pstart; p1 <= pstop; p1++){
	           /* skip already used (zeroed) ones; in_mem only */
	    if ((i=p1->tlid) == 0) {
	        if (p1>=pstop) break;
	        p1 = p1 + (p1->rs - 1);
	        continue;
	    }
	    if ( i < record)  continue;
	    if ( i >  record) break; /* not here; they are sorted */
	    /* must be equal; there is a type 2, get points */
	    if ((rtsq=r_shape_file(p1,xtmp,ytmp,tiger2))==0)
	          goto tailend; /* should not happen! */
	    if (in_mem) {
	        p1->tlid = 0;    /* Zero pointer */
	          /* set offset to next good one */
	        p2 = p1 + 1;
	        if (p1<pstop && p2->tlid == 0){
	          i = p2->rs;
	          (p1+i)->rs = p1->rs =  i + 1;
	        }
	        else p1->rs = 1;

	        p2 = p1 - 1;
	        if (p2>=pstart && p2->tlid == 0) {
	          p2 = p1 - (p2->rs);
	          p2->rs = p1->rs + (p1-1)->rs;
	          i = p1->rs - 1;
	          (p1+i)->rs = p2->rs;  
	          p1 = p2 + (p2->rs - 1);
	        }
	        else p1 = p1 + (p1->rs - 1);

	    }
	    for (i=1; i<=10; i++) { /* copy data to arrays */
		  ic = (rtsq-1)*10 + i;
		  yarray[ic] = ytmp[i] ;
		  if ((xarray[ic] = xtmp[i]) == 0.0) goto tailend;
	    }
	   } /* end of for */
	   if (in_mem) goto tailend;
	} /* end of while(1) */

	tailend:
 	    /* count coord pairs */
	for (i=0; xarray[i] != 0.0; i++);

            /* stuff in the last point */
	ll_from_str(t1buf+REC1_X+REC1_X_LEN+REC1_Y_LEN -1,xarray+i,yarray+i);

            /* points now go from 0 to i ( i+1 points) */
	ip = i + 1; /* now there are ip LAT-LONG  points on line */

	if (proj==PROJECTION_UTM) make_utms(xarray,yarray,&zone,ip);

	/* update header coords */
	set_max_min(xarray,yarray,ip,&min_x,&min_y,&max_x,&max_y);

	    /* calc rep point */
	if (ip<3){rep_x= (xarray[0]+xarray[1]) / 2.0;
		  rep_y= (yarray[0]+yarray[1]) / 2.0;
		 }
	else	 {rep_x = xarray[ip/2];
	 	  rep_y = yarray[ip/2];
		 }

             /* write attribute point and record number */
	write_att(fp_att,'L',rep_x,rep_y,record);

             /* write line to dig file */
	Vect_copy_xy_to_pnts(lpts,xarray,yarray,ip);
	offset = Vect_write_line(&new_map,AREA,lpts);

     if (vdbrim_out) {
	  /* update rep point and other info in v.db.rim data base */

	fprintf(v_db,".c\ntlid %d\n",record);
	if (proj==PROJECTION_UTM){
	fprintf(v_db,"e %12.2f\nn %12.2f\n",rep_x,rep_y);
	fprintf(v_db,"freast %12.2f\ntoeast %12.2f\n",xarray[0],xarray[i]);
	fprintf(v_db,"frnorth %12.2f\ntonorth %12.2f\n",yarray[0],yarray[i]);
	}
	else {
	fprintf(v_db,"e %12.6f\nn %12.6f\n",rep_x,rep_y);
	fprintf(v_db,"freast %12.6f\ntoeast %12.6f\n",xarray[0],xarray[i]);
	fprintf(v_db,"frnorth %12.6f\ntonorth %12.6f\n",yarray[0],yarray[i]);
	}
	  /* compute block group values for left and right */
	leftbg  = my_atoi(t1buf+BLKL-1,3) / 100;
	rightbg = my_atoi(t1buf+BLKR-1,3) / 100;

	fprintf(v_db,"bgl %d\nbgr %d\n",leftbg,rightbg);
	fprintf(v_db,"vectoff %ld\n", offset);
	fprintf(v_db,"map 1\nvt L\n.e\n");
     }
     ip++;	/* zero last used part of xarray */
     for (i = 1; i <= ip; i ++) xarray[i] = 0.0;
} /* end of big loop */

free (rec2); /* in case space needed for v.support */
if (tig2_tab_fp != NULL) 
  fclose(tig2_tab_fp);
if (dont_keep_pf)
  unlink(sortfile);

if (v_db != NULL)
  fclose(v_db);

if (fp_tlid != NULL)
  fclose(fp_tlid);

/* fill the dig header */
  /* get the current date */
fp1 = popen("date +%m/%d/%y", "r");
fscanf(fp1, "%10s", head.date);
pclose(fp1);

strcpy(head.organization,"Family of TIGER users");
strcpy(head.your_name,G_whoami());
strcpy(head.map_name,dig_name);
strcpy(head.source_date,"1990");
strcpy(head.line_3,"Map of TIGER lines");
 /* really 100000, but this may fool topology building for v.in.landmark */
head.orig_scale = 1000L;
d = 10.0;
if (proj==PROJECTION_LL) d = 0.0001;
head.digit_thresh = 0.0;
head.map_thresh = 0.0;
if (proj==PROJECTION_UTM)
  head.plani_zone = zone;
else
  head.plani_zone = 0;
/* set limits in dig header +/- some leaway */
head.W = min_x - d;
head.E = max_x + d;
head.N = max_y + d;
head.S = min_y - d;

/* write the dig header again*/
Vect_destroy_line_struct(lpts);
Vect_copy_head_data(&head,&(new_map.head));
Vect_close(&new_map);
fclose(fp_att);

if (topology) {
  sprintf (t1buf,"%s/etc/v.build map=%s", G_gisbase(),dig_name);
  if (system (t1buf))
      G_warning("Could not run v.support to build topology.");
}
if (!quiet) {
  fprintf(stderr,"\n%d Type 1 and %d ",type1_count,type2_count);  
  fprintf(stderr,"Type 2 TIGER records processed to\n");
  fprintf(stderr,"create the GRASS4 vector file <%s> in mapset <%s>\n",
     dig_name,G_mapset() );
}
if (vdbrim_out) {
  strcpy(t1buf,vdbrim_file);
  sp = t1buf+strlen(t1buf);
  while (*sp != '/') sp--;
  strcpy(++sp,dig_name);
  rename(vdbrim_file,t1buf);
  if (!quiet) {
  fprintf(stderr,"\nFile to update v.db.rim database written to:\n%s",t1buf);
  fprintf(stderr,"\nIt may be destroyed when you exit GRASS.");
  fprintf(stderr,"\nUse it, move it, or loose it.\n");
  }
}
} /* end of main() */

/* make a known name for the sortfile using first TLID in
     Type 2 file, or verify a sortfile name 
   verify returns 0 for good.
*/
make_sort_name(s,t2)
char *s;
FILE *t2;
{
char *p, buf[220];
  strcpy(s, G_tempfile() );
  p = s + strlen(s);
  while (*p != '/') p-- ; /* find last divider */
  p++;
  rewind (t2);
  get_tiger_record(t2,'2',buf+1);
  *(buf+RECNUM+10) = '\0'; /* set NULL end of recnum */
  G_squeeze(buf+RECNUM);   /* get rid of any leading spaces */
  strcpy(p,buf+RECNUM);    /* tack onto full path */
  rewind (t2);
}

set_max_min(ax,ay,n,min_x,min_y,max_x,max_y)
double *ax,*ay;
int n;
double *min_x,*min_y,*max_x,*max_y;
{
double x,y;
int i;

  for (i=0; i<n; i++) {
    x = ax[i];
    y = ay[i];
    if (x < *min_x) *min_x = x;
    if (y < *min_y) *min_y = y;
    if (x > *max_x) *max_x = x;
    if (y > *max_y) *max_y = y;
  }
}

static int init_once=0;

make_utms(x,y,zone,n)
double *x, *y;
int *zone, n;
{
int i;
double lt, ln;

	if (!init_once) {
		if (proj!=PROJECTION_UTM)
		  G_fatal_error("Can't do UTM conversions in this location!");
		if ((CC_u2ll_spheroid(sphere)!=1))
			G_fatal_error(
		"\nBad spheroid.  See m.gc.ll manual page for choices.");
		init_once = 1;
	}
	for (i=0; i<n; i++) {
		ln = x[i]*3600.0;
		if (ln == 0.0) return; /* done if x=0 */
		if (ln < 0.0) ln = -ln;
		lt =  y[i]*3600.0;
		CC_ll2u( lt, ln, &x[i], &y[i], zone);
	}
}


/* read the next shape record */
r_shape_file(q1,xtmp,ytmp,f2)
struct r2 *q1;
double *xtmp, *ytmp;
FILE *f2;
{
char *cp,buf[300];
int shape_tlid,shape_rtsq,i;
double dx, dy;

  if (fseek(f2, q1->pos, 0) == -1  ||
      get_tiger_record(f2,'2',buf) == EOF) return (0);
  shape_tlid = my_atoi(buf+RECNUM-1,10);
  shape_rtsq = my_atoi(buf+RECNUM+10-1,3);
  if ((shape_tlid != q1->tlid) || (shape_rtsq != q1->rs)) {
    sprintf(buf,"\n TLID=%d %d or\n RTSQ=%d %d\n doesn't match for a Type 2 record!",
        shape_tlid,q1->tlid,shape_rtsq,q1->rs);
    G_fatal_error(buf);
  }
	
  for (cp=buf+REC2_X1-1,i=1; i<=10; cp=cp+(REC2_X_LEN+REC2_Y_LEN), i++)
  {
  ll_from_str(cp,xtmp+i,ytmp+i);
  if (xtmp[i] == 0.0) break;
  }

  return(shape_rtsq);
}

ll_from_str(cp,lon,lat)
char *cp;
double *lon, *lat;
{
char s[20];
	/* avoid sscanf for speed */
	strncpy(s,cp,4);     /* get int part  of lon */
	*(s+4) = '.' ;       /* add dec. pt. */
        strncpy(s+5,cp+4,6); /* get 6 dec's  */
        *(s+10) = '\0';      /* add null */
	*lon = atof(s);      /* convert it */

	strncpy(s,cp+10,3);  /* get int part  of lat */
	*(s+3) = '.' ;       /* add dec. pt. */
        strncpy(s+4,cp+13,6);/* get 6 dec's  */
        *(s+10) = '\0';      /* add null */
	*lat = atof(s);      /* convert it */
}

my_atoi(cp,n)
char *cp;
int n;
{
char s[20];

  strncpy(s,cp,n);     /* get the n chars of string */
  *(s+n) = '\0';       /* add null */
  return ( (int) strtol(s, (char **)NULL, 10) ); /* convert */
}

/* General routine to get tiger records.  Should work whether they
   contain <LF> or <CR> or neither or both.
   Returns: EOF for end of file; 0 for bad record_type requested;
            actual record length including possible <LF> and <CR>.
*/

get_tiger_record(fp,type,buffer)
FILE *fp;
char type;
char *buffer;
{
static int record_size[13]={0,228,208,111,58,52,76,74,36,98,52,44,46};
static int found_size[13]= {0,0,0,0,0,0,0,0,0,0,0,0,0};
static char record_type[13]={'\0','1','2','3','4','5','6','7','8','A','I','P','R'};

register int c,j;
int i,rec_size;
char msg[80];

  for (i=1; i<13; i++)
    if (type == record_type[i]) break;
  if (i > 12) return (-1);

  rec_size=record_size[i];

  /* skip <LF> and possible <CR>  and get first char */
  while (c=getc(fp)){
    if (c == EOF) return (EOF); 
    if (c == (int) type){
      buffer[0] = c;
      break;
    }
  }

  for (j=1; j<rec_size; j++)  /* get rest */
    buffer[j]=getc(fp);

  buffer[rec_size] = '\0';    /* terminate with null */

  if (found_size[i]==0) {     /* determine actual rec size first time */
    found_size[i] = record_size[i];
    while (c=getc(fp)){
      if (c == EOF) break;
      if (c == (int) type){ungetc(c,fp); break;}
      ++ found_size[i];
      if ((found_size[i] - record_size[i]) > 3) {
        sprintf(msg,
          "Extra chars found in Tiger Type %c File--Check it.\n", type);
        G_warning(msg);
      }
    }
  }
  return (found_size[i]);
} /* end of get_tiger_record() */

type2_number(t2)
FILE *t2;
{
long n;
  fseek(t2,0L,2);
  n = ftell(t2);
  rewind(t2);
  return ((int) (n/208));
}

check_wanted(rec,fp)
int rec;
FILE *fp;
{
static max;
static tlid_first = 1;
static int *r_list;

int *r,val;
char buff[302];

if (tlid_first){
  tlid_first = 0;
  max=0; /* count lines */
  while (fgets(buff,300,fp)!=NULL)
    if(1 == (sscanf(buff,"%d",&val)) )
       max++;
  rewind(fp);
  if( ((r_list = (int *) calloc(max+2,sizeof(int)))!=NULL) ) {
    r = r_list;
    while (fgets(buff,300,fp)!=NULL) 
      if (1 == (sscanf(buff,"%d",r)) )
        r++;
    *r = 0;
    fclose(fp);
  }
  else max = 0; /* can't use memory list */
} /* end of first time */

if (max) {
  for (r=r_list; *r != 0; r++)
    if (*r == rec) return 1;
}
else {
  rewind(fp);
    while (fgets(buff,300,fp)!=NULL)
      if (1 == (sscanf(buff,"%d",&val)) )
        if (val == rec) return 1;
}
return 0;
}
