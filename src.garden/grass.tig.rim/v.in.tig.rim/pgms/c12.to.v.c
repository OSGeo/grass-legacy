
/* Build GRASS dig file from Tiger Types 1 and 2 data
     and update the rep. point (X,Y) in the v.db.rim data base 
   Open Type 1, Type2, v.db.rim and dig files first
   1. Count the lines in Type 2.
   2. Build Type 2 pointer table (RECNUM and SEQNUM)
   3. Sort table by RECNUM
   4.
*/

#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"

#define REC1_LEN 229
#define RECNUM 6
#define RECNUM_LEN 10
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

#define ARRAY_MAX 2000

int zone;
char sphere[30];
double max_x,max_y,min_x,min_y;

struct r2 {
	int recnum;
	int pos;
	}
	*rec2,*p1,*p2;

/* for qsort of type 2 records by recnum */
cmp_type2(q1,q2)
struct r2 *q1, *q2;
{
  return( q1->recnum - q2->recnum );
}

usage(p)
char *p;
{
char msg[100];
sprintf(msg,
  "USAGE: %s db Tiger1_path Tiger2_path z=utm_zone s=spheriod dbtmp_path\n",p);
G_fatal_error(msg);
}

char * clean_blank(strbuf,n)

char *strbuf;
int n;
{
int i,j,end;

end=n-1;

for (i=0; i<end; i++)
{
	if (*(strbuf+i) == ' ')
	{
		for (j=i; j<end; j++)
			*(strbuf+j) = *(strbuf+j+1);
		*(strbuf+end) = ' ';
		end--;
	}
}

return strbuf;
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

FILE *tiger1, *tiger2, *v_db, *fp_att;

char db_name[20], dig_name[30], buf[100], t1buf[250],t2buf[250];
char *cp,rbuf[12];
char tempchar;
int type2_count,found,rtsq;
long record;
int i,j,ip;
int blkgrpleft, blkgrpright;
double xtmp[12],ytmp[12];
double cx[2],cy[2],rep_x,rep_y,d, xarray[ARRAY_MAX],yarray[ARRAY_MAX];
long offset, Vect_write_line();

G_gisinit(argv[0]);

if (argc < 6) usage(argv[0]);

/* get date */

  /* get the current date */
  v_db = popen("date +%m/%d/%y", "r");
  fscanf(v_db, "%10s", head.date);
  pclose(v_db);

strcpy(db_name,argv[1]);
sprintf(dig_name,"%s.Master",db_name);

if (sscanf(argv[4],"z=%d", &zone)!=1) usage(argv[0]);
if (sscanf(argv[5],"s=%s", sphere)!=1) usage(argv[0]);

 /* open new map and files */
if (1 != Vect_open_new(&new_map,dig_name))
	G_fatal_error("Can't open the new binary dig file");

if ((fp_att = G_fopen_new("dig_att",dig_name)) == NULL)
	G_fatal_error("Can't open dig_att file for Master binary map");

if ((v_db = fopen(argv[6],"w")) == NULL)
	G_fatal_error("Can't open v.db.rim/Rim temporary file.");

if ((tiger1 = fopen(argv[2],"r")) == NULL)
	G_fatal_error("Can't open Tiger Type 1 file.");

if ((tiger2 = fopen(argv[3],"r")) == NULL)
	G_fatal_error("Can't open Tiger Type 2 file.");

/* count type 2 records */
type2_count =0;
while (fgets(t2buf,250,tiger2) != NULL) type2_count++; 
fprintf(stderr, "%d Type 2 records found in scan of file\n",type2_count);
/*set up space */
rec2 = (struct r2 *) G_calloc (type2_count+2,sizeof(struct r2));

/* read records */
rewind(tiger2);
for (p1=rec2,i=0; i<type2_count; i++, p1++) {
	fgets(t2buf,250,tiger2);
/* sscanf will look for the first 10 digits, starting from the first
   non-blank character after the pointer, not from the pointer and
   then the next 10 digits...  end the field temporarily with a
   0 before the sscanf */
	tempchar = t2buf[RECNUM+RECNUM_LEN-1];
	t2buf[RECNUM+RECNUM_LEN-1] = 0;
	sscanf(t2buf+RECNUM-1,"%10d", &(p1->recnum) );
	t2buf[RECNUM+RECNUM_LEN-1] = tempchar;
	p1->pos = i*REC2_LEN;
	p2 = p1;  /* pointer to last entry in list */
	}
fprintf(stderr,"Pointer table built for type 2 records\n");
/* sort the pointers */
qsort(rec2,type2_count,sizeof(struct r2),cmp_type2);

/* set default max and min coords */
max_x = max_y = 0;
min_x = min_y = 1.0E+10;

/* fill the dig header */
strcpy(head.organization,"");
/*strcpy(head.date,""); see above */
strcpy(head.your_name,"");
strcpy(head.source_date,"1990");
strcpy(head.line_3,"Master file for v.db.rim Tiger lines.");
head.orig_scale = 24000L;
head.plani_zone = zone;
head.W = min_x;
head.E = max_x;
head.N = max_y;
head.S = min_y;
head.digit_thresh = 0.0;
head.map_thresh = 0.0;

/* The BIG LOOP */
lpts = Vect_new_line_struct();
while (fgets(t1buf,250,tiger1) != NULL) {  /* get a type 1 record */
	tempchar = t1buf[RECNUM+RECNUM_LEN-1];
	t1buf[RECNUM+RECNUM_LEN-1] = 0;
	sscanf(t1buf+RECNUM-1,"%10ld", &record);  /* get record number */
	t1buf[RECNUM+RECNUM_LEN-1] = tempchar;
	if (!(record%50)) fprintf(stderr,"\nDoing RECNUM=%ld ",record);
	 else fprintf(stderr,".");
	sscanf(clean_blank(t1buf+REC1_X-1,10),"%10lf", &d); 
	xarray[0]=mscale(d);
	sscanf(clean_blank(t1buf+REC1_Y-1,9),"%9lf" , &d); 
	yarray[0]=mscale(d);
	cx[0] = xarray[0];
	cy[0] = yarray[0];
	for (i=1; i<ARRAY_MAX; i += 10) xarray[i] = 0.0;
	found = 0;                         /* check for type 2 record */
	for (p1=rec2; p1 <= p2; p1++){
		if (p1->recnum > record) break;  /* no shapes */
		if (p1->recnum ==record) {found=1; break;}
	}
	while (found) {
		fseek(tiger2,(long) p1->pos,0);
		rtsq=r_shape_file(record,xtmp,ytmp,tiger2);
		if (rtsq > 0) {
			for (i=1; i<=10; i++) {
				xarray[(rtsq-1)*10 + i] = xtmp[i] ;
				yarray[(rtsq-1)*10 + i] = ytmp[i] ;
				if (xtmp[i]==0.0) break;
			}
		p1++;
		}
		else break;
	}

	for (i=0; xarray[i] != 0.0; i++) ;  /* count coord pairs */
             /* stuff in the last point */
	sscanf(clean_blank(t1buf+REC1_X+REC1_X_LEN+REC1_Y_LEN -1,10), "%10lf", &d);
		xarray[i]=mscale(d);
	sscanf(clean_blank(t1buf+REC1_Y+REC1_X_LEN+REC1_Y_LEN -1,9), "%9lf" , &d);
		yarray[i]=mscale(d);;
	tempchar = t1buf[BLKL+3-1];
	t1buf[BLKL+3-1] = 0;
   sscanf(t1buf+BLKL-1,"%3d",&blkgrpleft);
	t1buf[BLKL+3-1] = tempchar;
      blkgrpleft=(blkgrpleft/100) * 100; 
	tempchar = t1buf[BLKR+3-1];
	t1buf[BLKR+3-1] = 0;
   sscanf(t1buf+BLKR-1,"%3d",&blkgrpright);
	t1buf[BLKR+3-1] = tempchar;
      blkgrpright=(blkgrpright/100) * 100; 

	cx[1] = xarray[i];
	cy[1] = yarray[i];
            /* points now go from 0 to i ( i+1 points) */
	ip = i + 1; /* now there are ip points */

	get_utms(xarray,yarray,&zone,ip);

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

	     /* update rep point in v.db.rim data base */
	get_utms(cx,cy,&zone,2);

	fprintf(v_db,".c\nrecnum %ld\n",record);
	fprintf(v_db,"e %12.2f\nn %12.2f\n",rep_x,rep_y);
	fprintf(v_db,"freast %12.2f\ntoeast %12.2f\n",cx[0],cx[1]);
	fprintf(v_db,"frnorth %12.2f\ntonorth %12.2f\n",cy[0],cy[1]);
	fprintf(v_db,"bgl %3d\nbgr %3d\n",blkgrpleft,blkgrpright);
	fprintf(v_db,"vectoff %ld\n", offset);
	fprintf(v_db,"map 1\nvt L\n.e\n");

   } /* end of big loop */
/* mod values in dig header */
head.W = min_x;
head.E = max_x;
head.N = max_y;
head.S = min_y;
/* write the dig header again*/
Vect_destroy_line_struct(lpts);
Vect_copy_head_data(&head,&(new_map.head));
Vect_close(&new_map);
fclose(fp_att);
fclose(v_db);
} /* end of main() */

void
set_max_min(x,y)
double x,y;
{
if (x != 0.0 && x < min_x) min_x = x;
if (x > max_x) max_x = x;
if (y != 0.0 && y < min_y) min_y = y;
if (y > max_y) max_y = y;
}

static int init_once=0;

get_utms(x,y,zone,n)
double *x, *y;
int *zone, n;
{
int i,r;
double lt, ln;

	if (!init_once) {
		if ((CC_u2ll_spheroid(sphere)!=1))
			G_fatal_error(
		"\nBad spheroid.  See m.gc.ll GRASS Manual Page for choices.");
		init_once = 1;
		}

	for (i=0; i<n; i++) {
		ln = x[i]*3600.0;
		if (ln < 0.0) ln = -ln;
		if (ln == 0.0) return; /* done if x=0 */
		lt =  y[i]*3600.0;
		r=CC_ll2u( lt, ln, &x[i], &y[i], zone);
		set_max_min(x[i],y[i]);
		}
}




/* read the next shape record */
r_shape_file(rec,xtmp,ytmp,p1)
int rec;
double *xtmp, *ytmp;
FILE *p1;
{
char *cp,buf[300];
int rec_seq,i;
long shape_rec;
double d;
char tempchar;

	fgets(buf,250,p1);
	tempchar = buf[RECNUM+RECNUM_LEN-1];
	buf[RECNUM+RECNUM_LEN-1] = 0;
	sscanf(buf+RECNUM-1,"%10ld",&shape_rec);
	buf[RECNUM+RECNUM_LEN-1] = tempchar;
	if (shape_rec != rec) return(-1);

	tempchar = buf[SEQ+3-1];
	buf[SEQ+3-1] = 0;
	sscanf(buf+SEQ-1,"%3d",&rec_seq);
	buf[SEQ+3-1] = tempchar;
	cp = buf+REC2_X1-1;
	for (i=1; i<=10; i++){
		sscanf(clean_blank(cp,10),"%10lf", &d); xtmp[i]=mscale(d);
		cp = cp+REC2_X_LEN+REC2_Y_LEN;
	}
	cp = buf+REC2_Y1-1;
	for (i=1; i<=10; i++){
		sscanf(clean_blank(cp,9),"%9lf", &d); ytmp[i]=mscale(d);;
		cp = cp+REC2_X_LEN+REC2_Y_LEN;
	}
 return(rec_seq);
}

