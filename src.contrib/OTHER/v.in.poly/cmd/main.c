#include "gis.h"
#include <math.h>
#include "Vect.h"
static double pi = 3.1415926;

main(argc,argv)
int argc; char *argv[];
{
int i, nsegs, label, slabel;
char *p;
char buff[514], cmd[500], input_name[250];
char es[30],ns[30],desc[250];
double radius, x0, y0, inc, d, ang, sin(), cos();
double max_x,max_y,min_x,min_y;
double *x, *y;  /* coordinate arrays */

FILE *fp1, *fp_att, *fp_site;
struct Categories cats;
struct Map_info new_map;
struct line_pnts *lpts;
struct dig_head head;

long offset, Vect_write_line();

struct Flag   *flag1;
struct Option *opt0 ;
struct Option *opt1 ;
struct Option *opt2 ;
struct Option *opt3 ;

G_gisinit(argv[0]);

    opt0 = G_define_option() ;
    opt0->key        = "input";
    opt0->type       = TYPE_STRING;
    opt0->required   = NO;
    opt0->answer     = "-" ;
    opt0->description= "Name of input file (omit or use - for coords from stdin)";

    opt1 = G_define_option() ;
    opt1->key        = "vector";
    opt1->type       = TYPE_STRING;
    opt1->required   = YES;
    opt1->description= "Name of new vector map to create" ;
    opt1->gisprompt  = "new,dig,vector" ;

    opt2 = G_define_option() ;
    opt2->key        = "radius";
    opt2->type       = TYPE_DOUBLE;
    opt2->required   = YES;
    opt2->description= "Circumscribed radius of polygon(s)" ;

    opt3 = G_define_option() ;
    opt3->key        = "segments";
    opt3->type       = TYPE_INTEGER;
    opt3->required   = NO;
    opt3->answer     = "1";
    opt3->description= "Number of straight line segments bounding circle" ;

    flag1 = G_define_flag();
    flag1->key       = 't' ;
    flag1->description="Do not automatically build topology";

    if (G_parser(argc, argv) < 0)
	exit(-1);

strcpy(input_name, opt0->answer);
G_strip(input_name);

sscanf(opt2->answer,"%lf",&radius);
sscanf(opt3->answer,"%d",&nsegs);
if (nsegs < 3)   /* compute default number of segments */
  nsegs = compute_segs(radius);
fprintf(stderr,"Radius=%lf  Number of segments=%d\n",radius,nsegs);
x = (double *) G_malloc((nsegs+2) * sizeof (double)); 
y = (double *) G_malloc((nsegs+2) * sizeof (double));

fp_site = stdin; /* assume coords from stdin */
if (strcmp(input_name,"-")) { /* if not, open sites file for coords */
  fp_site = fopen(input_name,"r");
  if (fp_site == NULL) G_fatal_error("Could not find input file.");
}

 /* open new map and files */
if (1 != Vect_open_new(&new_map,opt1->answer))
	G_fatal_error("Can't open the new binary dig file");

if ((fp_att = G_fopen_new("dig_att",opt1->answer)) == NULL)
	G_fatal_error("Can't open dig_att file for Master binary map");

G_init_cats((CELL)0,"Polygons around points",&cats);

  /* set default max and min coords */
max_x = max_y = -5.0E+9;
min_x = min_y = 1.0E+10;
label = 0;
lpts = Vect_new_line_struct();
/* The BIG LOOP */
while (1) {
  p = cmd;
  *desc = '\0';
  if (fp_site == stdin && isatty(fileno(stdin))) /* print prompt */
      printf("\nE N [cat#] [description] > ");
  if (fgets(buff,512,fp_site) == NULL) break; /* done */
  G_strip(buff);
  i = strlen(buff);
  if (!strncmp(buff,"end",3)) break; /* done */
  if (*buff == '.') {
    if(buff[1]=='R') {
      sscanf(buff+2,"%lf",&radius);
      if (nsegs < 3) nsegs = compute_segs(radius);
      fprintf(stderr,"Radius=%lf\n",radius);
      continue;
    }
    if(buff[1]=='S') {
      sscanf(buff+2,"%d" ,&nsegs);
      if (nsegs < 3) nsegs = compute_segs(radius);
      fprintf(stderr,"Number of segments=%d\n",nsegs);
      x = (double *) G_realloc(x, (nsegs+2) * sizeof (double));
      y = (double *) G_realloc(y, (nsegs+2) * sizeof (double));
      continue;
    }
  }
  if (sscanf(buff,"%s %s %[^\n]",es,ns,p) < 2) continue;
  G_strip(p);
  G_scan_easting (es,&x0,G_projection() );
  G_scan_northing(ns,&y0,G_projection() );

  if (*p =='#') p++;
 /* extract numeric label and rest of description string */
  i = sscanf(p,"%d %[^\n]",&slabel,desc);
  if (i==0 || slabel == 0){
    label++ ;
    sprintf(desc,"%d-sided polygon", nsegs);
  } 
  else label = slabel;

  if (radius <= 0.0)
    G_fatal_error("Radius requested must be greater than 0.0");

  inc = 2.0 * pi / nsegs;
  if (nsegs % 2) ang = pi / 2.0;
  else           ang = pi / 4.0;

  for (i=0; i<nsegs; i++, ang += inc) {
    x[i] = x0 + radius * cos(ang);
    y[i] = y0 + radius * sin(ang);
  }
  x[nsegs] = x[0]; /* make end point same as first point */
  y[nsegs] = y[0];

    /* write line to dig file */
  Vect_copy_xy_to_pnts(lpts,x,y,nsegs+1);
  offset = Vect_write_line(&new_map,AREA,lpts);

    /* write attribute point and record number */
  write_att(fp_att,'A',x0,y0,label);

  G_set_cat((CELL)label,desc,&cats);

    /* update header coords */
  set_max_min(x,y,nsegs+1,&min_x,&min_y,&max_x,&max_y);
}

if (x!=NULL) free(x);
if (y!=NULL) free(y);
/* fill the dig header */
  /* get the current date */
fp1 = popen("date +%m/%d/%y", "r");
fscanf(fp1, "%10s", head.date);
pclose(fp1);

strcpy(head.organization,"");
strcpy(head.your_name,G_whoami());
strcpy(head.map_name,opt1->answer);
strcpy(head.line_3,"Circles from sites");
head.orig_scale = 100L;
d = 10.0;
if (G_projection()==PROJECTION_LL) d = 0.0001;
head.digit_thresh = 0.0;
head.map_thresh = 0.0;
if (G_projection()==PROJECTION_UTM)
  head.plani_zone = G_zone();
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
G_write_vector_cats(opt1->answer,&cats);
G_free_cats(&cats);
if (flag1->answer != 1){
  sprintf(cmd,"v.support map=%s option=build",opt1->answer);
  puts(cmd);puts("\n");
  G_system(cmd); /* build topology */
}

} /* end of main() */

compute_segs(r) /* automatically determine number of segments for "circle" */
double r;       /* assume min distance (min_d) between points of 0.2% of */
{               /*  the current region width. */
int n;
double theta,min_d, asin();
struct Cell_head window;

  G_get_window(&window);
  min_d = (window.east - window.west) * 0.0020 ;
  theta = 2.0 * asin(min_d/r);
  if (theta > pi/3.0) return 6;
  else return  ((int) (2.0*pi/theta + 0.99999) );
}

set_max_min(ax,ay,n,min_x,min_y,max_x,max_y)
double *ax,*ay;
int n;
double *min_x,*min_y,*max_x,*max_y;
{
double xt,yt;
int i;

  for (i=0; i<n; i++) {
    xt = ax[i];
    yt = ay[i];
    if (xt < *min_x) *min_x = xt;
    if (yt < *min_y) *min_y = yt;
    if (xt > *max_x) *max_x = xt;
    if (yt > *max_y) *max_y = yt;
  }
}

