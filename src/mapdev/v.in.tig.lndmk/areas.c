#include "gis.h"
#include "globals.h"

struct landrec {
  int polyid;
  int land;
  double rep_x,rep_y;
  char name[32];
} *r1,*r2,*rstart,*rend,*r1tmp;

/* for qsort of memory list by polyid */
cmp_polyid(q1,q2)
struct landrec *q1, *q2;
{
    return ( q1->polyid - q2->polyid );
}

/* for qsort of int list */
cmp_int(q1,q2)
int *q1, *q2;
{
    return ( *q1 - *q2 );
}


do_areas()
{
int i, j, k, natt, nland, npoly, nunique, both_kinds;
int tmpsize, tmpinc, landid, polid, polyl, polyr;
int s1, s2, s3;
int *id;
struct Categories cats;
char *tmpfile1, *tmpfile2, buff[120], cmd[500];
FILE *fp1, *fp2;

if (feature_type[2][0] != 'A') return 0; /* don't do anything */

printf("\nAreas: Step 1");
tmpinc = tmpsize = 1000;
r1 = rstart = (struct landrec *) G_malloc(tmpsize*sizeof(struct landrec));
rend = rstart + tmpsize;
nland = 0;
tig_open('7');
both_kinds = both_conds(); /* init for good_one() */
is_a_match("~!~start!","");   /* init for good_one() */

/* First get the Landmark ID numbers from Rec 7 */
while (get_tiger_record(tiger[7].fp,'7',buff+1)!=EOF ){
  if ( area_yes(buff) )
    for (i=1; i<NCFCC; i++) {
      if (good_one('A',buff,both_kinds)) { /* got area record of right cat */
        track(nland);
        strncpy(r1->name,buff+25,30); r1->name[30] = '\0';
        sscanf(buff+11,"%10d",&(r1->land) );
        nland++; /* count landmarks */
        r1->polyid = 0;
        r1++;
        if (r1 >= rend) {
          s1 = r1 - rstart; /* save offset of r1 */
          tmpsize = tmpsize + tmpinc;
          rstart = (struct landrec *)
              G_realloc((char *)rstart,tmpsize*sizeof(struct landrec));
          rend = rstart + tmpsize;
          r1 = rstart + s1;
        }
      break;
      }
    }
}
if (nland==0) return 0;
r1tmp = r1;
tig_close('7');
printf("\n%d Landmark areas of requested type(s) found in Type 7 file.",nland);
printf("\nAreas: Step 2");
/* Second get the polygon identification number(s) from Rec 8 */
tig_open('8');
while (get_tiger_record(tiger[8].fp,'8',buff+1)!=EOF) {
  sscanf(buff+16,"%10d%10d",&polid,&landid);
  for (r2=rstart; r2<r1tmp; r2++)
    if(landid == r2->land) {  /* got one match */
      track((int) (r2-rstart));
      if (r2->polyid == 0)
        r2->polyid = polid;
      else {
        strcpy(r1->name,r2->name);
        r1->land   = landid;
        r1->polyid = polid;
        r1++;
        if (r1 >= rend) {
          s1 = r1 - rstart; /* save relative positions of pointers */
          s2 = r1tmp - rstart;
          s3 = r2 - rstart;
          tmpsize += tmpinc;
          rstart = (struct landrec *)
                G_realloc((char *)rstart,tmpsize*sizeof(struct landrec));
          rend = rstart + tmpsize;
          r1 = rstart + s1; /* restore pointer positions */
          r1tmp = rstart + s2;
          r2 = rstart + s3;
        }
      }
    }
}
 /* sort the memory list by polygon id */
npoly = r1 - rstart;
qsort(rstart,npoly,sizeof(struct landrec),cmp_polyid);
printf("\n%d polygons found in step 2, search of Type 8 records",npoly);
tig_close('8');
printf("\nAreas: Step 3");
k = 0;
/* Third get the polygon internal coords from Rec P */
tig_open('P');
while (get_tiger_record(tiger[11].fp,'P',buff+1)!=EOF){
  sscanf(buff+16,"%10d",&polid);
  for (r2=rstart; r2<r1; r2++) {
    if ((j=r2->polyid) > polid) break; /* list is sorted */
    if (polid==j) {
      get_point_coords(buff+26,&(r2->rep_x),&(r2->rep_y));
      track(k++);
      break;
    }
  }
}
printf("\n%d internal polygon points found in Type P records",k);
if(k != npoly)G_warning("Internal point not found for some records");
tig_close('P');
printf("\nAreas: Step 4");
/* Fourth make list of unique polygon id's. Sort it. */
/*  List is used later to avoid writing duplicate attributes */
id = (int *) G_calloc(npoly+1,sizeof(int));
j = 0;
for (r2=rstart; r2<r1; r2++) {
  polid = r2->polyid;
  if (j==0) id[j++] = polid;
  else {
    k = 0;
    for (i=0; i<j; i++)
      if (id[i] == polid) {k=1; break;} /* don't add dups*/
    if (k == 0) id[j++] = polid;
  }
}
nunique = j;
qsort(id,nunique,sizeof(int),cmp_int);
printf(": %d unique polygons identified.",nunique);

/* Fifth go through Rec I and write a file of tlid's for left and right
   area boundaries, and write a dig_atts file with internal points and
   some numeric label (polyid? landmarkid?)
*/
printf("\nAreas: Step 5");
k = 0; natt = 0;
fp1 = fopen(tmpfile1=G_tempfile(),"w");
fp2 = fopen(tmpfile2=G_tempfile(),"w");
G_init_cats((CELL)0,"Tiger landmarks",&cats);
tig_open('I');
while (get_tiger_record(tiger[10].fp,'I',buff+1)!=EOF) {
  if (buff[21] != 'P') continue;
  buff[37] = '\0'; buff[52] = '\0'; /* terminate numbers */
  sscanf(buff+27,"%10d",&polyl);
  sscanf(buff+42,"%10d",&polyr);
  for (r2=rstart; r2<r1; r2++) {
    polid = r2->polyid;
    if (polid == polyl || polid == polyr) { /* got one */
      track(k++);
      fprintf(fp1,"%.10s\n",buff+6); /* write the TLID */
      for (i=0; i<nunique; i++){
        if ((j = id[i]) == 0) continue; /* already used */
        if (j >  polid) break;
        if (j == polid) {  /* write the attribute for the polygon */
          fprintf(fp2,"A %14.6lf %14.6lf %10d\n",
             r2->rep_x, r2->rep_y, r2->polyid);
          printf("A %14.6lf %14.6lf %d %d %30s\n",
             r2->rep_x, r2->rep_y, r2->polyid, r2->land, r2->name);
          sprintf(cmd,"Landmark %d: %s",r2->land,r2->name);
          G_set_cat((CELL)r2->polyid,cmd,&cats);
          id[i] = 0;
          natt++;
          break;
        }
      }
    }
  }
} 
tig_close('I');
printf("\nAreas: Step 6: Call v.in.tig.basic and v.support\n");

/* Sixth close up and exit and call v.in.tig.basic and v.support */
fclose(fp1);
fclose(fp2);
free (id);
free (rstart);

  /* make v.in.tig.basic command */
sprintf(cmd,"v.in.tig.basic t1=%s t2=%s out=%s tlid=%s ",
    tiger[1].name, tiger[2].name, vect_name, tmpfile1);
if (proj == PROJECTION_UTM) {
  sprintf(buff,"spheroid=%s zone=%d",sphere,zone);
  strcat(cmd, buff);
}
puts(cmd);puts("\n");
G_system(cmd); /* run v.in.tig.basic */

sprintf(buff,"%s/%s/dig_att/%s",G_location_path(),G_mapset(),vect_name);
sprintf(cmd,"rm -f %s", buff);
puts(cmd);puts("\n");
G_system(cmd); /* remove vect atts file */

sprintf(cmd,"mv %s %s", tmpfile2, buff);
puts(cmd);puts("\n");
G_system(cmd); /* copy new vect atts file to map */

sprintf(cmd,"v.support map=%s option=build",vect_name);
puts(cmd);puts("\n");
G_system(cmd); /* build topology */

G_write_vector_cats(vect_name,&cats);
G_free_cats(&cats);
unlink(tmpfile1);
return (npoly);
}

/* check for an area record (missing lat-long is an area) */
area_yes(buf)
char *buf;
{
  return ((buf[59] == ' ')? 1 : 0);
}

track(t)
int t;
{
  if (!(t%50)) printf("\n");
  printf(".");
}
