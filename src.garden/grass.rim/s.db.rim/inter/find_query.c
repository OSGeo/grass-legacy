/* Routines and support functions for .find .query */

#include <math.h>
#include "gis.h"
#include "globals.h"
#include "rim.h"

#define LIST_OPTION 'l'
#define ADD_OPTION 'a'

static int s;                     /* site number returned from db */
static double e, n;               /* coordinates of that site */
static double target_e, target_n; /* target loc for .find */
static double target_dist;        /* distance filter */
static int target_site;           /* site # of target loc. */
static int pass;                  /* pass counter for masking test */
static int Mask;                  /* masking in effect */
static int auto_pr,list,add_list; /* flags for print during query */
static int append_list;           /* flag for append new sites to list */
                                  /* 0=none; 1=append; -1=delete */

/* this is the distance-to-target calculator */
double
dist(x,y)
     double x,y;
{
  double dx,dy;
  dx = x - target_e;
  dy = y - target_n;
  return(sqrt(dx*dx + dy*dy));
}

/* This is the ordering and qsort function for .find */
cmp_distance(q1,q2)
     struct query_site *q1, *q2;
{
  double d;
  d = dist(q1->east,q1->north) - dist(q2->east,q2->north);
  if (d == 0.0) return (0);
  return((d > 0.0)? 1: -1);
}

coord_good()
{
  if ( ((!Mask && in_wind(&Active_wind)) || (Mask && in_mask(pass++)) )
          && in_target() )
       return 1;
  else return 0;
}

#define FIND
#define FIND_PROMPT "find"

find_init(inp_buf)
     char *inp_buf;
{
  target_dist = 0.0;            /* flag for no target circle */
  G_squeeze(inp_buf);           /* added at CERL */
  set_mask_wind_app(inp_buf);   /* from .find input line */
  strcpy(Prompt, FIND_PROMPT);
}

/* Read the sites and keep one more than the requested
   number of nearest sites.  If the next site is closer than the last one
   throw out the last one and sort in the new one.  Returns number of
   site found.  */

find(inp_buf)
char *inp_buf;
{
  struct query_site *s1, *new_list;
  int i, count, error, number, offset, n_original;
  int add_del_count;
  CELL in_mask();
  char *p1;

  /* initialize for new or appended site list */
  if (append_list && (Last_site != NULL))
        n_original = 1 + (Last_site - Site_list);
  else {
        n_original = 0;
        Last_site = NULL;
  }
  count = Number_of_sites+1;  /* more than max number of records */
  number = 0; pass = 0;
  new_list = NULL;
  add_del_count = 0;

  G_squeeze(inp_buf);
  G_tolcase(inp_buf);
  /* decode input buffer and exit if error */

  switch (dist_clause(inp_buf) ) {
  case -1: {
    fprintf(Outfile,".find: target site %d not in data base %s.\n",
            target_site,File_name);
    goto abort_point;
  }
  case 1: break;
  case 2: break;
  default:
    {
      if(number=sscanf(inp_buf,"site %d %d",&target_site,&count) )
        {
          count = (number==2?count:1);
          if (! get_loc_of_site(target_site) ) {
            fprintf(Outfile,".find: target site %d not in data base %s.\n",
                    target_site,File_name);
            goto abort_point;
          }
        }
      else
        {
          number=sscanf(inp_buf,"%lf %lf %d",&target_e,&target_n,&count);
          if (number == 2) count=1;
          if (number < 2){
            fprintf(Outfile,".find: Invalid request coordinates\n");
            goto abort_point;
          }
          if (count < 1) {
            fprintf(Outfile,".find: Invalid number of records requested\n");
            goto abort_point;
          }
        }
      break;
    }                           /* end of default case */
  }                             /* end of switch */

  number = 0; pass = 0; add_del_count = 0;
  switch (append_list) {
    case -1: /* delete some sites */
      if (n_original) {  /* only if some exist */
        /* make new list space */
        new_list = (struct query_site *)
                   G_malloc((count+2)*sizeof(struct query_site));
        /* get the sites */
        if ((number = get_n_closest(new_list,count)) < 0)
          goto abort_point;
        if (number>0) /* delete them, if duplicates */
          for (s1=new_list; s1<(new_list+number); s1++)
            add_del_count += delete_rec(s1->site_number);
      }
    break; /* exit switch */
    case 1:
      if (n_original) { /* only if have some already, else do case 0 */
        /* make new list space */
        new_list = (struct query_site *)
                   G_malloc((count+2)*sizeof(struct query_site));
        /* get the sites */
        if ((number = get_n_closest(new_list,count)) < 0)
          goto abort_point;
        if (number>0) /* add them, if not duplicates */
          for (s1=new_list; s1<(new_list+number); s1++)
            if (!dup_rec(Site_list,Last_site,s1->site_number)) {
              copy_list_entry(++Last_site,s1);
              add_del_count++;
            }
        /* sort them in */
        if ((number=add_del_count) > 1)
          qsort((char *)Site_list+n_original,number,
                sizeof(struct query_site),cmp_distance);
      break;/* exit switch */
      }
    case 0:
     if ((add_del_count=get_n_closest(Site_list,count)) < 0)
       goto abort_point;
     if (add_del_count > 0) Last_site = Site_list+(add_del_count-1);
} /* end of switch */

#ifndef DBSITES
  report_sites(add_del_count);
  fprintf(Outfile,
    "\nE=%.6f N=%.6f was target location for .find\n", target_e,target_n);
#endif

goto norm_exit;
abort_point:
  add_del_count = -1;
norm_exit:
  if (pass) in_mask(-1);
  if (new_list) free(new_list);
  strcpy(Prompt, PROMPT);
  return (add_del_count);
}                               /*  end of find() */

dup_rec(q1,q2,rec)
struct query_site *q1, *q2;
int rec;
{
struct query_site *s1;
if (append_list!=1 || Last_site==NULL) return 0;
for (s1=q1; s1<=q2; s1++)
  if (s1->site_number == rec) return 1;
return 0;
}

delete_rec(rec)  /* delete a site from Site_list */
int rec;            /* and update Last_site */
{
struct query_site *s1, *s2;

  if (Last_site != NULL)
   for (s2=Site_list; s2<=Last_site; s2++)
    if (s2->site_number == rec) {
      for (s1=s2; s1<Last_site; s1++)
         copy_list_entry(s1,s1+1);
      if (Last_site > Site_list) Last_site--;
        else Last_site = NULL;
      return 1;
    }
  return 0;
}

copy_list_entry(dest,src)
struct query_list *dest, *src;
{
int i;
char *p1, *p2;
 p2 = (char *) dest;
 p1 = (char *) src;
 for (i=0; i<sizeof(struct query_site); i++) *p2++ = *p1++;
}

get_n_closest(start,count)
struct query_site *start;/* start is start of place to store */
                           /* caller must be sure is big enough */
int count;                 /* count is maximum number to get and sort */
                           /* by distance to target */
{                          /* returns actual number got */
int number,error;
struct query_site *s1, *s2;

  error=crim(DATA_TABLE,"SELECT FROM DATA" ); /* do the select command */
  if (error > 0) rim_error(error);
  if (error == -1) {
    fprintf(Outfile,".find: No data in data base");
    return -1;
  }
  number=0; pass=0;
  s1=start;
  while (number<count && crimdm(DATA_TABLE,GET,Rim_buffer) == 0) {
      get_s_e_n();      /* get coordinates */
      if (coord_good()) {
        save_s_e_n(s1++);
        number++;
      }
  }
  if (number > 0) { /* exit if no entries */

        /* sort them, if more than one entry */
        if (number > 1)
                qsort((char *)start,number,
                        sizeof(struct query_site),cmp_distance);

        /* continue looking if enough rows */
        if (number == count) {
          s2 = start + number;
          s1 = s2 - 1;

          /* get next rows from data base */
          while ((error=crimdm(DATA_TABLE,GET,Rim_buffer)) == 0) {
            get_s_e_n();
            if (coord_good()) {
                save_s_e_n(s2);
                /* sort all entries  when new is closer than last entry */
                if (cmp_distance(s1,s2) > 0)
                  qsort((char *)start,number+1,
                     sizeof(struct query_site),cmp_distance);
            }
          }
        }
    }
  if(pass) in_mask(-1);
  return number;
} /* end of get_n_closest() */

save_s_e_n(s1) /* put new entry on query list */
struct query_site *s1;
{
  s1->site_number = s;
  s1->north = n;
  s1->east = e;
}

/* Routines to handle the query function.  Data base select command
        is assembled, then executed.  Site numbers, eastings,
        northings, are stored on Site_list for each record retrieved.
*/

#define QUERY

#define Q_BUFFER_SIZE 800
#define QUERY_PROMPT "query"

static char *q_buffer = NULL;

query_init(inp_buf)
char *inp_buf;
{
        auto_pr = 0;    /* assume no auto print */
        list = FALSE; add_list = FALSE;
        target_dist = 0.0; /* assume no distance mask */
        G_squeeze(inp_buf); /* added at CERL */
        set_mask_wind_app(inp_buf); /* from .query line */

/* Allocate buffer space (once) and initialize site list pointers */
        if (q_buffer == NULL) q_buffer = G_malloc(Q_BUFFER_SIZE);

/* Initialize RIM request and user prompt */
        strcpy(q_buffer,"SELECT FROM DATA");
        strcpy(Prompt,QUERY_PROMPT);
}

query_line(inp_buf)
char *inp_buf;
{
int count;
        G_squeeze(inp_buf);
                /* get and interpret distance mask */
        switch (dist_clause(inp_buf) ) {
                case -1: {
                        if (! get_loc_of_site(target_site) )
                        { fprintf(Outfile,
                          ".find: target site %d not in data base %s.\n",
                                target_site,File_name);
                        target_dist = 0.0;
                        }
                return;
                }
                case 1:
                case 2: return;
                default: ;
        }

                /* set up for auto printing if requested */
        if (!strncmp(inp_buf,".p",2)) {
                auto_pr = 1;
                count = strcspn(inp_buf, " \t");
                count += strspn(&inp_buf[count], " \t-");
                if (inp_buf[count]==LIST_OPTION) list = TRUE;
                if (inp_buf[count]==ADD_OPTION) {list = TRUE; add_list = TRUE;}
                return;
        }
                /* assemble regular lines */
        if (strlen(inp_buf)+strlen(q_buffer) > Q_BUFFER_SIZE - 3 )
                G_fatal_error("Query request too long!");
        strcat(q_buffer," ");
        strcat(q_buffer,inp_buf);
}

query_done()
{
  int error, offset, number, n_original;
  CELL in_mask();

  strcpy(Prompt, PROMPT);       /* reset prompt */
  /* initialize for new or appended site list */
  if (append_list && (Last_site != NULL))
    n_original = 1 + (int) (Last_site - Site_list);
  else {
    n_original = 0;
    Last_site = NULL;
  }

  number = 0; pass = 0;
  error=crim(DATA_TABLE,q_buffer); /* do the select command */
  if (error > 0) {
    if (error != 4) rim_error(error);
    fprintf(Outfile,"Invalid RIM select clause in .query");
#ifdef DBSITES
    SLEEP3;
#endif
  }
  else
    if (error == 0) {           /* do the work if successful select */
      /* get each row */
      while (crimdm(DATA_TABLE,GET,Rim_buffer) == 0) {
        get_s_e_n();            /* get coordinates, etc. */

        /* check mask, window and target circle conditions */
        if (coord_good())
          if (append_list < 0) { /* delete site */
            number += delete_rec(s);
            auto_print();
          }
          else
            if (!dup_rec(Site_list,Last_site,s)) {
              /* really add one to list */
              Last_site = Site_list+(n_original+number);
              save_s_e_n(Last_site);
              number++;
              auto_print();
            }
      }
    }
  /* clean up */
#ifndef DBSITES
  report_sites(number);
#endif
  if (pass) in_mask(-1);
  return(number);
}                               /* end of query_done() */

set_mask_wind_app(buf)
char *buf;
{
 int window_yes;
 Active_wind.rows = Active_wind.cols = 0;
 Mask = 0;
 window_yes = 0;
 append_list = 0;
 /* check for mask/window use in this find/query request */
 /* also check append or delete flag */
 G_squeeze(buf);
 G_tolcase(buf);
 while(*buf) {
        if ( !strncmp(buf," a",2) || !strncmp(buf," -a",3) )
                append_list = 1;
        if ( !strncmp(buf," d",2) || !strncmp(buf," -d",3) )
                append_list = -1;
        if ( !strncmp(buf," m",2) || !strncmp(buf," -m",3) ) {
                Mask = 1;
                window_yes = 1;
        }
        if ( !strncmp(buf," w",2) || !strncmp(buf," -w",3)  )
                        window_yes = 1;
                buf++;
 }
 if (window_yes) {
         if (G__get_window(&Active_wind,"","WIND",G_mapset())!=NULL)
         /* above line is to be sure latest window is gotten. */
                G_fatal_error("Bad read of WIND in find/query");
         G_set_window(&Active_wind);
 }
}

/* See if a point (coords in e,n) is inside or outside of
   a target circle of radius target_dist */
in_target()
{
        if (target_dist == 0.0) return (1); /* no distance calc needed */
        if (target_dist > 0.0 )    /* keep only points within target dist */
                return ((dist(e,n) <= target_dist )? 1 : 0 );
        if (target_dist < 0.0 )    /* keep only points beyond target dist */
                return ((dist(e,n) > fabs(target_dist))? 1 : 0 );
}

/* See if a point is within a geographic window */

in_wind(w)
struct Cell_head *w;
{
        if ((w->rows == 0) && (w->cols ==0)) return(1); /* no window defined */
        if (w->west > e || w->east < e || w->north <n || w->south >n )
                return(0);
        else return(1);
}

/* get a cell value from MASK */

CELL
in_mask(flag)
int flag;
{
/* static variables */
static CELL *data_buf = NULL;
static struct Cell_head window;
static int fd_data;

/* other  variables   */
int row, col;
CELL value;

 switch (flag) {
 case -1: /* clean up for exit */
        if(data_buf) free(data_buf);
        if(fd_data> -1) G_close_cell(fd_data);
        pass =0;
        break;

 case  0:/* initialize, first time called */
        if(data_buf) free(data_buf);
        data_buf = NULL;
        value = 1;
        if(G_find_cell("MASK",G_mapset()) == NULL) {
                Mask = 0;  /* turn off masking request if no MASK */
                value = (CELL) in_wind(&Active_wind);
                break;
        }
        G_get_set_window(&window); /* get current window */
        fd_data = G_open_cell_old("MASK", G_mapset());
        if (fd_data<0)
        G_fatal_error(
                "Failed to open existing MASK cell file in query or find");

        data_buf = G_allocate_cell_buf();

 default:/* after the first time */
        if (!Mask || !data_buf)
                value = 1;
        else {
                /* check in bounds of current window--necessary for
                meaningful masking */
                if (!in_wind(&window) )
                        value=0;
                else {
                        /* now get the data cell */
                        row = (window.north - n)/window.ns_res;
                        col = (e - window.west)/window.ew_res;
        if (row < 0 || row >= window.rows || col <0 || col >= window.cols)
                                {value = 0; break;}
                        G_get_map_row_nomask (fd_data, data_buf, row);
                        value=data_buf[col];
                }
        }
 } /* end of switch */
 return (value);
}


#define OFFSET(num) Field_info[num].rec_offset
char type_to_char();

get_s_e_n()
{
 s = *Rim_buffer;
 e = * ( (double *) (Rim_buffer+1) );
 n = * ( (double *) (Rim_buffer+1+sizeof(double)/sizeof(int) ));
/*
  retr_buf_i(Rim_buffer, &s);
  retr_buf_d(Rim_buffer+sizeof(int), &e);
  retr_buf_d(Rim_buffer+sizeof(int)+sizeof(double), &n);
*/
}

report_sites(number)
int number;
{
int n;

 switch (append_list) {
  case -1: fprintf(Outfile,
               "\n%d sites deleted from selected set.",number);
           break;
  case 1:  fprintf(Outfile,
               "\n%d sites appended to seleted set.",number);
           break;
  case 0:
  default: break;
 }
 n = 0;
 if (Last_site)
        n = 1+ (Last_site-Site_list);
 fprintf(Outfile,
        "\n%d sites now selected from data base <%s>\n",n,File_name);
}

/* put e and n of site into globals target_e and target_n */
get_loc_of_site(rec_number)
int rec_number;
{
int error;
char cmd[50];

 sprintf(cmd,"select from data where %s eq %d",
           Field_info[0].column_name, rec_number);
 error=crim(DATA_TABLE,cmd); /* do the select command */
 if (error > 0) rim_error(error);
 if (error == -1) return(0);  /* no site with that number */
 /* get the row */
 if (crimdm(DATA_TABLE,GET,Rim_buffer) == 0 ) {
        get_s_e_n();  /* get coordinates */
        target_e = e;
        target_n = n;
 }
 return(1);
}

/*   Interpret distance from clause.  Return -1 for site requested
     not in data base, 0 for phrase not recognizable, 1 for "distance
     from record" properly interpreted, 2 for "distance from x y" read.
*/
dist_clause(inp_buf)
char *inp_buf;
{
 if (sscanf(inp_buf,"distance from site %d %lf",
        &target_site,&target_dist) == 2) {
   if (get_loc_of_site(target_site) )
        return (1);
   else
        return (-1);
 }
 else
   if (sscanf(inp_buf,"distance from %lf %lf %lf",
        &target_e,&target_n,&target_dist) == 3)
     return (2);
 return (0);
}

auto_print()
{
  if (auto_pr) {
        fill_values();
        if (list==TRUE) print_list(add_list);
          else print_form();
        fprintf(Outfile, "\n");
  }
}
