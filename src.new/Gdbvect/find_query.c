/* Routines and support functions for .find and .query */

#include <math.h>
#include "gis.h"
#include "globals.h"
#include "rim.h"

#define LIST_OPTION 'l'
#define ADD_OPTION 'a'

static int s;                     /* record number returned from db */
static double e, n;               /* coordinates of that vector */
static int m;                     /* map number of vector */
static char t;                    /* vector type of vector */
static double target_e, target_n; /* target loc for .find */
static double target_dist;           /* distance filter */
static int target_vect;           /* record # of target loc. */
static int auto_print,list,add_list;/* flags for print during query */
 

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
cmp_distance(p1,p2)
struct query_record *p1, *p2;
{
double d;

	d = dist(p1->east,p1->north) - dist(p2->east,p2->north);
	if (d == 0.0) return (0);
	return((d > 0.0)? 1: -1);
}

#define FIND
#define FIND_PROMPT "find"

static int Mask;

find_init(inp_buf)
char *inp_buf;
{
	target_dist = 0.0;  /* flag for no target circle */
	G_squeeze(inp_buf);
	set_mask_and_window(inp_buf); /* from .find input line */
   strcpy(Prompt, FIND_PROMPT);
}

/* Read the records and keep one more than the requested
   number of nearest records.  If the next record is closer than the last one
   throw out the last one and sort in the new one.  Returns number of
   records found.  */

find(inp_buf)
     char *inp_buf;

{
  int i, count, error, number, offset, pass;
  struct query_record *s1, *s2;
  CELL in_mask();
  
  /* initialize for null record list */
  Last_record = NULL;
  count = Number_of_records+1;  /* more than max number of recordss */

  G_squeeze(inp_buf);
  /* decode input buffer and exit if error */

  switch (dist_clause(inp_buf) ) {
  case -1: {
    fprintf(Outfile,".find: target record %d not in data base %s.\n",
	    target_vect,File_name);
    goto abort_point;
  }
  case 1: break;
  case 2: break;
  default:
    {
      if(number=sscanf(inp_buf,"record %d %d",&target_vect,&count) )
	{
	  count = (number==2?count:1);
	  if (! get_loc_of_record(target_vect) ) {
	    fprintf(Outfile,".find: target record %d not in data base %s.\n",
		    target_vect,File_name);
	    goto abort_point;
	  }
	}
      else
	{
	  number=sscanf(inp_buf,"%lf %lf %d",&target_e,&target_n,&count);
	  if (number < 2){
	    fprintf(Outfile,".find: Invalid request coordinates\n");
	    goto abort_point;
	  }
	  if (number == 2) count=1;
	  if (count < 1) {
	    fprintf(Outfile,".find: Invalid number of records requested\n");
	    goto abort_point;
	  }
	}
      break;
    }				/* end of default case */
  }				/* end of switch */
  /* get first count rows */

  number = 0; pass = 0;
  error=crim(DATA_TABLE,"SELECT FROM DATA" ); /* do the select command */
  if (error > 0) rim_error(error);
  if (error == -1) {
    fprintf(Outfile,".find: No data in data base");
    goto abort_point;
  }
  /* get each row */
  while (number<count && crimdm(DATA_TABLE,GET,Rim_buffer) == 0)
    { 
      get_s_e_n_t_m();		/* get coordinates */
      
      if ( ((!Mask && in_wind(&Active_wind)) ||
	    (Mask && in_mask(pass++)) ) 
	  && in_target()  )
	{
	  put_vect_on_list( Last_record = (Record_list+number) );
	  number++;
	}
    }
  if (number == 0) goto exit_point;
  s1 = Last_record = Record_list + (number-1);
  s2 = Record_list + number;
  /* sort them */
  if (number > 1)
    qsort((char *)Record_list,number, sizeof(struct query_record),cmp_distance);
  
  /* exit if done or not enough rows */
  if (number < count) goto exit_point;
  
  /* get next rows from data base */
  while ((error=crimdm(DATA_TABLE,GET,Rim_buffer)) == 0)
    {
      get_s_e_n_t_m();
      
      if ( (!Mask && in_wind(&Active_wind)) ||
	  (Mask && in_mask(number) ) )
	{
	  put_vect_on_list( s2 );
	  
	  /* sort all entries  when new is closer than last entry */
	  if (cmp_distance(s1,s2) > 0) 
	    qsort((char *)Record_list, number+1,
		  sizeof (struct query_record),cmp_distance );
	}
    }
  
 exit_point:
#ifndef DBVECT
  report_records("find");
#endif
  if (pass) in_mask(-1);
  strcpy(Prompt, PROMPT);
  return (number);
 abort_point:
  strcpy(Prompt, PROMPT);
  return(-1);
}				/*  end of find() */


/* Routines to handle the query function.  Data base select command
	is assembled, then executed.  Record numbers, eastings, 
	northings, vector types, and reference map numbers are stored
	on Record_list for each record retrieved.  */

#define QUERY

#define Q_BUFFER_SIZE 800
#define QUERY_PROMPT "query"

static char *q_buffer = NULL;

query_init(inp_buf)
char *inp_buf;
{
	auto_print = 0;    /* assume no auto print */
	list = FALSE; add_list = FALSE;
	target_dist = 0.0; /* assume no distance mask */
	G_squeeze(inp_buf);
	set_mask_and_window(inp_buf); /* from .query line */

/* Allocate buffer space (once) and initialize record list pointers */
	if (q_buffer == NULL) q_buffer = G_malloc(Q_BUFFER_SIZE);

/* Initialize RIM request and user prompt */
	strcpy(q_buffer,"SELECT FROM DATA");
	strcpy(Prompt,QUERY_PROMPT);
	/*fprintf(Outfile,"%s>%s     [where ...] [sort by ...]  .end\n",
					Prompt,q_buffer);*/
}

query_line(inp_buf)
char *inp_buf;
{
int count;
	G_squeeze(inp_buf);
		/* get and interpret distance mask */
	switch (dist_clause(inp_buf) ) {
		case -1: {
			if (! get_loc_of_record(target_vect) )
 			{ fprintf(Outfile,".find: target record %d not in data base %s.\n",
				target_vect,File_name);
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
		auto_print = 1;
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
int error, offset, number, pass;
CELL in_mask();

	strcpy(Prompt, PROMPT);  /* reset prompt */
	Last_record = NULL; /* initialize to no records selected */

	number = 0; pass = 0;
	error=crim(DATA_TABLE,q_buffer); /* do the select command */
	if (error > 0) {
		if (error != 4) rim_error(error);
		fprintf(Outfile,"Invalid RIM select clause in .query");
#ifdef DBVECT
	SLEEP3;
#endif
	}
	else  /* do the work if successful select */
		if (error == 0)
		{
		/* get each row */
		while (crimdm(DATA_TABLE,GET,Rim_buffer) == 0) 
		{
		get_s_e_n_t_m(); /* get coordinates */

	/* check mask, window and target circle conditions */
		if ( ((!Mask && in_wind(&Active_wind)) ||
				( Mask && in_mask(pass++) ) )
				&& in_target() )
			{
			put_vect_on_list(	Last_record = (Record_list+number) );
			number++;
			if (auto_print) {
				fill_values();
				if (list==TRUE) print_list(add_list);
					else print_form();
				fprintf(Outfile, "\n");
				}
			}
		}
		}
/* clean up */
#ifndef DBVECT
	report_records("query");
#endif
	if (pass) in_mask(-1);
	return(number);
} /* end of query_done() */

set_mask_and_window(buf)
char *buf;
{
	int window_yes;
	Active_wind.rows = Active_wind.cols = 0;
	Mask = 0;
	window_yes = 0;
	/* check for mask/window use in this find/query request */
	G_tolcase(buf);
	while(*buf) {
		if ( !strncmp(buf," m",2) || !strncmp(buf," -m",3) ) {
			Mask = 1;
			break;
			}
		if ( !strncmp(buf," w",2) || !strncmp(buf, " c",2)
			 || !strncmp(buf," -w",3) || !strncmp(buf, " -c",3) )
			window_yes = 1;
		buf++;
		}
	if (window_yes || Mask) {
			if (G__get_window(&Active_wind,"","WIND",G_mapset()) != 1)
	/* above line is to be sure latest window is gotten. */
           G_fatal_error("Bad read of WIND in find/query");;
			G_set_window(&Active_wind);
	}
}

/* See if a point (coords in e,n) are inside or outside of
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
	case -1:{
			if(data_buf) free(data_buf);
			if(fd_data> -1) G_close_cell(fd_data);
			break;
			}
	case  0:	/* initialize, first time called */
			if(data_buf) free(data_buf);
			data_buf = NULL;
			value = 1;
			if(G_find_cell("MASK",G_mapset()) == NULL) {
				Mask = 0;  /* turn off masking request if no MASK */
				break;
				}
			G_get_set_window(&window); /* get current window */
			fd_data = G_open_cell_old("MASK", G_mapset());
			if (fd_data<0)
			G_fatal_error(
				"Failed to open existing MASK cell file in query or find");

			data_buf = G_allocate_cell_buf();

	default:		/* after the first time */
			if (!Mask || !data_buf)
				value = 1;
			else
			{
			/* check in bounds of current window--necessary for
				meaningful masking */
			if (!in_wind(&window) )
			 	value=0;
				else
				{
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

get_s_e_n_t_m()
{
  char tmpstr[INP_SIZ];

  retr_buf_i(Rim_buffer+OFFSET(Sequence_field) ,&s);
  retr_buf_d(Rim_buffer+OFFSET(East_field), &e);
  retr_buf_d(Rim_buffer+OFFSET(North_field), &n);
  retr_buf_t(Rim_buffer+OFFSET(Vect_type_field), tmpstr, 
	     Field_info[Vect_type_field].length);
  t = type_to_char(tmpstr);
  retr_buf_i(Rim_buffer+OFFSET(Map_field), &m);
}

report_records(pgm)
char *pgm;
{
int n;
struct query_record *s1;

	n = 0;
	if (Last_record)
		 n = 1+ (Last_record-Record_list);

	fprintf(Outfile,
		"\n%d records selected in %s from data base %s\n",n,pgm,File_name);
#ifdef DEBUG
	if (Last_record)
		for (s1=Record_list; s1<=Last_record; s1++)
			fprintf(Outfile,"E= %.6f  N= %.6f  Rec. Num.= %d\n",
					s1->east,s1->north,s1->record_number);
#endif
	if (!strcmp(pgm,"find") )
		fprintf(Outfile,
			"\nE= %.6f  N= %.6f was target location for .find\n",
				target_e,target_n);
}

put_vect_on_list(s1)
struct query_record *s1;
{
		s1->record_number = s;
		s1->north = n;
		s1->east = e;
		s1->vect_type = t;
		s1->map_num = m;
}

/* put e and n of record into globals target_e and target_n */
get_loc_of_record(rec_number)
int rec_number;
{
int error;
char cmd[50];

	sprintf(cmd,"select from data where %s eq %d",
           Field_info[Sequence_field].column_name, rec_number);
	error=crim(DATA_TABLE,cmd); /* do the select command */
	if (error > 0) rim_error(error);
	if (error == -1) {
		return(0);  /* no vect with that number */
		}
	/* get the row */
		if (crimdm(DATA_TABLE,GET,Rim_buffer) == 0 )
		{ 
		get_s_e_n_t_m();  /* get coordinates */
		target_e = e;
		target_n = n;
		}
	return(1);
}

  /* interpret distance from clause.  Return -1 for record requested
     not in data base, 0 for phrase not recognizable, 1 for "distance
     from record" properly interpreted, 2 for "distance from x y" read.
*/
dist_clause(inp_buf)
char *inp_buf;
{
if (sscanf(inp_buf,"distance from record %d %lf",
		&target_vect,&target_dist) == 2)
	if (get_loc_of_record(target_vect) )
		return (1);
	else
		return (-1);
	else
		if (sscanf(inp_buf,"distance from %lf %lf %lf",
			&target_e,&target_n,&target_dist) == 3)
				return (2);
	return (0);
}
