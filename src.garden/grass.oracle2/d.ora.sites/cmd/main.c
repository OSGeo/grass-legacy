#include "gis.h"
 
#include "options.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/////////////////////////////////////////////////////////////////////////////
//
//  VERY IMPORTANT
//  modify next includes with right paths 
//
/////////////////////////////////////////////////////////////////////////////

#include </u/app/oracle/product/8.0.5/rdbms/demo/oci.h>
#include </u/app/oracle/product/8.0.5/rdbms/demo/oratypes.h>
/* LDA and CDA struct declarations */
#include </u/app/oracle/product/8.0.5/rdbms/demo/ocidfn.h>
#ifdef __STDC__
#include </u/app/oracle/product/8.0.5/rdbms/demo/ociapr.h>
#else
#include <ocikpr.h>
#endif
#include </u/app/oracle/product/8.0.5/rdbms/demo/ocidem.h>


// following flags & constants needed by oracle routines

/* oparse flags */
#define  DEFER_PARSE        1
#define  NATIVE             1
#define  VERSION_7          2

/* exit flags */
#define OCI_EXIT_FAILURE 1
#define OCI_EXIT_SUCCESS 0

#define HDA_SIZE 256
#define NO_DATA_FOUND 1403

// following variables (and apparently unuseful include) needed by oracle
Lda_Def lda;                                                   /* login area */
ub4     hda[HDA_SIZE/(sizeof(ub4))];                            /* host area */
Cda_Def cda;                                                  /* cursor a
#include ""

#define TYTIME ITIMER_REAL


int n;
int colore;
char nometab[20];
uword tabnum = 0;                                            /* table number */
uword row_count = 0;  
char sqlstr[200];
double xtop=36.5;
double ytop=6.5;
text cname[100];
struct Cell_head window;
int type,size;
double x,y;


/* Function prototypes */
void logon ();
void logoff ();
void setup();
void err_report();
void get_data();
void do_exit();

//////////////////////////////////////////////////////////////////////
//
// logon procedure: connects user to an oracle database
//
//////////////////////////////////////////////////////////////////////
void logon(char *user, char *passwd)
{
  if (olog(&lda, (ub1 *)hda, (text *)user, -1, (text *)passwd, -1, 
	   (text *)0, -1, (ub4)OCI_LM_DEF))
  {
    err_report((Cda_Def *)&lda);
    exit(OCI_EXIT_FAILURE);
  }
  printf("\n Connected to ORACLE as <%s> \n",user);
}

//////////////////////////////////////////////////////////////////////
//
// logoff procedure
//
//////////////////////////////////////////////////////////////////////

void logoff()
{
  if (oclose(&cda))                                          /* close cursor */
  {
    fprintf(stderr, "Error closing cursor 1.\n");
    do_exit(OCI_EXIT_FAILURE);
  }
  if (ologof(&lda))                                  /* log off the database */
  {
    fprintf(stderr, "Error on disconnect.\n");
    do_exit(OCI_EXIT_FAILURE);
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  setting up query and binds variables & columns 
//
///////////////////////////////////////////////////////////////////////////////
void setup()
{
  /* open */
  if (oopen(&cda, &lda, (text *) 0, -1, -1, (text *) 0, -1))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  /* parse */
  if (oparse(&cda, sqlstr, (sb4) -1, DEFER_PARSE,
               (ub4) VERSION_7))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  /* bind x and y columns */
  if (odefin(&cda, 1, (ub1 *) &y, (sword) sizeof(double),
	     (sword) SQLT_FLT,
	     (sword) -1, (sb2 *) 0, (text *) 0, -1, -1,
	     (ub2 *) 0, (ub2 *) 0))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }
  if (odefin(&cda, 2, (ub1 *) &x, (sword) sizeof(double),
	     (sword) SQLT_FLT,
	     (sword) -1, (sb2 *) 0, (text *) 0, -1, -1,
	     (ub2 *) 0, (ub2 *) 0))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }
}


///////////////////////////////////////////////////////////////////////////////
//
//  all-in-one execute query, get data and call draw point
//
////////////////////////////////////////////////////////////////////////////////
void get_data()
{
  /* execute query */
  if (oexec(&cda))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }

  /* retrieve every single tuple */
  while (ofetch(&cda) == 0)
  {
    row_count++;			//Point number?                          
    switch(type)
    {
      case TYPE_X:			//What kind of symbol should i drive?
	draw_points_x(&window) ;
	break ;
      case TYPE_PLUS:
	draw_points_plus(&window) ;
	break ;
      case TYPE_BOX:
	draw_points_box(&window) ;
	break ;
      case TYPE_DIAMOND:
	draw_points_diamond(&window) ;
	break ;
    }  
  }

  /* this if for no-result query */
  if (cda.rc != NO_DATA_FOUND)
    err_report(&cda);
  printf("%s%d","row in query output=",row_count);
  row_count = 0;                                     /* reset for next table */
}

///////////////////////////////////////////////////////////////////////////////
//
//  error management function
//
///////////////////////////////////////////////////////////////////////////////
void err_report(cursor)
Cda_Def *cursor;
{
    sword n;
    text msg[512];                      /* message buffer to hold error text */
    if (cursor->fc > 0)
      printf("\n ORACLE error when processing OCI function %s \n\n", 
            oci_func_tab[cursor->fc]);
    else
      printf("\n ORACLE error.\n");
    n = (sword)oerhms(&lda, cursor->rc, msg, (sword) sizeof msg);
    fprintf(stderr, "%s\n", msg);
}


//////////////////////////////////////////////////////////////////////////////
//
//  do_exit  
//
//////////////////////////////////////////////////////////////////////////////
void do_exit(status)
eword status;
{
  if (status==OCI_EXIT_FAILURE)
    printf("\n Exiting with FAILURE status %d \n",status);
  else
    printf("\n Exiting with SUCCESS status %d\n",status);
  exit(status);
}


/////////////////////////////////////////////////////////////////////////////
//
// the following four routines draw a single simbol at x,y
// just like original grass d.sites command
//
/////////////////////////////////////////////////////////////////////////////
draw_points_diamond(x,y)
        double x,y;
{
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;
		D_X = (int)D_u_to_d_col(x) ;
		D_Y = (int)D_u_to_d_row(y) ;
		R_move_abs(D_X     , D_Y+size) ;
		R_cont_abs(D_X+size, D_Y     ) ;
		R_cont_abs(D_X     , D_Y-size) ;
		R_cont_abs(D_X-size, D_Y     ) ;
		R_cont_abs(D_X     , D_Y+size) ;
}

draw_points_box(window,x,y)
	struct Cell_head *window;
        double x,y;
{
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;
		D_X = (int)D_u_to_d_col(x) ;
		D_Y = (int)D_u_to_d_row(y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_cont_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y-size) ;
}

draw_points_plus(window,x,y)
	struct Cell_head *window;
        double x,y;
{
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;
		D_X = (int)D_u_to_d_col(x) ;
		D_Y = (int)D_u_to_d_row(y) ;
		R_move_abs(D_X-size, D_Y     ) ;
		R_cont_abs(D_X+size, D_Y     ) ;
		R_move_abs(D_X     , D_Y-size) ;
		R_cont_abs(D_X     , D_Y+size) ;
}

draw_points_x(x,y)
        double x,y; 
{
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;
                D_X = (int)D_u_to_d_col(x) ;
		D_Y = (int)D_u_to_d_row(y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_move_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
}


//////////////////////////////////////////////////////////////////////////////
//
//  main routine
//
//////////////////////////////////////////////////////////////////////////////

main(argc, argv)
int argc ;
char **argv ;
{
	char *desc;
	char *mapset;
	char buff[128] ;
	char msg[200];
	char window_name[64] ;
	char *D_color_list();
	double east, north;
	int i ;
	int t, b, l, r ;
	//struct Cell_head window ;
	struct Option *opt1, *opt2, *opt3, *opt4, *opt5, *opt6;
        struct Cell_head temp_window;
	char *G_align_window();
        char nomecolore[20];
        int colore;
        char nometab[20]; 
        char user[20],passwd[20];
 
        opt4 = G_define_option() ;
	opt4->key        = "sitefile";
	opt4->type       = TYPE_STRING;
	opt4->required   = YES;
	opt4->description= "Name of an Oracle site table" ;
     
        opt5 = G_define_option() ;
	opt5->key        = "sitefile";
	opt5->type       = TYPE_STRING;
	opt5->required   = YES;
	opt5->description= "Name of an Oracle user" ;

        opt6 = G_define_option() ;
	opt6->key        = "sitefile";
	opt6->type       = TYPE_STRING;
	opt6->required   = YES;
	opt6->description= "Password for Oracle user" ;

	opt1 = G_define_option() ;
	opt1->key        = "color" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO ;
	opt1->answer     = "gray" ;
	opt1->options    = D_color_list();
	opt1->description= "Sets the current color " ;

	opt2 = G_define_option() ;
	opt2->key        = "size" ;
	opt2->type       = TYPE_INTEGER ;
	opt2->required   = NO ; 
	opt2->answer     = "5" ;
	opt2->options    = "0-1000" ;
	opt2->description= "Size, in pixels, in which the icon is to be drawn" ;

	opt3 = G_define_option() ;
	opt3->key        = "type" ;
	opt3->type       = TYPE_STRING ;
	opt3->required   = NO ;
	opt3->answer     = "x" ;
	opt3->options    = "x,diamond,box,+" ;
	opt3->description= "Specify the type of the icon" ;

	G_gisinit(argv[0]) ;

	if (G_parser(argc, argv))
		exit(-1);
        sscanf(opt2->answer,"%d",&size);
        strcpy(nomecolore,opt1->answer);
        strcpy(nometab,opt4->answer);
        strcpy(user,opt5->answer);
	strcpy(passwd,opt6->answer);

        colore=D_translate_color(nomecolore);
	R_standard_color(colore) ;

        if (! strcmp(opt3->answer, "x"))
		type = TYPE_X ;
	else if (! strcmp(opt3->answer, "+"))
		type = TYPE_PLUS ;
	else if (! strcmp(opt3->answer, "box"))
		type = TYPE_BOX ;
	else if (! strcmp(opt3->answer, "diamond"))
		type = TYPE_DIAMOND ;
           
        R_open_driver();

        if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen frame") ;
        G_get_window(&window);
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

        sprintf(sqlstr,"%s%s%s%f%s%f%s%f%s%f%s","SELECT sdo_x1,sdo_y1 FROM ",nometab,"_sdogeom 
                        WHERE (sdo_x1 < ",window.north,") AND (sdo_x1 > ",window.south,") 
                        AND (sdo_y1 < ",window.east,") AND (sdo_y1 > ",window.west,")"); 
        
        logon(user,passwd);				/* logon to Oracle database */
        setup();				/* prepare sql statement */
        get_data(type);				/* retrieve data and draw points*/
        logoff();				/* logoff Oracle database */
        do_exit(OCI_EXIT_SUCCESS);		/* printing "success" or "failure" */

	R_close_driver();
 	exit(0);
}
