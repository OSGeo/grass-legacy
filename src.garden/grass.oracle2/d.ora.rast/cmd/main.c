#include "gis.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "idc.h"
/*#include "/home/cristian/pgsql/include/postgres.h"
#include "/home/cristian/pgsql/include/c.h"
#include "/home/cristian/pgsql/include/libpq-fe.h"
#include "/home/cristian/pgsql/include/utils/geo_decls.h"*/
#include </u/app/oracle/product/8.0.5/rdbms/demo/oci.h>
#include </u/app/oracle/product/8.0.5/rdbms/demo/oratypes.h>
#include </u/app/oracle/product/8.0.5/rdbms/demo/ocidfn.h>
#ifdef __STDC__
#include </u/app/oracle/product/8.0.5/rdbms/demo/ociapr.h>
#else
#include <ocikpr.h>
#endif
#include </u/app/oracle/product/8.0.5/rdbms/demo/ocidem.h>


/* oparse flag */
#define  DEFER_PARSE	1
#define  NATIVE		1
#define	 VERSION_7	2

/* exit flags */
#define  OCI_EXIT_FAILURE 1
#define  OCI_EXIT_SUCCESS 0

#define  HDA_SIZE 256
#define  NO_DATA_FOUND 1403

Lda_Def lda;
ub4	hda[HDA_SIZE/(sizeof(ub4))];
Cda_Def cda;
//#include ""

long cell[180][205];
double nv,sv,ov,ev,nsv,eov;
double x,y,q;
char sqlstr[200];
text cname[100];
uword row_count = 0;
uword tabnum = 0;

#define TYTIME ITIMER_VIRTUAL

/* Function prototypes*/
void logon();
void logoff();
void setup();
void err_report();
void get_data();
void do_exit();


void logon()
{
  if (olog(&lda, (ub1 *)hda, (text *)"mdsys", -1, (text *)"ora99", -1,
           (text *)0, -1, (ub4)OCI_LM_DEF))
  {
    err_report((Cda_Def *)&lda);
    exit(OCI_EXIT_FAILURE);
  }
  printf("\n Connected to ORACLE as MDSYS\n");
}

void logoff()
{
  if (oclose(&cda))
  {
    fprintf(stderr, "Error closing cursor 1.\n");
    do_exit(OCI_EXIT_FAILURE);
  }
  if (ologof(&lda))
  { 
    fprintf(stderr, "Error on disconnect.\n");
    do_exit(OCI_EXIT_FAILURE);
  }
}

void setup()
{
  if (oopen(&cda, &lda, (text *) 0, -1, -1, (text *) 0, -1))         /* open */
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }
  if (oparse(&cda, sqlstr, (sb4) -1, DEFER_PARSE,                  /* parse */
               (ub4) VERSION_7))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }
/*  if (obndrv(&cda, (text *)":tabname", -1,
             (ub1 *)tabname, (sword)sizeof(tabname),
             SQLT_STR, -1, (sb2 *) 0, (text *) 0, -1, -1))            bind
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);   
  } */
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
  if (odefin(&cda, 3, (ub1 *) &q, (sword) sizeof(long),
             (sword) SQLT_FLT,
             (sword) -1, (sb2 *) 0, (text *) 0, -1, -1,
             (ub2 *) 0, (ub2 *) 0))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }
}


void get_data()
{
  int xx,yy;
  if (oexec(&cda))
  {
    err_report(&cda);
    do_exit(OCI_EXIT_FAILURE);
  }
  while (ofetch(&cda) == 0)
  {
    row_count++;                          /* bump this for printing purposes */
    if (cda.rc==NULL_VALUE_RETURNED)
        err_report(&cda);
    xx=(x-ov)/eov;
    yy=(nv-y)/nsv;
    if ((xx<0) ||(yy<0))
      printf("\n\n NEGATIVI!!!!!!!\n\n");
    if ((xx>204) ||(yy>179))
      printf("\n\n STICAZZI!!!!\n\n");
    cell[yy][xx]=q;
  }
  if (cda.rc != NO_DATA_FOUND)
    err_report(&cda);
  printf("%s%d","row in query output=",row_count);
  row_count = 0;                                     /* reset for next table */
}  



void err_report(cursor)
Cda_Def *cursor;
{
    sword n;
    text msg[512];                      /* message buffer to hold error text */

    if (cursor->fc > 0)
      printf("\n-- ORACLE error when processing OCI function %s \n\n",
            oci_func_tab[cursor->fc]);
    else
      printf("\n-- ORACLE error\n");

    n = (sword)oerhms(&lda, cursor->rc, msg, (sword) sizeof msg);
    fprintf(stderr, "%s\n", msg);
}   

void do_exit(status)
eword status;
{
  if (status==OCI_EXIT_FAILURE)
    printf("\n Exiting with FAILURE status %d \n",status);
  else
    printf("\n Exiting with SUCCESS status %d\n",status);
  exit(status);
} 


struct
{
  struct Option *mappa,*nord,*sud,*est,*ovest,*nsres,*eores;
}parm;


disegna(double nv,double sv,double ev,double ov,double nsv,double eov)
{
  int t, b, l, r, i,j;
  struct Colors colori;
  char buff[50];
  struct Cell_head regione;

  R_open_driver();
  regione.format=3;
  regione.compressed=0;
  regione.proj=0;
  regione.zone=0;
  regione.ew_res=eov;
  regione.ns_res=nsv;
  regione.north=nv;
  regione.south=sv;
  regione.east=ev;
  regione.west=ov;
  regione.rows=(nv-sv)/nsv;
  regione.cols=(ev-ov)/eov;
  G_put_window(&regione);
  G_init_colors(&colori);
  G_add_color_rule(-4246,0,0,2,-2765,0,0,155,&colori);
  G_add_color_rule(-2765,0,0,155,-1503,0,0,200,&colori);
  G_add_color_rule(-1503,0,0,200,-170,0,0,255,&colori);
  G_add_color_rule(-170,0,0,255,0,0,255,255,&colori);
  G_add_color_rule(0,0,255,0,150,0,75,0,&colori);
  G_add_color_rule(150,0,75,0,220,30,75,20,&colori);
  G_add_color_rule(220,30,75,20,400,60,75,20,&colori);
  G_add_color_rule(400,60,75,20,839,90,70,30,&colori);
  G_add_color_rule(839,90,70,30,3511,255,255,255,&colori); 
  D_set_colors(&colori);
  D_get_screen_window(&t, &b, &l, &r) ;
  if (D_cell_draw_setup(t,b,l,r))
  {
    sprintf(buff,"Cannot use current window") ;
    G_fatal_error(buff) ;
  }
  D_set_overlay_mode(1);
    for (i = 0; i<178;i++)
  {
    D_draw_cell(i, (CELL *)&cell[i], &colori) ;
  }
  R_flush() ;
  R_close_driver();
}


/*
prendi_dati(double nv,double ov,double nsv,double eov)
{
  PGconn *conn;
  PGresult *res;
  int q_num,c_num;
  char *pghost,*pgport,*pgoption,*pgtty;
  char dbName[20];
  char query[100];
  int i,numt,numcols,plen,x ,y;
  BOX coord;
  
  pghost=NULL;
  pgport=NULL;
  pgoption=NULL;
  pgtty=NULL;
  strcpy(dbName,"cristian");
  conn=PQsetdb(pghost,pgport,pgoption,pgtty,dbName);
  if (PQstatus(conn) == CONNECTION_BAD)
  {
    printf("%s\n","Connessione al db cristian non riuscita!");
    esci(conn);
  }
  numcols=36;
  strcpy(query,"BEGIN");
  res=PQexec(conn,query);
  if (PQresultStatus(res)!= PGRES_COMMAND_OK)
  {
    printf("%s\n","BEGIN fallito!");
    PQclear(res);
    esci(conn);  
  }
  PQclear(res);
  sprintf(query,"DECLARE cursore BINARY CURSOR FOR SELECT * FROM %s WHERE coord@'((%s,%s),(%s,%s))';", parm.mappa->answer, parm.ovest->answer, parm.sud->answer, parm.est->answer, parm.nord->answer);
  //printf("%s\n",query);
  res=PQexec(conn,query);
  if (PQresultStatus(res)!= PGRES_COMMAND_OK)
  {
    printf("%s\n","Comando non accettato!");
    PQclear(res);
    esci(conn);  
  }
  PQclear(res);
  strcpy(query,"FETCH ALL in cursore;");
  res=PQexec(conn,query);
  if ((PQresultStatus(res)== PGRES_FATAL_ERROR) || (PQresultStatus(res)== PGRES_NONFATAL_ERROR))
  {
    printf("%s\n","NOT FETCHED");
    PQclear(res);
    esci(conn);  
  }
  q_num=PQfnumber(res,"quota");
  c_num=PQfnumber(res,"coord");
  for (i=0;i<PQntuples(res);i++)
  {
    memmove(&coord,(BOX *)PQgetvalue(res,i,c_num),sizeof(coord)); 
    x=(coord.low.x-ov)/eov;
    y=(nv-coord.low.y)/nsv;    
    memmove(&cell[y][x],(int *)PQgetvalue(res,i,q_num),sizeof(int));  
  }
  res=PQexec(conn,"CLOSE cursore");
  PQclear(res);
  res=PQexec(conn,"COMMIT");
  PQclear(res);
  PQfinish(conn);
}
*/

main(argc, argv)
     int argc;
     char **argv;
{
  char tabname[100]; 
  usage_t usage;
     
        G_gisinit (argv[0]);

	parm.mappa = G_define_option();
	parm.mappa->key = "tabname";
	parm.mappa->type = TYPE_STRING;
	parm.mappa->required = YES;
	parm.mappa->description = "Name of Oracle table containing raster data";

	parm.nord = G_define_option();
	parm.nord->key = "North";
	parm.nord->type = TYPE_DOUBLE;
	parm.nord->required = YES;
	parm.nord->description = "Northern limit raster map";
        
        parm.sud = G_define_option();
	parm.sud->key = "South";
	parm.sud->type = TYPE_DOUBLE;
	parm.sud->required = YES;
	parm.sud->description = "Southern limit raster map";

        parm.ovest = G_define_option();
	parm.ovest->key = "West";
	parm.ovest->type = TYPE_DOUBLE;
	parm.ovest->required = YES;
	parm.ovest->description = "Western limit raster map";

        parm.est = G_define_option();
	parm.est->key = "East";
	parm.est->type = TYPE_DOUBLE;
	parm.est->required = YES;
	parm.est->description = "Eastern limit raster map";
        
        parm.nsres = G_define_option();
	parm.nsres->key = "NSres";
	parm.nsres->type = TYPE_DOUBLE;
	parm.nsres->required = YES;
	parm.nsres->description = "North-South resolution (>= real N-S map resolution)";

        parm.eores = G_define_option();
	parm.eores->key = "ESres";
	parm.eores->type = TYPE_DOUBLE;
	parm.eores->required = YES;
	parm.eores->description = "East-West resolution (>= real E-W map resolution)";

	if (G_parser(argc,argv))
		exit(1);
        else
        {
           getusage(&usage);
           fprintf(stdout,"cnt %9d cpu(us) %9d  time(us) %9d load %06.3f%%"
                                                " tic/us %4.3f\n",
                        usage.cnt, usage.cpu_usec, usage.t_usec,
                        ((float)usage.cpu_usec/usage.t_usec)*100.0,
                        (float)usage.cnt/usage.t_usec);
        }
        strcpy(tabname,parm.mappa->answer);
        sscanf(parm.nord->answer,"%lf",&nv) ;
        sscanf(parm.sud->answer,"%lf",&sv) ;
        sscanf(parm.est->answer,"%lf",&ev) ;
        sscanf(parm.ovest->answer,"%lf",&ov) ;
        sscanf(parm.nsres->answer,"%lf",&nsv) ;
        sscanf(parm.eores->answer,"%lf",&eov) ;
        //printf("%s\n","input & conversioni OK");
    
     //prendi_dati( nv,ov,nsv,eov);
     sprintf(sqlstr,"SELECT sdo_x1,sdo_y1,sdo_q1 FROM %s_sdogeom where (sdo_x1<%lf) and (sdo_x1>%lf)
            and (sdo_y1<%lf) and (sdo_y1>%lf)",tabname,nv,sv,ev,ov);
     printf("\n%s\n",sqlstr); 
     getusage(&usage);
           fprintf(stdout,"cnt %9d cpu(us) %9d  time(us) %9d load %06.3f%%"
                                                " tic/us %4.3f\n",
                        usage.cnt, usage.cpu_usec, usage.t_usec,
                        ((float)usage.cpu_usec/usage.t_usec)*100.0,
                        (float)usage.cnt/usage.t_usec);
     getusage(&usage);
     logon();
     getusage(&usage);
           fprintf(stdout,"\n logon cnt %9d cpu(us) %9d  time(us) %9d load %06.3f%%"
                                                " tic/us %4.3f\n",
                        usage.cnt, usage.cpu_usec, usage.t_usec,
                        ((float)usage.cpu_usec/usage.t_usec)*100.0,
                        (float)usage.cnt/usage.t_usec);
     getusage(&usage);
     setup();
     getusage(&usage);
           fprintf(stdout,"\n setup cnt %9d cpu(us) %9d  time(us) %9d load %06.3f%%"
                                                " tic/us %4.3f\n",
                        usage.cnt, usage.cpu_usec, usage.t_usec,
                        ((float)usage.cpu_usec/usage.t_usec)*100.0,
                        (float)usage.cnt/usage.t_usec);
     getusage(&usage);
     get_data();
     getusage(&usage);
           fprintf(stdout,"\n getdata cnt %9d cpu(us) %9d  time(us) %9d load %06.3f%%"
                                                " tic/us %4.3f\n",
                        usage.cnt, usage.cpu_usec, usage.t_usec,
                        ((float)usage.cpu_usec/usage.t_usec)*100.0,
                        (float)usage.cnt/usage.t_usec);
     getusage(&usage);
     logoff();
     do_exit(OCI_EXIT_SUCCESS);
     getusage(&usage);
           fprintf(stdout,"logoff cnt %9d cpu(us) %9d  time(us) %9d load %06.3f%%"
                                                " tic/us %4.3f\n",
                        usage.cnt, usage.cpu_usec, usage.t_usec,
                        ((float)usage.cpu_usec/usage.t_usec)*100.0,
                        (float)usage.cnt/usage.t_usec);
    disegna( nv,sv,ev,ov,nsv,eov);
     getusage(&usage);
           fprintf(stdout,"cnt %9d cpu(us) %9d  time(us) %9d load %06.3f%%"
                                                " tic/us %4.3f\n",
                        usage.cnt, usage.cpu_usec, usage.t_usec,
                        ((float)usage.cpu_usec/usage.t_usec)*100.0,
                        (float)usage.cnt/usage.t_usec);
}
