
#include "gis.h"
#include "globals.h"

#define COORD_MENU

static double local_target_dist=0.0;
static double local_target_e=0.0, local_target_n=0.0;

static char local_target_site[10]="";
static char count_sites[10]="";
static char current[2]="";
static char respect_mask[2]="";
static char target_yes[2]="";
static char nearest_yes[2]="";
static char mouse[2]="";

/* Present menu to set params for query/find  */

set_coord_menu(caller)
char *caller;
{
int n, pos;
char buf[100];
char reset_yes[2];
struct Cell_head window;

if (! *File_name ) {
	G_warning("You must open a data base before making this choice.");
		SLEEP3;
	return (-1);
}
G_get_window(&window);  /* get current window */


while(1)   /* create screen */
{
	V_clear();
	*reset_yes = '\0';
	n = 0;
	V_line(n++,"            QUERY/FIND:  WINDOW/MASK/TARGET SELECTION MENU");
	V_line(n, make_line_2() );

	n += 2;
	V_line(n,"       Mark requests with 'x' and enter required values.");
	n += 2;
	V_line(n , "                  Respect current window");
	V_ques(current,'s',n,41,1);

 if (G_find_cell("MASK",G_mapset() )) {
	n += 2;
	V_line(n,  "                  Respect current MASK");
	V_ques(respect_mask,'s',n,41,1);
	V_line(++n,"                 (forces current window)");
	}
	else *respect_mask = '\0';

	n += 2;
	V_line(n,  "A.  Find all sites within (or outside) a circular target ");
	V_ques(target_yes,'s',n,58,1);
	V_line(++n,"         and give the radius (negative for outside)");
	V_ques(&local_target_dist,'d',n,53,12);

 if(!strcmp(caller,"FIND")) {
	V_line(++n,"                        OR");
	V_line(++n,"B.  Find a number of sites nearest a point");
	V_ques(nearest_yes,'s',n,43,1);
	V_line(++n,"         and the number of sites requested");
	V_ques(count_sites,'s',n,43,7);

	n += 2;
	V_line(n,"After selecting A or B, complete one(!) of these lines:");
	}
 else {
	*nearest_yes = '\0';
	*count_sites = '\0';
	n += 2;
	V_line(n++,"        If you selected A, complete one(!) of these lines:");
	}
 if (G__getenv("MONITOR") != NULL) {
	V_line(++n,"        1. To select target point with mouse");
	V_ques(mouse,'s',n,46,1);
	}
 V_line(++n,  "        2. Enter site number for target point");
 V_ques(local_target_site,'s',n,46,7);

 sprintf(buf, "        3. Target coordinates %s            %s",
		Field_info[East_field].column_name,
		Field_info[North_field].column_name);
 V_line(++n,buf);
 pos = 30+strlen(Field_info[East_field].column_name)+1;
 V_ques(&local_target_e,'d',n,pos,10);
 pos = pos + strlen(Field_info[North_field].column_name)+11+1;
 V_ques(&local_target_n,'d',n,pos,11);

 n += 2;
 V_line(n,"           Reset to default choices for this menu");
 V_ques(reset_yes,'s',n,50,1);

/* Show screen and get choice */
 V_intrpt_ok(); /* allow for a bailout */
 if (! V_call() ) return (-1);

 if (*reset_yes == 'x') {reset_all(); continue; }
 else 
	break;
}
 if (*mouse == 'x')  /* Get east and north with pointer */
	get_e_n_with_mouse();
 return(1);
} /* end of set_coord_menu() */

reset_all()
{
 *current=0;  *respect_mask=0; *target_yes=0;
 *nearest_yes=0; *mouse=0;
 *local_target_site = 0; *count_sites = 0;
 local_target_dist = local_target_e = local_target_n = 0.0;
}


disp_maps(line1)
char *line1;
{
	struct query_site *s;
	char work[240];
	FILE *fp;
	char cell_file[20],vect_file[20], site_file[20], color_name[10];
	char curr_sites[2], erase[2];
	char size1[3],size2[3],type1[8],type2[8],colr1[10],colr2[10];
	char erase_color[10];

	strcpy(size1,"3");strcpy(size2,"6");
	strcpy(type1,"box");strcpy(type2,"x");
	strcpy(colr1,"white");strcpy(colr2,"red");
	strcpy(erase_color,"black");
	*cell_file=0; *vect_file=0; *site_file=0; *color_name = 0;
	*curr_sites = 0; *erase=0;

	V_clear();
	V_line( 1, line1);
	V_line( 3,"Enter cell and/or vector map names, if desired");
	V_line( 5,"                 Cell file to display");
	V_line( 7,"                 Vector file to display in color: ");
	V_line( 9,"                 Site list to display");
	V_line(10,"                 Dpoints with: size=   type=        color=");

	if(Last_site != NULL) {
	V_line(12,"                 Display currently selected sites (enter x)");
	V_line(13,"                 Dpoints with: size=   type=        color=");
	}
	V_line(15,"                 Erase graphics screen (enter x)");
	V_line(16,"                 Derase");
	V_ques(cell_file,'s',5,1,14);
	V_ques(vect_file,'s',7,1,14);
	V_ques(color_name, 's', 7, 50, 9);
	V_ques(site_file,'s',9,1,14);
	V_ques(size1,'s',10,36,2);
	V_ques(type1,'s',10,44,7);
	V_ques(colr1,'s',10,58,9);

	if(Last_site != NULL) {
	V_ques(curr_sites,'s',12,15,1);
	V_ques(size2,'s',13,36,2);
	V_ques(type2,'s',13,44,7);
	V_ques(colr2,'s',13,58,9);
	}
	V_ques(erase,'s',15,15,1);
	V_ques(erase_color,'s',16,25,9);
	V_intrpt_ok();
	if (V_call())
	{
	if (*erase) {
		sprintf(work,"Derase %s",erase_color);
		G_system(work);
	}
	if (*cell_file) {
		sprintf(work,"%s/bin/Dcell %s",G_gisbase(), cell_file);
		G_system(work);
		}
	if (*vect_file) {
		sprintf(work,"%s/bin/Dvect %s %s >/dev/null",
			 G_gisbase(), vect_file, color_name);
		G_system(work);
		}
        if (*site_file) {
		sprintf(work,
		"%s/bin/Gsites %s | %s/bin/Dpoints size=%s type=%s color=%s",
	 	G_gisbase(), site_file,G_gisbase(),size1,type1,colr1 );
		G_system(work);
		}
	if (*curr_sites && (Last_site != NULL)) {
		sprintf(work,
		"Dpoints size=%s type=%s color=%s",size2,type2,colr2 );
		fp = popen(work,"w");
		for(s=Site_list; s<=Last_site; s++)
			fprintf(fp,"%lf %lf\n",s->east,s->north);
		pclose(fp);
		}
	}
}

get_e_n_with_mouse()
{
	FILE *fp;
	disp_maps(
	"          OPTIONALLY DISPLAY MAP(S) FOR REFERENCE BEFORE USING MOUSE");
	fp = popen("Dwhere m=quiet","r");
	fscanf(fp,"%lf %lf", &local_target_e, &local_target_n);
	pclose (fp);
}


do_find()
{
char cmd[100];
int n;

/* set up the info for find/query */
if (set_coord_menu("FIND") == -1) return;

/* call find_init with proper mask, window request */
sprintf(cmd,".find %s %s",
		(*respect_mask=='x')? "mask" : "",
		(*current=='x')? "window" : "");
find_init(cmd);

*cmd = '\0';

if (*target_yes == 'x')
	if (sscanf(local_target_site,"%d",&n)==1)
		sprintf(cmd,"distance from site %d %f", n, local_target_dist);
	else
		sprintf(cmd,"distance from %f %f %f",
			local_target_e,local_target_n,local_target_dist);

if (*nearest_yes=='x' && *local_target_site) {
	if (sscanf(count_sites,"%d",&n)==1)
		sprintf(cmd,"site %s %d", local_target_site , n);
	else
		sprintf(cmd,"site %s", local_target_site);
	}
if (*nearest_yes=='x' && !*local_target_site) {
	if (sscanf(count_sites,"%d", &n)==1)
		sprintf(cmd,"%f %f %d",local_target_e,local_target_n, n);
	else
		sprintf(cmd,"%f %f",local_target_e,local_target_n);
	}
if(*cmd) {
	find(cmd);
	view_sites();
	}
else {
	G_warning("Improperly set up target parameters for find.");
	SLEEP3;
	}
}

#define LINES 14
#define COLS 80

do_query()
{
char *p, c[LINES][COLS]; /* blank lines for V_ask */
char init_cmd[100], cmd[100], buf[100], buf2[100];
int i, n, exit_flag;

 if (set_coord_menu("QUERY") == -1) return;

 /* call query_init with proper mask, window request */
 sprintf(init_cmd,".query %s %s",
		(*respect_mask=='x')? "mask" : "",
		(*current=='x')? "window" : "");



 *cmd = '\0';

 if (*target_yes == 'x')
	if (sscanf(local_target_site,"%d",&n)==1)
		sprintf(cmd,"distance from site %d %f", n, local_target_dist);
	else
		sprintf(cmd,"distance from %.2f %.2f %.2f",
			local_target_e,local_target_n,local_target_dist);

 for (n=0;n<LINES;n++) 
	*c[n] = 0;
 exit_flag = 0;

 while(exit_flag==0)
 {
 query_init(init_cmd);

 V_clear();
 n = 0;
 V_line(n++,"                 SQL QUERY COMMAND ENTRY SCREEN");
 V_line(n++,make_line_2() );
 
 strcpy(buf,"    The SQL select query will ");
 if (*current=='x' || *respect_mask=='x')
		  strcat(buf,"use the current window");
	else strcat(buf,"not use window or mask");
 if (*respect_mask=='x') strcat(buf," and mask");

 V_line(n++,buf);
 if (*cmd) {
	sprintf(buf2,"   and a target clause of '%s'",cmd);
	V_line(n++,buf2);
	}

 n +=2;
 for (i=n; i<LINES; i++)
	V_ques(c[i],'s',i,0,79);

 V_intrpt_ok();
 if (!V_call()) return(0);

 if (*cmd) query_line(cmd);

 for (i=n; i<LINES; i++)
	{
	p = c[i]+strspn(c[i]," \t");
	
        exit_flag = 1;
	if (! strncmp(p,".end", 4) ) 
		break;
	if (! strncmp(p,".show", 5) )
		{
		do_show();
		*c[i] = '\0';
		exit_flag = 0;
		break;
		}

	if (*p)
		query_line(p);
	}
 }
 query_done();

 view_sites();
}  /* end of do_query() */

