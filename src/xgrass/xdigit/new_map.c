/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
#include "Browser.h"
#include "xgrass_lib.h"

#include <signal.h>
#include <stdio.h>
#include "gis.h"
#include "dig_head.h"
#include "digit.h"
#include "wind.h"

static int have_old;
static int have_plus;

void
new_map (parent)
    Widget parent;
{
    Widget dialog;
    XmString t = XmStringCreateSimple ("Enter name of new file:");
    XmString title = XmStringCreateSimple ("Create new file");
    Arg wargs[5];
    int n=0;

    XtSetArg (wargs[n], XmNselectionLabelString, t); n++;
    XtSetArg (wargs[n], XmNautoUnmanage, True); n++;
    XtSetArg (wargs[n], XmNdialogTitle, title); n++;
    dialog = XmCreatePromptDialog (toplevel, "", wargs, n);
    XtVaSetValues (XtParent(dialog), 
		XmNsaveUnder, True,
		NULL);

    XtAddCallback (dialog, XmNokCallback, checkfile, parent);
    XtAddCallback (dialog, XmNcancelCallback, XtDestroyWidget, NULL);
    XtAddCallback (dialog, XmNokCallback, XtDestroyWidget, NULL);

    XtUnmanageChild(XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
    XtManageChild (dialog);
    XtFree (t);
    XtFree (title);
}

void 
checkfile (w, wig, cbs)
    Widget w, wig;
    XmSelectionBoxCallbackStruct *cbs;

{
    char *file;
    char buf[100];
  
    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET,&file);

    if (!file || !*file)
    {
	if (file)
	    XtFree(file);
    }
    else
    {
	if (G_find_file2 ("dig", file, ""))
        {
	    sprintf (buf, "%s already exists. Choose a new name", file);
	    make_monolog (1, buf);
        }
        else
        {
	    check_proj (wig, file);
        }
    }
   

}


void
old_map (w)
    Widget w;
{
    char *file;
    char *get_browser_string();

    if ((file = get_browser_string(w)) != NULL)
	check_proj (w, file);
}



check_proj (w, name)
    Widget w;
    char *name;
{
    char  *mapset ;
    int proj;
    char buff[1024];


	/* ask user if map is in same units as mapset */
	if (NULL == (mapset = G_find_file2 ("dig", name, "")))
	{
	    mapset = G_mapset(); 
	    
	    proj = G_projection ();
	    if (proj == 0)
	    {
                sprintf (buff, "Mapset units are undefined. Continue? ");
		if (mouse_yes_no(buff))
		    change_map(w, name); 
            }
	    else
	    {
		sprintf ( buff, "Current mapset is %s.Is this map in %s %s? ",
			     G_database_projection_name (), 
			     G_database_projection_name (), 
			     G_database_unit_name (1));
		if (mouse_yes_no(buff))
		    change_map(w, name); 
		else
		    make_monolog(1, 
		    "Sorry, GRASS does not currently support mixing map units");
	    }
	}
	else 
	    change_map (w, name);

}

int
change_map (w, name)
    Widget w;
    char *name;
{
    int ret;
    char  *mapset ;
    char path[256];
    FILE *digit, *fopen(), *attr, *plus;
    char buf[1024];
    int have_attr;
    int n;

	
	mapset = G_mapset(); 
	G__file_name( path, "", "", mapset) ;
	G__make_mapset_element ("dig");
	G__make_mapset_element ("dig_plus");
	G__make_mapset_element ("dig_att");
	G__make_mapset_element ("reg");
      

    Data_Loaded = 0;
    Files_Open = 0;

    CM = &Map1;

    /* store the original file names */
    {
	N_name = name;
	N_path = path;

	sprintf (buf, "%s/%s/%s", N_path, "dig", N_name);
	N_dig_file= G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "dig_plus", N_name);
	N_plus_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "dig_att", N_name);
	N_att_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "reg", N_name);
	N_coor_file = G_store (buf);

    }

    Vect_init ();	/* TODO 4.0 */

    have_old = have_plus = have_attr = 0;
    if ( (digit = fopen(N_dig_file, "r+")) != NULL )
    {
	V2__setup_for_digit (CM, N_name);
	CM->dig_fp = digit;
	have_old = 1;
/* 4.0 **************************************************************/
/* Check to see if We have a 3.0 file and if so, update to 4.0 */
	{
	    struct dig_head thead;

	    /* TODO */
	    /* simulate dig_init on digit file */

	    Vect__read_head_binary (CM, &thead);

	    if (thead.Version_Major < 4)
	    {
		char buf[200];

		/* call  etc/v.from.3 to update file */

		fclose (digit);

		sprintf (buf, "%s/etc/v.from.3 %s", G_gisbase(), N_name);
		ret = system (buf);
		if (ret & 0xff00)
		{
		    make_monolog
			(1, "File conversion failed. Possibly Disk Full.\n");
		    return (-1);
		}

		/* and get back to where we were */
		digit = fopen(N_dig_file, "r+");
	    }
	}
/********************************************************************/
    }
    else
    {
	if ( (digit = fopen(N_dig_file, "w+") ) == NULL )
	{
	    sprintf (buf, "Not able to open <%s>\n", N_dig_file);
	    make_monolog(1, buf);
	    return (-1);
	}
	V2__setup_for_digit (CM, N_name);

	have_old = 0;
    }

    CM->dig_fp = digit;		/* 4.0 */

    if ((plus = fopen (N_plus_file, "r+")) != NULL)
    {
	fclose (plus);
	have_plus = 1;
    }
    else
    {
	if ( (plus = fopen(N_plus_file, "w+") ) == NULL )
	{
	    if (have_old)
	        sprintf (buf,
		    "No dig_plus file exists. You must run v.support.\n");
	    else
		sprintf (buf, "Not able to open <%s>\n", N_plus_file);
	    make_monolog (1, buf);
    	    fclose (digit); 
	    return (-1);
	}
	fclose (plus);
	unlink (N_plus_file);
	have_plus = 0;
    }


    if ((attr = fopen (N_att_file, "r+")) != NULL)
	have_attr = 1;
    else
    {
	if ( (attr = fopen(N_att_file, "w+") ) == NULL )
	{
	    sprintf (buf, "Not able to open <%s>\n", N_att_file);
	    make_monolog(1, buf);
    	    fclose (digit); 
	    return -1;
	}
	have_attr = 0;
    }

    /* do the work of dig_init()  for 4.0 */
    {
	if (have_old)
	    Vect__read_head_binary (CM, &(CM->head));
	else
	{	/* 5/14/92  dpg
		**  lines were disapearing after digitizing on 
		**  brand new file.
		**  problem was that no portable_in_head info had been
		**  placed in CM->head struct 
		*/
	    Vect__copy_portable_info (Vect__get_default_out_head(), &(CM->head));
	}
    }

    if (have_old)
    {
	ret = dig_do_file_checks (CM, N_plus_file, N_dig_file, N_att_file);
	if (ret < 0)
	{
	/*HERE*/
	    make_monolog(1, 
	    "Could not open dig_plus file. You must first run v.support.\n");
       	    fclose (digit); 
       	    fclose (attr); 
	    return(-1);
	}
    }
    initialize();
    init_init (digit, attr, N_plus_file, N_coor_file);
    CM->digit_file = N_dig_file;
    CM->att_file = N_att_file;
    Files_Open = 1;
   
#ifdef LATLON
get_conv_info(N_coor_file);
#endif 

    put_head_info (have_old); 
    showintro();
    return 0;
}

init_head (w)
    Widget w;
{
    int q=0;
    get_head_info();

    no_new_map();
    Vect__write_head_binary(CM, &(CM->head));
    /* if we have created a new dig file, then create new dig_plus file */
    if (!have_old)
	if ( 0 > write_out (0))
	{
	    fprintf (stderr, "\nError creating 'dig_plus' file!\n");
	    close_down (0);
	}

    /* if we have a digit file, but no plus file, complain and exit */
    if (have_old && !have_plus)
    {
	BEEP;
	fprintf (stderr, "\n");
	fprintf(stderr, "No 'dig_plus' file exists. Run import.vect first\n\n");
	close_down (0);

	Extended_Edit = 0;
    }
    
    Vect__write_head_binary(CM, &(CM->head));

    if (have_old)
    {
	Vect__read_head_binary(CM, &(CM->head));
	if (!Extended_Edit)
	{
	    fprintf (stderr, "\nNo Dig_Plus file.\n"), 
	    close_down (-1);
	}
	else
	{
	    dig_load_plus (CM, CM->dig_fp, 0);
	}
    }
    else
    {
	if (Extended_Edit)
	    init_plus (CM);	/* initialize arrays */
    }
    set_thresh();
    setup_win(); 

    if (!Dig_Enabled)
    {
	while (CM->head.orig_scale == 0)
	{
	    BEEP;
	    ask_value ("Original Scale is not set! Enter scale: ",
					&(CM->head.orig_scale));
	}
	replot (CM);

    }

    else /* Dig_Enabled */
    {

	if ( reset_map(w, CM, N_coor_file) <0)
	    close_down() ;
	else  
	{
	   set_window();
	   clear_window();
	   replot (CM);
	}
    }
    
   show_snapth();
   show_digth();
   init_pull();

    Data_Loaded = 1;
}

setup_win ()
{
    static int first = 1;
    if (first)
    {
        window_rout (CM->head.N, CM->head.S, CM->head.E, CM->head.W) ;
	first = 0;
    }
    else
        window_rout (U_north, U_south, U_east, U_west);
    standard_color( dcolors[CLR_ERASE]);
    erase_window();
    outline_window();
}
