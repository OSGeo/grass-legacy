#include "digit.h"

void 
Quit(w, client_data, call_data)
    Widget   w;
    caddr_t  client_data;
    caddr_t  call_data;
{
    XmString  message, save, done, cancel, title;
    Widget dlog;
    int n, q=0;
    Arg wargs[10];

    save = XmStringCreateSimple ("save & quit");
    title = XmStringCreateSimple ("Quit");
    if (Changes_Made)
    {
   
        message = XmStringCreateSimple ("Quitting digit?");
        /*done = XmStringCreateSimple ("quit without saving"); */
        done = XmStringCreateSimple (" ?   "); 
        cancel = XmStringCreateSimple ("cancel");
    }
    else
    {
        message = XmStringCreateSimple 
			("You really want to quit digit?");
        done = XmStringCreateSimple ("Yes");
        cancel = XmStringCreateSimple ("No");
    }

        
    n = 0;
    dlog = XmCreateMessageDialog (toplevel, "not_saved", NULL, 0);

    XtVaSetValues (dlog,
	XmNautoUnmanage, True,
	XmNmessageString, message,
	XmNokLabelString, save,
	XmNcancelLabelString, cancel,
	XmNhelpLabelString, done,
	XmNdialogTitle, title, 
	NULL);
    XtVaSetValues (XtParent(dlog), 
		XmNsaveUnder, True,
		NULL);

    XtAddCallback (dlog, XmNhelpCallback, Really_quit, 0); 
    XtAddCallback (dlog, XmNokCallback, savefile, NULL); 
    XtAddCallback (dlog, XmNokCallback, Really_quit, 1); 
    XtAddCallback (dlog, XmNcancelCallback, XtDestroyWidget, NULL);

    
    if (!Changes_Made)
	 XtUnmanageChild (XmMessageBoxGetChild(dlog, XmDIALOG_OK_BUTTON));
    else
	 XtUnmanageChild (XmMessageBoxGetChild(dlog, XmDIALOG_HELP_BUTTON));
    XmStringFree (message);
    XmStringFree (save);
    XmStringFree (done);
    XmStringFree (title);
    XmStringFree (cancel);

    showcb (w, dlog, NULL);
  
}
void 
Really_quit(w, save, call_data)
    Widget   w;
    int      save;
    caddr_t  call_data;
{
    int ret;
    if (N_lockname != NULL)
	unlock_file (N_lockname);


    ret = 0;    /* normal termination code */
    if (save && Changes_Made)
    {
            if ( 0 > write_out (1))
            {
                ret = -1;
            }
    }
    if (CM != NULL)
    {
	if (save)
            last_words (CM, ret);
	else
	{
    	    fclose (CM->dig_fp);
    	    if (Extended_Edit)
	        fclose (CM->att_fp);
	}
    }
    XtCloseDisplay (XtDisplay(w));
#ifdef DEBUG
    close_debug();
#endif
    exit(0);
}

last_words (map, ret)
    struct Map_info *map;
{
    register int att;
    P_ATT *Att;
    
    {
    fclose (map->dig_fp);
    if (Extended_Edit)
    {
	fclose (map->att_fp);
	if (Changes_Made)
	{
	    if(mouse_yes_no ( "Do you want to compress the Atts file? "))
	    {
		write_info (1, "Updating Att file...");
		/* update the dig_att file to agree with the digit atts */
		if (0 > unlink (map->att_file))
		{
		    make_monolog (1, "Error trying to unlink att file");
		}
		if (NULL == (map->att_fp = fopen (map->att_file, "w")))
		{
		    make_monolog (1, 
		    "Error openning att file. Attributes may be lost");
		}
		else
		{
		    for (att = 1 ; att <= map->n_atts ; att++)
		    {
			Att = &(map->Att[att]);
			if (ATT_ALIVE (Att))
			{
			    write_att (map->att_fp, (char) dig_new_to_old_type (Att->type), Att->x, Att->y, Att->cat);
			}
		    }
		    fclose (map->att_fp);
		    write_info (1, "Updating Att file... DONE.");
		}
	    }
	}
    }

    do_file_checks (map);
    }

    exit (ret);
}

do_file_checks (map)
    struct Map_info *map;
{
    FILE *fp;
    struct Plus_head Plus;

    if ((fp = fopen (map->plus_file, "r+")) == NULL)
    {
	G_fatal_error ("Can't open Plus file for final write\n");
    }
    dig_Rd_Plus_head (map, &Plus, fp);
    rewind (fp);
    dig_write_file_checks (fp, map, &Plus);
    fclose (fp);
}

/* exit status code needs to be finished */
/* EXIT (-5)  abnormal, but data should be ok */
/* EXIT (-1)  abnormal, Data should not be trusted */
close_down()
{
	int ret;

	if (Data_Loaded)
	{
	    if(mouse_yes_no( "Program exiting abnormally! Attempt to save data?"))
	    {
		    if ( 0 > write_out (1))
		    {
			BEEP;
			write_info (1, "Write FAILED!!!");
			sleep (4);
			ret = -1 ;
		    }
		    else
		    {
			Changes_Made = 0;
			write_info (1, "File updated...");
			ret = -5 ;
		    }
	    }

	}
	    
	ret = -5 ;
end:

    last_words (CM, ret);
    exit (ret); /* redundant */
}


