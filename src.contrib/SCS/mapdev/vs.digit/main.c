/*  @(#)main.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
**  Re-Written by Ron Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include <signal.h>
#include "gis.h"

#define MAIN
#include "dig_head.h"
#include "digit.h"

/*
#define DEBUG
*/

#ifdef DEBUG
#include <sys/types.h>
#include <time.h>
#endif

/*
**  calling sequence
**  digit file_name path_to_mapset parent_pid digitizer_tty
*/
main(argc, argv)
    int argc;
    char **argv;
{
    FILE *digit, *fopen(), *attr, *plus;
    char buf[1024], tmpname[20];
    char *sav_name, *ptr;
    int have_old;
    int have_plus;
    int have_attr;
    int    close_down();
    int ret;
    unsigned	short	getuid() ;
    unsigned	short	getgid() ;
    char *memptr;	/* for load_file */
    struct Categories cats;

#ifdef HIGHPRIORITY
#ifndef MASSCOMP
    if (geteuid () != 0 && getuid () != 0)
    {
	/* if running w/ no digitizer this is not important */
	if (strcmp (argv[4], "nodig"))
	{
	printf ( "Warning!  Digit not running as root.\n");
	printf ( "Please consult GRASS installation manual on how to set up DIGIT\n");
	if (!G_yes ("Do you want to continue any way?", 1))
	    exit (0);
	}
    }
#endif
    init_priority ();	/* set up permissions and stuff for higher nice value */
#endif HIGHPRIORITY

#ifdef DEBUG
    init_debug (argv[2]);
#endif

    Data_Loaded = 0;
    Files_Open = 0;
    
    signal(SIGFPE, close_down) ;    /*  in case of floating point error  */

    /* Couldn't call G_gisinit () because of UID stuff */
    G_no_gisinit ("VS.DIGIT");

    putenv ("DPG_LOCK", "LOCKED");

/*
    if (argc < 6)
    {
	sprintf(buf, "Usage: %s  path/mapset  vect_name  PPID [ digitizer_tty | 'nodig' ]\n", argv[0]);
	G_fatal_error (buf);
    }
*/

    CM = &Map1;

    /* store the original file names */
    {
	sav_name = argv[2];
	sprintf(tmpname,"%d.0",getpid());
	N_name = tmpname;
	N_path = argv[1];
	N_PPID = argv[3];

     /* copy the original to temp */
	if (G_find_vector (sav_name, "") != NULL)
	  {
	  sprintf(buf, "g.copy vect=%s,%s > /dev/null",sav_name,N_name);
  	  system(buf); 
	  }
	else
	  N_name = argv[2];

     /* get backdrop raster name */
	N_backdrop = argv[5];
	if ((strncmp(N_backdrop, "None", 4) == 0) ||
	    (strncmp(N_backdrop, "none", 4) == 0))
	   {
           N_backdrop_mapset = NULL;
           Disp_backdrop = 0;
	   }
        else
	   {
           N_backdrop_mapset = G_find_cell (N_backdrop, "");
           if (N_backdrop_mapset == NULL)
	      {
	      fprintf(stderr, "raster map <%s> not found\n",N_backdrop);
	      exit(0);
	      }
           Disp_backdrop = 1;
	   }

     /* get overlay vect file name */
        N_overlay = argv[6];
	if ((strncmp(N_overlay, "None", 4) == 0) ||
	    (strncmp(N_overlay, "none", 4) == 0))
	   {
           N_overlay_mapset = NULL;
           Disp_overlay = 0;
	   }
        else
	   {
           N_overlay_mapset = G_find_vector (N_overlay, "");
           if (N_overlay_mapset == NULL)
	      {
	      fprintf(stderr, "overlay vector map <%s> not found\n",N_overlay);
	      exit(0);
	      }
           G__file_name (buf, "dig", N_overlay, N_overlay_mapset) ;
           Overlay.digit_file = G_store (buf);
           Overlay.name = N_overlay;
           Overlay.mapset = N_overlay_mapset;
           Disp_overlay = 1;
	   }

     /* get region name */
        N_region = argv[7];
	if ((strncmp(N_region, "None", 4) == 0) ||
	    (strncmp(N_region, "none", 4) == 0))
	   {
           N_region_mapset = NULL;
           got_region = 0;
	   }
        else
	   {
           N_region_mapset = G_find_file ("windows", N_region, "");
           if (N_region_mapset == NULL)
	      {
	       fprintf(stderr, "window region file <%s> not found\n",N_region);
	       exit(0);
	       }
	   sprintf (buf, "%s/%s/%s", N_path, "windows", N_region);
	   N_region_file= G_store (buf);
	   got_region = 1;
	   }

	sprintf (buf, "%s/%s/%s", N_path, "dig", N_name);
	N_dig_file= G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "dig_plus", N_name);
	N_plus_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "dig_att", N_name);
	N_att_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "dig_cats", N_name);
	N_cats_file = G_store (buf);

	sprintf (buf, "%s/%s/%s", N_path, "reg", N_name);
	N_coor_file = G_store (buf);

	N_digitizer = argv[4];
    }

    Vect_init ();	/* TODO 4.0 */

    have_old = have_plus = have_attr = 0;
    if ( (digit = fopen(N_dig_file, "r+")) != NULL )
    {
	V2__setup_for_digit (CM, N_name);
	CM->dig_fp = digit;
	have_old = 1;
/* 4.0 **************************************************************/
/* Check to see if we have a 3.0 file and if so, update to 4.0 */
	{
	    struct dig_head thead;

	    /* TODO */
	    /* simulate dig_init on digit file */

	    Vect__read_head_binary (CM, &thead);

	    if (thead.Version_Major < 4)
	    {
		char buf[200];

    /*DEBUG*/ fprintf (stderr, "Converting %s from 3.0 to 4.0\n", N_name);
		/* call  etc/v.from.3 to update file */

		fclose (digit);

		sprintf (buf, "%s/etc/v.from.3 %s", G_gisbase(), N_name);
		ret = system (buf);
		if (ret & 0xff00)
		    G_fatal_error ("File conversion failed. Possibly Disk Full.\n");

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
	    G_fatal_error (buf);
	}
	V2__setup_for_digit (CM, N_name);

	printf ( "\nCreating a new vector file\n");
        sleep(2);
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
	    {
		G_fatal_error ("No dig_plus file exists. You must run v.support.\n");
	    }
	    sprintf (buf, "Not able to open <%s>\n", N_plus_file);
	    G_fatal_error  (buf);
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
	    G_fatal_error (buf);
	}
	have_attr = 0;
    }

	/* read category file , see if it exists*/
    G_suppress_warnings (1);
    ret = G__read_cats ("dig_cats", N_name, G_mapset(), &cats, 1);
    G_suppress_warnings (0); 
    if (ret < 0)
       {          /* cats file does NOT exist, make one */
       G__make_mapset_element("dig_cats") ;
       G_init_cats ((CELL)0, N_name, &cats);
       G_set_cat ((CELL)0, "no data", &cats); 
       G__write_cats ("dig_cats", N_name, &cats);
       }

    /* do the work of dig_init()  for 4.0 */
    {
	if (have_old)
	    Vect__read_head_binary (CM, &(CM->head));
    }

    if (have_old)
    {
	ret = dig_do_file_checks (CM, N_plus_file, N_dig_file, N_att_file);
	if (ret < 0)
	{
	    fprintf (stderr, "Could not open dig_plus file\n");
	    fprintf (stderr, "You must first run v.support.\n");
	    sleep (4);
	    exit (-1);
	}
    }

    get_head_info(have_old, &(CM->head));

    Vect__write_head_binary(CM, &(CM->head));

    initialize (digit, attr, N_plus_file, N_coor_file);

    CM->digit_file = N_dig_file;
    CM->att_file = N_att_file;
    Files_Open = 1;

    /* if we have created a new dig file, then create new dig_plus file */
    if (!have_old)
	if ( 0 > write_out (0))
	{
	    fprintf (stderr, "\nError creating 'dig_plus' file!\n");
	    close_down (0);
	}

    Dig_Enabled = 0;		/* Disable Digitizer */
    Window_Device = MOUSE; 	/* Setup Mouse as windowing device */
    Digtiz_Device = MOUSE;


    /* if we have a digit file, but no plus file, complain and exit */
    if (have_old && !have_plus)
    {
	BEEP;
	fprintf (stderr, "\n");
	fprintf(stderr, "No 'dig_plus' file exists. Run import.vect first\n\n");
	close_down (0);

	Extended_Edit = 0;
    }

/*  if (CM->head.orig_scale == 0)
    {
	BEEP;
	fprintf (stderr, "\r\n Original Scale is not set!\n");
	sleep (3);
	close_down(0);
     }
*/
    /* set_thresh() is called after load_plus () */
    

/*DEBUG*/ debugf ("entering write_head_binary ()\n");
    Vect__write_head_binary(CM, &(CM->head));

    if (do_graphics()) 
    {
	init_window();
	R_standard_color( dcolors[CLR_ERASE]);
	erase_window();
	outline_window();
    }

/*  if (! leave()) 
        goto clean_up;  */

    if (have_old)
    {
	Vect__read_head_binary(CM, &(CM->head));
	if (!Extended_Edit)
	{
	    fprintf (stderr, "\nFIRST_PLOT REACHED.  EXITTING\n"), close_down (-1);
	    /*first_plot(digit);*/
	}
	else
	{
	    dig_load_plus (CM, digit, 0);
	    replot (CM);
	}
    }
    else
    {
	if (Extended_Edit)
	    init_plus (CM);	/* initialize arrays */

        if (Disp_backdrop) display_backdrop ();
        if (Disp_overlay) display_overlay ();
    }
    set_thresh();
    Data_Loaded = 1;
    
	/* set the screen save options for the menus
	    want these to only save once */
    Dchoose(MEN.name) ;
    _main = _digit = _edit = _label = _tool = _custom = _wind = _zoom = 1;
    cats_changed = wind_savd = 0;
    interact();

    ret = 0;	/* normal termination code */
    if (Changes_Made)
       {
       while (ret == 0)
        {
	sprintf (buf,
	     "  Changes Made\nDo you want to\nsave this session? ");
	if (mouse_yes_no (buf))
	    {
	    if ( 0 > write_out (1))
	       {
	       ret = -1;
	       Changes_Made = 0;
	       continue;
	       }
	    ret = 1;
	    }
	else
	    {
	    sprintf (buf,
	    "   Changes will be LOST !!  \nDo you want to\nsave the session? ");
	    if (mouse_yes_no (buf))
	       {
	       if (0 > write_out (1))
		    {
		    ret = -1;
		    Changes_Made = 0;
		    continue;
		    }
	       ret = 1;
	       }
            else
	       {
	       Changes_Made = 0;
	       ret = -1;
	       }
	    }
        }
       }  /* end of if Changes_Made */

clean_up:
    last_words (CM, ret);
    if (do_graphics())
	{
        R_open_driver();
	Dnew("full screen", 0., 100., 0., 100.);
	Dchoose("full screen");
	Dclearscreen() ;
	R_close_driver();
	sprintf(buf,"d.frame -e");
	system(buf);
        }

    if (file_savd == 1)
	{
        if (strcmp(N_name,sav_name) != 0)
           {              /* copy the temp to original, if required */
	   sprintf(buf, "g.copy vect=%s,%s > /dev/null",
				          N_name,sav_name);
  	   system(buf); 
		       /* remove the temp digit file */
	   sprintf(buf, "g.remove vect=%s > /dev/null",N_name);
           system(buf); 
	   ret = 0;
	   }
        }
    else
	{              /* remove the temp digit file */
	sprintf(buf, "g.remove vect=%s > /dev/null",N_name);
        system(buf); 
	}

    exit (ret);  /*redundant */
}

last_words (map, ret)
    struct Map_info *map;
{
    register int att;
    int ans=0;
    char buf[256];
    P_ATT *Att;

    fclose (map->dig_fp);
    if (Extended_Edit)
    {
	fclose (map->att_fp);
	if (Changes_Made)
	{
	    sprintf(buf, "Do you want to compress\n the Atts file? ");
	    if (mouse_yes_no (buf))
	        {
                sprintf (buf, "Updating Att file...        ");
                message[0] = (char *) malloc (strlen (buf) + 1);
                sprintf(message[0],"%s", buf);
                message[1] = " ";
                message[2] = '\0';

                Dchoose(MEN.name) ;
                popup_messg( "info1", 1) ;
		/* update the dig_att file to agree with the digit atts */
		if (0 > unlink (map->att_file))
		{
                    sprintf (buf,"Error trying to unlink att file");
                    message[0] = (char *) malloc (strlen (buf) + 1);
                    sprintf(message[0],"%s", buf);
                    message[1] = " ";
                    message[2] = '\0';

                    Dchoose(MEN.name) ;
                    popup_messg( "warn1", 1) ;
	            sleep(3);
                    erase_popup("warn1");
		}
		if (NULL == (map->att_fp = fopen (map->att_file, "w")))
		{
                    sprintf (buf,"Error openning att file   ");
                    message[0] = (char *) malloc (strlen (buf) + 1);
                    sprintf(message[0],"%s", buf);
		    sprintf (buf,"  Attributes may be lost    ");
                    message[1] = (char *) malloc (strlen (buf) + 1);
                    sprintf(message[1],"%s", buf);
                    message[2] = " ";
                    message[3] = '\0';

                    Dchoose(MEN.name) ;
                    popup_messg( "warn2", 1) ;
	            sleep(3);
                    erase_popup("warn2");
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
                    sprintf (buf, "Updating Att file... DONE   ");
                    message[0] = (char *) malloc (strlen (buf) + 1);
                    sprintf(message[0],"%s", buf);
                    message[1] = " ";
                    message[2] = '\0';

                    Dchoose(MEN.name) ;
                    erase_popup("info1");
                    popup_messg( "info2", 1) ;
                    sleep(2);
		    erase_popup("info2");
		    }
	        }
	    if (cats_changed)
	       {
	       sprintf (buf, "Do you want to update\nthe 'Cats' file? ");
	       if (mouse_yes_no (buf))
	         {
                 sprintf(buf,"cp %s/SUBJ/%s %s",
		     N_path,N_subj_file, N_cats_file);
                 system(buf);
	         }
	       } 

            if (subj_changed)
	       {
	       sprintf (buf, "Do you want to update\na SUBJ file? ");
	       if (mouse_yes_no (buf))
	          {
	          while(1)
	             {
		     ans = -1;
		     while(ans == -1)
		       {
                       sprintf (buf," Enter the SUBJECT matter ");
                       message[0] = (char *) malloc (strlen (buf) + 1);
                       sprintf(message[0],"%s", buf);
                       sprintf (buf," Enter 'list' for available Subject files    ");
                       message[1] = (char *) malloc (strlen (buf) + 1);
                       sprintf(message[1],"%s", buf);
                       sprintf (buf," <CR> to Abort/Quit): ");
                       message[2] = (char *) malloc (strlen (buf) + 1);
                       sprintf(message[2],"%s", buf);
                       message[3] = " ";
                       message[4] = " ";
                       message[5] = '\0';
                       Dchoose(MEN.name) ;
                       popup_messg( "info1", 1) ;
                       popup_ques(20,&buf[0]);
		       if (strlen(buf) == 0) 
		          {
			  erase_popup("info1");
			  ans = 0;
			  }

		       else if (strcmp(buf,"list") == 0) 
		          {
		          erase_popup("info1");
                          sprintf (buf, "See terminal for information   ");
                          message[0] = (char *) malloc (strlen (buf) + 1);
                          sprintf(message[0],"%s", buf);
                          message[1] = '\0';

                          Dchoose(MEN.name) ;
                          popup_messg( "info2", 1) ;
			  G_clear_screen ();
			  list("SUBJ");
		          erase_popup("info2");
			  G_clear_screen ();
			  }
		       else
			  ans = 1;
		       }
		     if (ans == 0) break;
		       N_subj_file = G_store(buf);
	             }   /* end of while */
                  sprintf(buf,"cp %s %s/SUBJ/%s",
		     N_cats_file, N_path,N_subj_file);
                  system(buf);
	          }
	       }  /* end of if subj_changed */
	} /* end of if changes_made */
    }   /* end of extended edit */

#ifdef DEBUG
    close_debug();
#endif

    do_file_checks (map);
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
	    fprintf(stderr, "Program exitting abnormally! Attempting to save data ");
		    if ( 0 > write_out (1))
		    {
			BEEP;
			fprintf(stderr, "Write FAILED!!!");
			sleep (4);
			ret = -1 ;
		    }
		    else
		    {
			Changes_Made = 0;
			fprintf(stderr, "File updated...");
			sleep (2);
			ret = -5 ;
		    }

	}
	    
	ret = -5 ;
end:

    last_words (CM, ret);
    exit (ret); /* redundant */
}


#ifdef DEBUG

static FILE *debugfp;
static int debug_on;
init_debug (file)
     char *file;
{
    char *getenv ();

    debug_on = 0;
    if (!getenv ("DEBUG"))
	return ;
    if (strcmp ("wy50", getenv ("DEBUG")) == 0)
    {
	/* set 43 lines/page */
	fprintf (stderr, "%ce+", 27);
	/* split  24/19 */
	fprintf (stderr, "%cx18", 27);
	sleep (1);
	flush_keyboard ();
	debug_on = 1;
    }
    else
    {
	if ((debugfp = fopen (getenv ("DEBUG"), "a")) == NULL)
	{
	    if ((debugfp = fopen (getenv ("DEBUG"), "w")) == NULL)
	    {
		fprintf (stderr, "NO DEBUG\n");
		debug_on = 0;
	    }
	    else
		debug_on = 2;
	}
	else
	    debug_on = 2;
    }
    if (debug_on)
    {
	long timer;

	setbuf (debugfp, NULL);
	time (&timer);
	debugf ("\n\nSTARTUP: %s", asctime (localtime(&timer)));
	debugf ("USER: %s  File: %s\n", G_whoami(), file);
    }
}

close_debug()
{
    switch (debug_on) {
	case 1:
#ifdef FOO
	    /* set 24 rows */
	    fprintf (stderr, "%ce(", /*)*/  27);
	    /* set to full screen */
	    fprintf (stderr, "%cx0", 27);
	    flush_keyboard ();
#endif
	    break;
	case 2:
	    fclose (debugfp);
	    break;
	default:
	    break;
    }
    debug_on = 0;
}
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
      
    char buf[1024], *p;

    if (!debug_on)
	return 0;

    switch (debug_on) {
	case 1:
	    /* { */
	    fprintf (stderr, "%c}", 27);

	    sprintf (buf, format, a, b, c, d, e, f, g, h, i, j, k, l);
	    for (p = buf ; *p ; p++)
	    {
		fputc (*p, stderr);
		if (*p == '\n')
		    fputc ('\r', stderr);
	    }

	    fprintf (stderr, "%c]", 27);
	    break;
	case 2:
	    fprintf (debugfp, format, a, b, c, d, e, f, g, h, i, j, k, l);
	    break;
	default:
	    break;
    }
}
#endif
