#ident " @(#)main.c	1.6   03/18/91 "
/* created by: R.L.Glenn
*
*****/
#include <ctype.h>
#include <stdio.h>
#include "geo.h"
static FILE *sys;

main(argc, argv) 
	int argc ;
        char *argv[];
{
 char *tmp_file, *G_tempfile();

 conv_typ = conv_way = 0;
 input_typ = output_typ = 1;
 G_clear_screen();
 fprintf(stderr,"\n\n Keyboard and Screen are default input/output devices     -Hit any key- \n");
 gets(answer);

/*   check the memory file */
 if (!get_mem(1))
	 if (!get_mem(0))
	    {
	    fprintf(stderr,"\n Memory File could not be found/created ");
	    sleep(2);
	    }

/*  user main menu */
 for (;;)
   { 
   G_clear_screen();
   fprintf(stderr,"                   Coordinate Conversions\n\n\n");
   fprintf(stderr,"    1- Coord->LL Conversion       2- LL->Coord Conversion\n");
   fprintf(stderr,"    3- Input/Output Selection     4- Quit\n\n");


/* for the next release
   fprintf(stderr,"    3- Coord->Coord Conversion    4- Input/Output Selection\n");
   fprintf(stderr,"    5- Quit\n\n");*/


   fprintf(stderr,"    Enter your selection [4] : ");
   gets(answer);
   if (strlen(answer) == 0) conv_typ = 4;
   else
      {
      if (*answer == '1' || *answer == '2' ||
          *answer == '3' || *answer == '4') 
          conv_typ = atoi(answer);
      else conv_typ = 0;
      }
   if (conv_typ == 1 || conv_typ == 2)
      {
      if (strlen(proj_name) == 0 || strcmp(proj_name,"none",4) == 0)
          {
	  proj_name[0] = '\0';
          if (!get_proj(proj_name)) continue;
          }
      else
          {
          if (strncmp(proj_name,"stp",3) == 0)
	      fprintf(stderr,
		   "\n Last projection used was \"%s\"\n\tfor \"%s county, %s\"; do you want to change it (y/[n]) ? ", proj_name,COname,STabbr);
          else 
	      fprintf(stderr,"\n Last projection used was \"%s\", do you want to change it (y/[n]) ? ", proj_name);

          gets(answer);
          if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n')
	     {
	     COzone = SFIPS = CFIPS = 0;
	     sprintf(COname,"x");
	     sprintf(STabbr,"x");
	     proj_name[0] = '\0';
             if (!get_proj(proj_name)) continue;
	     }
          }
      if (strncmp(proj_name,"stp",3) != 0)
	  {
          if (strcmp(ellps_name,"none",4) == 0 || strlen(ellps_name) == 0)
	     {
	     ellps_name[0] = '\0';
             if (!get_ellp(ellps_name)) continue;
	     }
          else
             {
             fprintf(stderr,"\n Last spheroid used was \"%s\", do you want to change it (y/[n]) ? ", ellps_name);
             gets(answer);
             if ((strlen(answer) != 0) && *answer != 'N' && *answer != 'n')
		{
	        ellps_name[0] = '\0';
                if (!get_ellp(ellps_name)) continue;
		}
             }
          }
      }

/*  actions   */
   switch(conv_typ)
	{
        case 1: 		        /*  coord->ll conversions */
        case 2: 			/*  ll->coord conversions */
    parms[0] = '\0';
	tmp_file = G_tempfile();
    if (strcmp(proj_name,"stp",3) == 0)
		{
		if (COzone == 0) if (get_CO_code() == 0) break;
		if (get_stp_code(COzone,parms) == 0)
			{
		    fprintf(stderr," ERROR: This should not happen, see your system admin\n");
		    sleep(2);
		    break;
			}
		}
	else
		{
        if (conv_typ == 1)
		     sprintf(parms,
		     "%s/mapgen/bin/proj -s +proj=%s +ellps=%s +inv ",
		     getenv("GISBASE"),proj_name,ellps_name);
        else
		     sprintf(parms,
		     "%s/mapgen/bin/proj -s +proj=%s +ellps=%s ",
		     getenv("GISBASE"),proj_name,ellps_name);
	    if ((strncmp(proj_name,"utm",3) != 0))
			{
            fprintf(stderr,"\n     Last Prime meridian used was %lf\n     Std. Parallel was %lf\n\t\tdo you want to change them (y/[n]) ? ", LONCEN,LATPAR);
            gets(answer);
            if ((strlen(answer) != 0) &&
			   *answer != 'N' && *answer != 'n')
	            {
				for(;;)
					{
               		G_clear_screen();
		       		if (get_PM()) break;
			   		}
	            }
		    sprintf(buff,"+lon_0=%lf +lat_0=%lf ",LONCEN,LATPAR);
		    strcat(parms,buff);
			}
/********  END UTM   ****************/
		}

        if (input_typ == 1)       /* keyboard input */
	    	{
			for(;;)
				{
                 if (conv_typ == 1) {if (!get_enz()) break;}
                 else { if (!get_ll()) break; }
	         	if (strncmp(proj_name,"utm",3) == 0)
		    		{
		    		sprintf(parms,"%s +zone=%d ",parms,ZONE);
		    		}
                sys = fopen (tmp_file,"w");
                if (conv_typ == 1) fprintf(sys,"%s +inv",parms);
                else fprintf(sys,"%s",parms);
		 		fclose (sys);
                if (conv_typ == 1)
		   		{
		   			if (strncmp(proj_name,"stp",3) == 0)
		      		sprintf(command,
		      		"%s/mapgen/bin/proj -c %s -s -f \"%%.8f\" -m 1/.30480060960121920243 << eof\012%lf %lf\012eof\n",
		      		getenv("GISBASE"),tmp_file,EAS,NOR);
                else
		      		sprintf(command,
		      		"%s/mapgen/bin/proj -c %s -s -f \"%%.8f\" << eof\012%lf %lf\012eof\n",
		      		getenv("GISBASE"),tmp_file,EAS,NOR);
		   		}
                else
		   			{
		   			if (strncmp(proj_name,"stp",3) == 0)
		      			sprintf(command,
		      			"%s/mapgen/bin/proj -c %s -s -m 1/.30480060960121920243 << eof\012%lf %lf\012eof\n",
		      			getenv("GISBASE"),tmp_file,LON,LAT);
                   else
		      			sprintf(command,
		      			"%s/mapgen/bin/proj -c %s -s << eof\012%lf %lf\012eof\n",
		      			getenv("GISBASE"),tmp_file,LON,LAT);
                   }
          		if (sys = popen(command,"r"))
	            	{
	            	fgets(buff, sizeof buff, sys);
		           /* remove the trailing newline */
		    		for (ptr = buff; *ptr; ptr++)
					if (*ptr == '\n') *ptr = 0;

                    if (conv_typ == 1) sscanf(buff,"%lf %lf",&LAT, &LON);
                    else sscanf(buff,"%lf %lf",&NOR, &EAS);
		    		DMS (); /*  convert LAT/LON to dd mm ss.ss */
		    		Write_results(0);
                    pclose (sys);
                    }
	         }  /*end for loop */
		 }
               
		if (input_typ == 2)        /* file input */
			{
			rec_cnt = 0;
            G_clear_screen();
            fprintf(stderr,"                   Coordinate Conversions\n\n");
            if (conv_typ == 1)
		   		fprintf(stderr,"                 Coord->Lat/Long Conversion\n\n");
            else
            	fprintf(stderr,"                 Lat/Long->Coord Conversion\n\n");
		 	rec_cnt = 0;
		 	for (;;)
		    	{
                if (fgets(buff,80,In_file) == NULL) break; 
		     	rec_cnt++;
                if (conv_typ == 1) sscanf(buff,"%lf%lf%d",&EAS,&NOR,&ZONE);
		     	else
					{
		        	ZONE = 0;
                    sscanf(buff,"%d%d%f%d%d%f%d",
                    &IDEG,&IMIN,&XSEC,&JDEG,&JMIN,&YSEC,&ZONE);
                    sscanf(buff,"%f%f%f%f%f%f%d",
                    &XDEG,&XMIN,&XSEC,&YDEG,&YMIN,&YSEC,&ZONE);
                  	if (XDEG < 0.0)
			   			{
			   			LON = -XDEG + (XMIN/60.) + (XSEC/3600.);
			   			LON = -LON;
			   			}
                 	else
			   			LON = XDEG + (XMIN/60.) + (XSEC/3600.);
                    if (YDEG < 0.0)
			   			{
			   			LAT = -YDEG + (YMIN/60.) + (YSEC/3600.);
			   			LAT = -LAT;
			   			}
                    else
			   			LAT = YDEG + (YMIN/60.) + (YSEC/3600.);
					}
	       		if (strncmp(proj_name,"utm",3) == 0)
		       		{
                    if (ZONE <= 0) ZONE = (186E0 + LON)/6E0;
		        	sprintf(buff,"%s +zone=%d ",parms,ZONE);
		        	}
             	sys = fopen (tmp_file,"w");
             	if (conv_typ == 1)  fprintf(sys,"%s +inv",buff);
                else fprintf(sys,"%s",buff);
		     	fclose (sys);
		     	if (conv_typ == 1)
		      		{
		      		if (strncmp(proj_name,"stp",3) == 0)
		 				sprintf(command,
						"%s/mapgen/bin/proj -c %s -s -f \"%%.8f\" -m 1/.30480060960121920243 << eof\012%lf %lf\012eof\n",
		         		getenv("GISBASE"),tmp_file,EAS,NOR);
					else
						sprintf(command,
						"%s/mapgen/bin/proj -c %s -s -f \"%%.8f\" << eof\012%lf %lf\012eof\n",
		         		getenv("GISBASE"),tmp_file,EAS,NOR);
		      		}
				else
		      		{
		      		if (strncmp(proj_name,"stp",3) == 0)
		         		sprintf(command,
		         		"%s/mapgen/bin/proj -c %s -s -m 1/.30480060960121920243 << eof\012%lf %lf\012eof\n",
		         		getenv("GISBASE"),tmp_file,LON,LAT);
					else
		  				sprintf(command,
		         		"%s/mapgen/bin/proj -c %s -s << eof\012%lf %lf\012eof\n",
		         		getenv("GISBASE"),tmp_file,LON,LAT);
					}
				if (sys = popen(command,"r"))
					{
					fgets(buff, sizeof buff, sys);
		           /* remove the trailing newline */
		        	for (ptr = buff; *ptr; ptr++)
			    	if (*ptr == '\n') *ptr = 0;
               		if (conv_typ == 1)
			   			{
              			sscanf(buff,"%lf %lf",&LAT, &LON);
			   			DMS (); /*  convert to dd mm ss.ss */
			   			}
					else sscanf(buff,"%lf %lf",&NOR, &EAS);
		        	Write_results(0);
					pclose (sys);
					}
				} /* end for loop */

			Write_results(1);
			fclose(In_file);
			input_typ = output_typ = 1;
			fprintf(stderr,"\tresetting to default screen & keyboard\n");
			sleep(2);
			}
		unlink(tmp_file);
        break;
        
/*
        case 3: 			**  coord->coord conversions **
               G_clear_screen();
fprintf(stderr,"coord->coord conversions\n");
sleep(2);
        break;

        case 4:                         **  input/output selection */
        case 3:                         /*  input/output selection */
	   		conv_way = 0;
           	while (conv_way != 3)
	      		{
				G_clear_screen();
				fprintf(stderr,"\n\n\t1- Input selection  2- Output selection 3- Main Menu\n\n");
				fprintf(stderr,"    Enter your selection [3] : ");
				gets(answer);
				if (strlen(answer) == 0)   conv_way = 3;
				else 
					if (*answer != '1' && *answer != '2' && *answer != '3' )
						conv_way = 3;
	           else conv_way = atoi(answer);

/*  actions   */
		switch(conv_way)
	        {
/* --------------------- Input Selection ------------------------ */
   			case 1: 	
                input_typ = 0;
                while (input_typ == 0)
	      			{
		   			G_clear_screen();
	           		fprintf(stderr,"\n\n\t1- Keyboard           2- File\n\n");
               		fprintf(stderr,"\tEnter your selection : ");
					gets(answer);
	           		if (strlen(answer) == 0)   input_typ = 1;
	           		else if (*answer != '1' && 
		            	*answer != '2' )  input_typ = 3;
	           		else input_typ = atoi(answer);

           /*  actions   */
                   switch(input_typ)
	             	{
                   case 1: 				/*  Keyboard */
                         break;
                   case 2: 				/*  File */
                         get_file(1);
                         break;
                   default:               /* invalid option */
		         		fprintf(stderr,"  *** INVALID option *** \n");
			 			sleep(2);
	                 	input_typ = 0;
	                 	break;
                     }
                   }
	           break;

/* --------------------- Output Selection ------------------------ */
              case 2: 			
                 output_typ = 0;
                 while (output_typ == 0)
	            	{
		    		G_clear_screen();
	            	fprintf(stderr,"\n\n\t1- Screen             2- File\n\n");
                    fprintf(stderr,"\tEnter your selection : ");
	            	gets(answer);
	            	if (strlen(answer) == 0)   output_typ = 1;
	            	else if (*answer != '1' && 
		            	 *answer != '2' )  output_typ = 3;
	            	else output_typ = atoi(answer);

            /*  actions   */
                    switch(output_typ)
	              	{
                    case 1: 				/*  Screen */
                          break;
                    case 2: 				/*  File */
                          get_file(2);
                          break;
                    default:                /* invalid option */
		          		fprintf(stderr,"  *** INVALID option *** \n");
			  			sleep(2);
	                  	output_typ = 0;
	                  	break;
                     }
                    }
	            break;

/* --------------------- Return to Main Menu ------------------------ */
              		case 3: 		
		    			break;

/* --------------------- Invalid option ---------------------------- */
              		default:                /* invalid option */
		    			fprintf(stderr,"  *** INVALID option *** \n");
		    			sleep(2);
	            		input_typ = 0;
	            		output_typ = 0;
	            		break;
                }
              }
              break;

/*  quit 
       case 5:*/
       case 4:
	      G_clear_screen();
	      if (!get_mem(2)) 
	         {
	         fprintf(stderr,"\n Memory File could not be found/created ");
	         sleep(2);
	         G_clear_screen();
	         }
              exit(0) ;

/* invalid option */
      default:
        	fprintf(stderr," *** INVALID option ***\n");
	      	sleep(2);
	      	break;
      }
    }
}

