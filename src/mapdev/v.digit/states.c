/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "local_proto.h"

	static int Label_Device_Sav; 
	static int Window_Device_Sav;
	static int Digtiz_Device_Sav;
	static int Point_Device_Sav; 

int init_states (void)
{
    Dig_Enabled = 1;	  /* Using Digitizer? */
    Extended_Edit = 1;	  /* Have Dig.plus?   */
    Beep_On = 1;		  /* Toggle beep      */
    Terse_On = 0;		  /* Terse Sub-Menus */
    Compress_File = 0;	  /* Remove deleted lines when writing files*/
    Label_Device = MOUSE; /* Labelling device (Mouse,Digitizer) */
    Window_Device = DIGITIZER; /* Windowing device (Mouse,Digitizer) */
    Digtiz_Device = DIGITIZER; /* Digitizing device (Mouse,Digitizer) */
    Point_Device = MOUSE; /* Pointing device (Mouse,Digitizer) */
    Changes_Made = 0;	  /* Changes made, Exit will warn or Not write*/
    Auto_Window = 0;	/* rewindow if chosen area extends outside */
    Remove_Draw = 1;	/* Display Block removes vs. redrawing screen */

    return 0;
}

#ifndef  SCS_MODS

int 
set_default_display (void)
{
    Disp_overlay = 0;	/* Redraw Border Map after ReWindowinng */
    Disp_backdrop = 0;	/* Redraw Cell backdrop after ReWindowinng */
    Disp_flex = 0; /* OHLER - calculate grey.eq  for backdrop */
    Auto_Smooth = 0; /* OHLER smooth line after digitizing */
    Disp_lines =   1;	/* all lines */
    Disp_points  = 0;	/* all points in line */
    Disp_nodes =   1;	/* arc endpoints */
    Disp_labels =  0;	/* area labels */
    Disp_outline = 0;	/* area border lines, for labeled areas */
    Disp_markers = 1;	/* area ID markers (dot) */
    Disp_llines =  1;	/* labelled lines */
    Disp_llabels = 0;	/* line labels (category #s)*/
    Disp_ulines =  0;	/* unlabelled lines */
    Disp_thresh  = 0;	/* all snapping thresholds */
    Disp_sites =   1;   /* Sites */
    Disp_slabels = 0;   /* site labels*/
    return 0;
}

#else	/* SCS_MODS */

int 
set_default_display (void)
{
                /*   check the memory file */
      if (!get_mem(0))
	   {
           Disp_overlay = 0; /* Redraw Border Map after ReWindowinng */
           Disp_backdrop = 0; /* Redraw Cell backdrop after ReWindowinng */
           Disp_lines =   1; /* all lines */
           Disp_points  = 0; /* all points in line */
           Disp_nodes =   1; /* arc endpoints */
           Disp_labels =  0; /* area labels */
           Disp_outline = 0; /* area border lines, for labeled areas */
           Disp_markers = 1; /* area ID markers (dot) */
           Disp_llines =  1; /* labelled lines */
           Disp_llabels = 0; /* line labels (category #s)*/
           Disp_ulines =  0; /* unlabelled lines */
           Disp_thresh  = 0; /* all snapping thresholds */
           Disp_sites =   1; /* Sites */
           Disp_slabels = 0; /* site labels*/


           CLR_LINE = 4; /*line color */ 
	   CLR_AREA = 8; /*area color */
           CLR_SITE = 6; /*site color */
           CLR_LSITE = 10; /*labeled site color */
           CLR_LLINE = 9; /*labeled line color */
           CLR_LAREA = 7; /*labeled area color */
           CLR_AMARK = 7; /*marking color */
           CLR_ALABEL = 7; /*area labels color */
           CLR_LLABEL = 9; /*lines/sites labels color */
           CLR_HIGHLIGHT = 3; /*highlight color */
           CLR_ERASE = 2; /*erasing color */
           CLR_UNKNOWN = 1; /*unknown color */
           CLR_OVERLAY = 1; /*overlay color */
           CLR_0_NODE = 6; /*0 lines on node color */
           CLR_1_NODE = 6; /*1 line on node color */
           CLR_2_NODE = 5; /*2+lines on node color */

	    Beep_On = 1;                  /* Toggle beep      */
	    Terse_On = 0;                 /* Terse Sub-Menus */
	    Auto_Window = 0;    /* rewindow if chosen area extends outside */

	    Label_Device_Sav  =  Label_Device  = MOUSE;
	    Window_Device_Sav =  Window_Device = DIGITIZER;
	    Digtiz_Device_Sav  =  Digtiz_Device = DIGITIZER;
	    Point_Device_Sav  =  Point_Device  = MOUSE;

           last_prune_thresh = 0.03;
           last_snap_thresh = 0.03;
	   }
    return 0;
}

int get_mem (int r_w)
{
         int D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14;
         int C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16;
         int E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11;
         double prune, snap;
  	 char in_file[200], buff[40];
	 FILE *Mem_file;

         sprintf(in_file,"%s/%s/.tmp/.dig_cfg", G_location_path(), G_mapset());
	 if (r_w == 0)
	    {
	    if ( (Mem_file = fopen(in_file, "r")) == NULL) return(0);
            else
	       {   /* memory file exits, read it, set options */
	       rewind(Mem_file);
               if (fgets(buff,sizeof(buff),Mem_file) == NULL) return(0);
               sscanf(buff,"%lf %lf",&prune,&snap);
               if (fgets(buff,sizeof(buff),Mem_file) == NULL) return(0);
               sscanf(buff,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                              &D1,&D2,&D3,&D4,&D5,&D6,&D7,
                              &D8,&D9,&D10,&D11,&D12,&D13,&D14);
               if (fgets(buff,sizeof(buff),Mem_file) == NULL) return(0);
               sscanf(buff,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                              &C1,&C2,&C3,&C4,&C5,&C6,&C7,&C8,&C9,
                              &C10,&C11,&C12,&C13,&C14,&C15,&C16);
               if (fgets(buff,sizeof(buff),Mem_file) == NULL) return(0);
               sscanf(buff,"%d %d %d %d %d %d %d",
                              &E1,&E2,&E3,&E4,&E5,&E6,&E7);

               Disp_overlay = D1; 
               Disp_backdrop = D2;
               Disp_lines =   D3;
               Disp_points  = D4;
               Disp_nodes =   D5;
               Disp_labels =  D6;
               Disp_outline = D7;
               Disp_markers = D8;
               Disp_llines =  D9;
               Disp_llabels = D10;
               Disp_ulines =  D11;
               Disp_thresh  = D12;
               Disp_sites =   D13;
               Disp_slabels = D14;

               CLR_LINE = C1;
               CLR_AREA = C2;
               CLR_SITE = C3; 
               CLR_LSITE = C4;
               CLR_LLINE = C5;
               CLR_LAREA = C6;
               CLR_AMARK = C7;
               CLR_ALABEL = C8;
               CLR_LLABEL = C9; 
               CLR_HIGHLIGHT = C10;
               CLR_ERASE = C11; 
               CLR_UNKNOWN = C12;
               CLR_OVERLAY = C13;
               CLR_0_NODE = C14;
               CLR_1_NODE = C15;
               CLR_2_NODE = C16;

		Beep_On = E1;          
		Terse_On = E2;         
		Auto_Window = E7;    
		Label_Device = E3; 
		Window_Device = E4;
		Digtiz_Device = E5;
		Point_Device = E6; 

		Label_Device_Sav  =  Label_Device;
		Window_Device_Sav =  Window_Device;
		Digtiz_Device_Sav  =  Digtiz_Device;
		Point_Device_Sav  =  Point_Device;

	     /* if no digitizer, set everything to mouse */
	     if (!Dig_Enabled)
	     {
		Label_Device  = MOUSE; 
		Window_Device = MOUSE;
		Digtiz_Device = MOUSE;
		Point_Device  = MOUSE; 
	     }

               last_prune_thresh = prune;
               last_snap_thresh = snap;
	       
               fclose(Mem_file);
	       }
            }
         if (r_w == 1)
            {
	    if ( (Mem_file = fopen(in_file, "w")) == NULL) return(0);
            else
	       {    /* save options to memory file, for next time */
               sprintf(buff,"%lf %lf\n",CMap->prune_thresh,CMap->snap_thresh);
	       fputs(buff,Mem_file);
                    /* make sure overlay and backdrop are 0 */
               Disp_overlay = 0;
	       Disp_backdrop = 0;

               sprintf(buff,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
		      Disp_overlay, Disp_backdrop, Disp_lines, Disp_points,
		      Disp_nodes, Disp_labels, Disp_outline, Disp_markers,
                      Disp_llines, Disp_llabels, Disp_ulines, Disp_thresh,
		      Disp_sites, Disp_slabels);
	       fputs(buff,Mem_file);

               sprintf(buff,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
                      CLR_LINE, CLR_AREA, CLR_SITE, CLR_LSITE, CLR_LLINE,
                      CLR_LAREA, CLR_AMARK, CLR_ALABEL, CLR_LLABEL, 
                      CLR_HIGHLIGHT, CLR_ERASE, CLR_UNKNOWN, CLR_OVERLAY,
                      CLR_0_NODE, CLR_1_NODE, CLR_2_NODE);
	       fputs(buff,Mem_file);

		if  (!Dig_Enabled)
		   sprintf(buff,"%d %d %d %d %d %d %d\n",
		     Beep_On, Terse_On, Label_Device_Sav, Window_Device_Sav, 
		     Digtiz_Device_Sav, Point_Device_Sav, Auto_Window);
		else
		   sprintf(buff,"%d %d %d %d %d %d %d\n",
		     Beep_On, Terse_On, Label_Device, Window_Device, 
		     Digtiz_Device, Point_Device, Auto_Window);

	       fputs(buff,Mem_file);
	       fclose(Mem_file);
	       }
            }
	 return(1);
}
#endif /* SCS */
