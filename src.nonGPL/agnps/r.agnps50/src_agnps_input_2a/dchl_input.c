
        /*---------------------------------------------------------*
         *                  AGNPS/GRASS Interface                  *
         *                                                         *
         *                       Developed by                      * 
         *                                                         *
         *                         Zhian Li                        *
         *                                                         *
         *                            at                           *
         *                                                         *
         *              The Pennsylvania State University          *
         *                                                         *
         *---------------------------------------------------------*/


#include "agnps_input.h"

/*
         This function is created to allow entries of point sources 
         in format of the newest AGNPS (version 5.).

                   June, 1995

                   Zhian Li,  
                   Department of Entomology
                   501 ASI Building
                   The Pennsylvania State University 
                   University Park, Pa 16802 

         ===========================================================
*/



  dchl_input()
     {

        int	i, j, nrows, ncols, ct, land_use;
        char    yesno1[3];
        int     cell_id;


     /*      Begin process the channel data input    */

               i = 0;
               if(hydro_mdl == 0 ) {
                  if(geom_mod == 0 ) {
                     dchannel00_dat(i);
                     i               = chl_cel_id; 
                     if(i <= 0 || i > tot_cells) {
                        fprintf (stderr,"Illegal cell number %d\n",i);
                        fprintf (stderr,"Please enter a valid cell number\n");
                        fprintf (stderr,"between 1 and %i\n",tot_cells);
                        dchannel00_dat(i);
                        i = chl_cel_id; 
                        }
                     cel[i].chl_indicator = chl_type;
                     cel[i].chlw     = chlw; 
                     cel[i].chlwc    = chlwc;    
                     cel[i].chlwe    = chlwe;    
                     cel[i].chld     = chld;     
                     cel[i].chldc    = chldc;    
                     cel[i].chlde    = chlde;   
                     cel[i].chllnth  = chllnth;  
                     cel[i].chllnthc = chllnthc;         
                     cel[i].chllnthe = chllnthe; 
                     cel[i].chlm     = chlm;     
                     }
                   else
                     {
                     chlslope=cel[i].chl_slope;
                     chlsslop=cel[i].ch_side_slope;
                     dchannel01_dat(i);
                     i = chl_cel_id; 
                     if(i <= 0 || i > tot_cells) {
                        fprintf (stderr,"Illegal cell number %d\n",i);
                        fprintf (stderr,"Please enter a valid cell number\n");
                        fprintf (stderr,"between 1 and %i\n",tot_cells);
                        dchannel00_dat(i);
                        i = chl_cel_id; 
                        }
                     cel[i].chl_indicator = chl_type;
                     cel[i].chlw     = chlw; 
                     cel[i].chlwc    = chlwc;    
                     cel[i].chlwe    = chlwe;    
                     cel[i].chld     = chld;     
                     cel[i].chldc    = chldc;    
                     cel[i].chlde    = chlde;   
                     cel[i].chllnth  = chllnth;  
                     cel[i].chllnthc = chllnthc;         
                     cel[i].chllnthe = chllnthe; 
                     cel[i].chlm     = chlm;     
                     }
                  }
                else
                  { 

                  if(geom_mod == 0 ) {
                     chlslope=cel[i].chl_slope;
                     chlsslop=cel[i].ch_side_slope;
                     dchannel10_dat(i);
                     i = chl_cel_id; 
                     if(i <= 0 || i > tot_cells) {
                        fprintf (stderr,"Illegal cell number %d\n",i);
                        fprintf (stderr,"Please enter a valid cell number\n");
                        fprintf (stderr,"between 1 and %i\n",tot_cells);
                        dchannel00_dat(i);
                        i = chl_cel_id; 
                        }
                     /*----------------------------------------------------
                        these two lines of code were added by Dave Peterson
                        March 1996
                     */
                     cel[i].chl_slope     = chlslope;
                     cel[i].ch_side_slope = chlsslop;
                     /*----------------------------------------------------*/
                     cel[i].chl_indicator = chl_type;
                     cel[i].chlw          = chlw; 
                     cel[i].chlwc         = chlwc;    
                     cel[i].chlwe         = chlwe;    
                     cel[i].chld          = chld;     
                     cel[i].chldc         = chldc;    
                     cel[i].chlde         = chlde;   
                     cel[i].chllnth       = chllnth;  
                     cel[i].chllnthc      = chllnthc;         
                     cel[i].chllnthe      = chllnthe; 
                     cel[i].chlm          = chlm;     
                     }
                   else
                     {
                     chlslope=cel[i].chl_slope;
                     chlsslop=cel[i].ch_side_slope;
                     dchannel11_dat(i);
                     i               = chl_cel_id; 
                     if(i <= 0 || i > tot_cells) {
                        fprintf (stderr,"Illegal cell number %d\n",i);
                        fprintf (stderr,"Please enter a valid cell number\n");
                        fprintf (stderr,"between 1 and %i\n",tot_cells);
                        dchannel00_dat(i);
                        i = chl_cel_id; 
                        }
                     cel[i].chl_indicator = chl_type;
                     cel[i].chlw     = chlw; 
                     cel[i].chlwc    = chlwc;    
                     cel[i].chlwe    = chlwe;    
                     cel[i].chld     = chld;     
                     cel[i].chldc    = chldc;    
                     cel[i].chlde    = chlde;   
                     cel[i].chllnth  = chllnth;  
                     cel[i].chllnthc = chllnthc;         
                     cel[i].chllnthe = chllnthe; 
                     cel[i].chlm     = chlm;

/************************************************************/
/* start of code section added by Dave Peterson, March 1996 */
/************************************************************/		     

                     cel[i].chl_slope     = chlslope;
                     cel[i].ch_side_slope = chlsslop;

/**********************************************************/
/* end of code section added by Dave Peterson, March 1996 */
/**********************************************************/
                     }
                  }

           V_clear();
           V_line(2,"More channel input? (y/n)");
           V_ques(yesno1,'s',2,60,3);
           V_intrpt_ok();
           if(!V_call()) exit(1);
           if((strncmp(yesno1,"n",1)) == 0 || 
               (strncmp(yesno1,"N",2)) == 0 ) {
                   return;
                   };

        dchl_input();


}
