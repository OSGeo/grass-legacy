
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



  mchl_input()
     {

        int	i, j, nrows, ncols, ct, land_use;
        FILE    *clid;
        char    yesno1[3];


     /*      Begin process the channel data input    */



           system("$GISBASE/etc/agnps50/get_cell_id.sh");  
           if((clid = fopen("cell.id","r")) == NULL) {
              fprintf(stderr,"Cannot open cell ID file");
              exit(0);
              }
           fscanf(clid,"%i\n",&chl_cel_id); 
           fclose(clid);
           i = chl_cel_id;


               if(hydro_mdl == 0 ) {
                  if(geom_mod == 0 ) {
                     chlslope=cel[i].chl_slope;
                     chlsslop=cel[i].ch_side_slope;
                     channel00_dat(i);

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
                     channel01_dat(i);

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
                     channel10_dat(i);

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
                     channel11_dat(i);

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






           strcpy(yesno1, "y");
           V_clear();
           V_line(2,"More channel input? (y/n)");
           V_ques(yesno1,'s',2,27,3);
           V_intrpt_ok();
           if(!V_call()) exit(1);
           if((strncmp(yesno1,"n",1)) == 0 || 
               (strncmp(yesno1,"N",2)) == 0 ) {
                   return;
                   };

        mchl_input();


}
