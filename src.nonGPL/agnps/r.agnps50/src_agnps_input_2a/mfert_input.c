
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



  mfert_input()
     {

        int	i, j, nrows, ncols, ct, land_use;
        FILE    *clid;
        char    yesno1[3];
        int     cell_id;


/*      Begin process the fertilizer input    */


           system("$GISBASE/etc/agnps50/get_cell_id.sh");
           if((clid = fopen("cell.id","r")) == NULL) {
              fprintf(stderr,"Cannot open cell ID file");
              exit(0);
              }
           fscanf(clid,"%i\n",&cell_id); 
           fclose(clid);

           fcel_id = cell_id;
           fert_lvl_input();
           cel[cell_id].fert_level = 5;
           cel[cell_id].fert4n = fert4n; 
           cel[cell_id].fert4p = fert4p;
           cel[cell_id].fert_nfac = fert_nfac;
           cel[cell_id].fert_pfac = fert_pfac;

           V_clear();
           V_line(2,"More fertilizer input? (y/n)");
           V_ques(yesno1,'s',2,30,3);
           V_intrpt_ok();
           if(!V_call()) exit(1);
           if((strncmp(yesno1,"n",1)) == 0 || 
               (strncmp(yesno1,"N",2)) == 0 ) {
                   return;
                   };


        mfert_input();


}
