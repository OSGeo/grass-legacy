
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
         This function is created to allow entries of fertilizer 
         data for the user selected cells.
           
                   June, 1995
                       
                   Zhian Li,  
                   Department of Entomology
                   501 ASI Building
                   The Pennsylvania State University 
                   University Park, Pa 16802 

         ===========================================================
*/
         

         
  dfert_input()
     {

	int	i, j, nrows, ncols, ct, land_use;
        char    yesno1[3];
        int     cell_id;


/*      Begin process the fertilizer input    */



           i = 0;
           dfert_lvl_input();
           i = fcel_id;
           if(fcel_id <= 0 || fcel_id > tot_cells) {
             fprintf (stderr,"Illegal cell number %d\n",fcel_id);
             fprintf (stderr,"Please enter a valid cell number\n");
             fprintf (stderr,"between 1 and %i\n",tot_cells);
             dfert_lvl_input();
             }
           cel[i].fert_level = 5;
           cel[i].fert4n = fert4n; 
           cel[i].fert4p = fert4p;
           cel[i].fert_nfac = fert_nfac;
           cel[i].fert_pfac = fert_pfac;

           V_clear();
           V_line(2,"More fertilizer input? (y/n)");
           V_ques(yesno1,'s',2,60,3);
           V_intrpt_ok();
           if(!V_call()) exit(1);
           if((strncmp(yesno1,"n",1)) == 0 || 
               (strncmp(yesno1,"N",2)) == 0 ) {
                   return;
                   };
           

        dfert_input();


}
