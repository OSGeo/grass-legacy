
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



  ptsrc_input()
     {

        int	i, j, nrows, ncols, ct, land_use;
        FILE    *clid, *fdlot, *nfdlot;
        char    yesno1[3], msg[81];
        int     cell_id;
        char    answer[10];
        int     test;


/*      Begin process the point source input    */


        if((fdlot = fopen("fdlot.dat","a")) == NULL) {
           fprintf(stderr,"Cannot open feedlot data file");
           fprintf(stderr,"for write in point source input\n");
           exit(0);
           }

        if((nfdlot = fopen("nfdlot.dat","a")) == NULL) {
           fprintf(stderr,"Cannot open nfeedlot data file");
           fprintf(stderr,"for write in point source input\n");
           exit(0);
           }

        tptsrc = tptsrc + 1;
        mfdlt  = mfdlt  + 1;
        mnfdlt = mnfdlt + 1;

        test = 0;
        while ( test == 0 ) {
           system("$GISBASE/etc/agnps50/get_cell_id.sh");
           if((clid = fopen("cell.id","r")) == NULL) {
              fprintf(stderr,"Cannot open cell ID file");
              exit(0);
              }
           fscanf(clid,"%i\n",&cell_id);


/***************************************************************/
/* start of code section modified by Dave Peterson, April 1996 */
/***************************************************************/

           strcpy(yesno1, "y");
           sprintf(msg, "The cell number is: %i\n", cell_id);
           V_clear();
           V_line(2, msg);
           V_line(4,"Is this the cell that contains the point source? (y/n)");
           V_line(5,"(if no, you may select another cell)");
           V_ques(yesno1,'s',4,56,3);
           V_intrpt_ok();
           if(!V_call()) exit(1);
           V_clear();
           if (approx_compare(yesno1, "y") == 0 ||
               approx_compare(yesno1, "yes") == 0  )
            test = 1;

/* old code section ...

           fprintf (stderr,"The cell number is:  %i\n",cell_id);
           fprintf (stderr,"\n");
           fprintf (stderr,"\n");
           fprintf (stderr,"Is this the cell you want to enter point source (y/n)?\n");
           fprintf (stderr,"You will be asked to select a cell again if answer NO\n");

           gets(answer);
           if ((strncmp(answer,"Y",1) == 0) || (strncmp(answer,"y",1) == 0)) {
              test = 1;
               }
*/

/*************************************************************/
/* end of code section modified by Dave Peterson, April 1996 */
/*************************************************************/

           fclose(clid);
           }
           get_pts_dat();



           if(pntsrctp < 0 ) {
             fprintf(nfdlot,"%i \n",cell_id);
             fprintf(nfdlot,"%8.3f%8.2f%8.2f%8.2f%8i\n",wdisch,tn,tp,tcod,
                ptsloc);
             }

           if(pntsrctp == 1 ) {
             fprintf(fdlot,"%i \n",cell_id);
             fprintf(fdlot,"%8.2f%8i%8.2f%8i%8i%8i\n",fdarea,fcs,rfarea,
             fln,flp,flcod);
             fprintf(fdlot,"%16.2f%8i%8.2f%8i\n",tb1a,tb1cnt,ad1a,ad1cnt);
             fprintf(fdlot,"%16.2f%8i%8.2f%8i\n",tb2a,tb2cnt,ad2a,ad2cnt);
             fprintf(fdlot,"%16.2f%8i%8.2f%8i\n",tb3a,tb3cnt,ad3a,ad3cnt);
             fprintf(fdlot,"%16.2f%8i%8.2f%8i\n",tb4a,tb4cnt,ad4a,ad4cnt);
             fprintf(fdlot,"%16.2f%8i%8.2f%8i\n",tb5a,tb5cnt,ad5a,ad5cnt);
             fprintf(fdlot,"%16.2f%8i%8.2f%8i\n",tb6a,tb6cnt,ad6a,ad6cnt);

             fprintf(fdlot,"%16.1f%8.2f%8i\n",bf1slp,bf1scnt,bf1fll);
             fprintf(fdlot,"%16.1f%8.2f%8i\n",bf2slp,bf2scnt,bf2fll);
             fprintf(fdlot,"%16.1f%8.2f%8i\n",bf3slp,bf3scnt,bf3fll);
             fprintf(fdlot,"%16i%8.2f%8.2f%8.2f\n",nanimal1,animcod1,animp1,
             animn1);
             fprintf(fdlot,"%16i%8.2f%8.2f%8.2f\n",nanimal2,animcod2,animp2,
             animn2);
             fprintf(fdlot,"%16i%8.2f%8.2f%8.2f\n",nanimal3,animcod3,animp3,
             animn3);
             }

           strcpy(yesno1, "y");
           V_clear();
           V_line(2,"Any more point sources? (y/n)");
           V_line(4,"WARNING:");
           V_line(5,"A maximum of 9 point sources are allowed per cell.");
           V_line(6,"Additional data will be ignored!");
           V_ques(yesno1,'s',2,55,3);
           V_intrpt_ok();
           if(!V_call()) exit(1);
           if((approx_compare(yesno1,"n")) == 0 || 
               (approx_compare(yesno1,"no")) == 0 ) {
                   fclose(nfdlot);
                   fclose(fdlot);
                   return;
                   };

           tptsrc = mfdlt + mnfdlt;
           if(tptsrc >= 9 ) {
             fprintf(stderr,"WARNING!!! \n");
             fprintf(stderr,"The number of point sources reaches the limit\n");
             fprintf(stderr,"No more entry is allowed\n"); 

             fclose(nfdlot);
             fclose(fdlot);
             return;
             }

        fclose(nfdlot);
        fclose(fdlot);
        ptsrc_input();


}
