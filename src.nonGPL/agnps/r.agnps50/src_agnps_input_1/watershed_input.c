
        /*---------------------------------------------------------*
         *               AGNPS/GRASS Interface Project             *
         *  Developed in the Agriculture Engineering Department    *
         *                at Purdue University                     *
         *                        by                               *
         *         Raghavan Srinivasan and Bernard Engel           *
         *                                                         *
         *   (c)Copyright, 1992 Purdue Research Foundation, West   *
         *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
         *   permission is granted, this material shall not be     *
         *   copied, reproduced or coded for reproduction by any   *
         *   electrical, mechanical or chemical processes,  or     *
         *   combinations thereof, now known or later developed.   *
         *---------------------------------------------------------*/

/*      June, 1991  Agricultural Engineering, Purdue University
        Raghavan Srinivasan (srin@ecn.purdue.edu)

        void get_wshd_input()

        user interface to get inputs to run the AGNPS-GRASS input
        interface and stores the name to globel variables.
*/

/*   This GRASS AGNPS interface has been modified to*/
/*   run the latest version of AGNPS-- AGNPS version*/
/*   Significant modification has been made in this */
/*   interface because of the significant difference*/
/*   between version 3.0 and version 5.0 in AGNPS   */
/*   input formats.  In addition, new AGNPS input   */
/*   parameters have been added in this interface.  */
/*   A new new method for entering point sources such*/
/*   as feedlot had been developed.   In this new   */
/*   interface, a cell map will be prompted for the */
/*   user to input the the point source location by */ 
/*   simply pointing the cursor to and clicking  on */
/*   the location where the point source is located */
/*   in the map.  An input quest screen will then be*/
/*   prompted for detailed description of the ponit */
/*   sources(s).                                    */
/*                                                  */
/*                        Zhian  Li                 */
/*                                                  */
/*                        Penn State University     */
/*                                                  */
/*                        July, 1995                */ 

#include "agnps_input.h"

watershd_input()
{

        FILE   *temp;
        int    i, end_reached;
        char   c;


        V_clear();

        V_line(0,  "                     WATERSHEDSS GRASS-AGNPS MODELING TOOL");
        V_line(1,  "                     Version 1.1 (5/96) modified 12/19/96");
        V_line(3,  "                                   D.E. Line");
        V_line(4,  "               Biological and Agricultural Engineering Department");
        V_line(5,  "                       NC State Univ., Raleigh, NC 27695");
        V_line(6,  "                                      and");
        V_line(7,  "                                  M.A. Foster");
        V_line(8,  "               Agricultural and Biological Engineering Department");
        V_line(9,  "                  Penn State Univ., University Park, PA 16802");
        V_line(11, "   Parts of the input file generator of this tool were based generally on the");
        V_line(12, "AGNPS/GRASS Input Interface Tool (version 1.1) developed by R. Srinivasan and");
        V_line(13, "B.A. Engel (1994).  Changes include updating the input file generator for AGNPS");
        V_line(14, "version 5.0, improvement in fertilizer application, curve number, and aspect");
        V_line(15, "map routines, and addition of the capability for user input of point source,");
        V_line(16, "channel, fertilizer, and pesticide data for specified cells.  A separate");
        V_line(17, "program for viewing model output was also developed.");
        V_line(18, "   Funding was provided through the U.S. EPA cooperative agreement #CR822270,");
        V_line(19, "Understanding the Role of Agricultural Landscape Feature Function and Position");
        V_line(20, "in Achieving Environmental Endpoints to the NCSU Water Quality Group.");

        V_intrpt_ok();
        if(!V_call()) exit(1);

        V_clear();

        grid_res   = 100;
        strcpy(wshd_name,"gaston");
        V_clear();
        V_line(3,  "File and General Information");
        V_line(5,  "1.  Enter a file name to save in AGNPS format");
        V_line(6,  "    with .dat extension (ex: kiser.dat)");
        V_line(8,  "2.  Enter the watershed name ");
        V_line(9,  "    (all input map layers should have the same"); 
        V_line(10, "     watershed name with proper extension)");
        V_line(12, "3.  Enter the watershed description (15 chars max.)");
        V_line(14, "4.  Enter the cell size in meters");
        V_line(16, "(if no pesticides in watershed, depress return key");
        V_line(17, "to skip pesticide map messages)");

        V_ques(in_fl_name, 's', 5, 55, 15);
        V_ques(wshd_name, 's', 8, 55, 15);
        V_ques(wshd_des, 's', 12, 55, 15);
        V_ques(&grid_res,'i', 14, 55, 4);

        V_intrpt_ok();
        if(!V_call()) exit(1);

        temp = fopen("temp_file","w");
        fprintf(temp,"%8i%15s%15s\n",grid_res,in_fl_name,wshd_name);

        /* write watershed description to file; if description is less than */
        /* 15 chars long, then pad with NULL chars                          */
        end_reached = 0;
        for (i = 0; i < 15; i++)
         { if (end_reached)
            c = 0;
           else if (wshd_des[i] == 0)
            { c = 0;
              end_reached = 1;
            }
           else
            c = wshd_des[i];

           fprintf(temp, "%c", c);
         }

        fclose(temp);
}
