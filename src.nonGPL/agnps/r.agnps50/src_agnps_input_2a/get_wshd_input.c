
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
#include <stdio.h>
#include <string.h>
#include <math.h>

float EI_calc(char [], double, double);

get_wshd_input()
{

        FILE    *temp;
        char	buf[1024];
        char    pts[3], c;
        int     i;
        float amc_value = 2;
        
        strcpy(wshd_des," ");
        rainfall = 2.5; 
        ei       = 30.0;
        strcpy(stmtp,"II");
        stmtm    = 24.0;
        stmnppm  = 0.8;
        strcpy(pkflcal,"AGNPS");
        strcpy(gmorp,"y");
        strcpy(hydrosf,"K Coef.");
        kfact    = 484.0;


/**************************************************************************/
/* The next block of text printed to the screen is commented out because  */
/* this header information was already displayed in the pre_aggrs module. */
/*                                                                        */   
/* MIKE FOSTER 8-31-95                                                    */  
/**************************************************************************/ 


/*       
        V_clear();
        V_line(1,"		AGNPS/GRASS Input Interface Tool");
        V_line(3,"		     Version 1.1 (4/30/92)");
        V_line(5,"		  R. Srinivasan and B.A. Engel"); 
        V_line(7,"		     Phone: 317-494-1198 ");
        V_line(9,"		  email: engelb@ecn.purdue.edu");
        V_line(11,"		Agricultural Engineering Department ");
        V_line(12,"			Purdue University"); 
        V_line(13,"		     West Lafayette, IN 47907"); 
        V_line(15,"	(c)Copyright, 1992 Purdue Research Foundation, West");
        V_line(16,"	Lafayette, Indiana 47907. All Rights Reserved. Unless");
        V_line(17,"	permission is granted, this material shall not be");
        V_line(18,"	copied, reproduced or coded for reproduction by any");
        V_line(19,"	electrical, mechanical or chemical processes,  or");
        V_line(20,"	combinations thereof, now known or later developed.");

        V_intrpt_ok();
        if(!V_call()) exit(1);

        V_clear();

        V_line(5,"Modifications for point source input and update to");
        V_line(8,"  AGNPS 5.0 by Z. Li and M. A. Foster July 1995");
        V_line(11," (c) 1995, Penn State University, University Park");
        V_intrpt_ok();
        if(!V_call()) exit(1);  

*/

        V_clear();
        V_line(1,  "Precipitation and Model Parameters");
        V_line(2,  "1.  Rainfall amount (inches)");  
        V_line(4,  "2.  Storm type (e.g. I, II, III)");
        V_line(6,  "3.  Duration of the storm (hours)"); 
        V_line(8,  "4.  Energy intensity value in English units");
        V_line(9,  "     (enter 0 if unknown)");
        V_line(10, "5.  Soil antecedant moisture condition (1,2 or 3)");
        V_line(12, "6.  Concentration of nitrogen in precipitation (ppm)");
        V_line(14, "7.  Peak flow calculation method (SCS-TR55/AGNPS)");
        V_line(16, "8.  Geomorphic calculations (y/n)"); 
        V_line(18, "9.  Hydrograph shape factor (K Coef./ % Runoff)");
        V_line(20,"10. K coefficient or % runoff value");
        

        V_ques(&rainfall,'f',2,55,5);
        V_ques(stmtp,'s',4,55,3);
        V_ques(&stmtm,'f',6,55,5);
        V_ques(&ei,'f',8,55,5);
        V_ques(&amc_value,'f',10,55,3);
        V_ques(&stmnppm,'f',12,55,8);
        V_ques(pkflcal,'s',14,55,10);
        V_ques(gmorp,'s',16,55,5);
        V_ques(hydrosf,'s',18,55,10);
        V_ques(&kfact,'f',20,55,10);
        
        V_intrpt_ok();
         
        if(!V_call()) exit(1);

        /* section added in to calculate the energy intensity if 
           user enters zero */
        if(ei==0) {   
           ei=EI_calc(stmtp,rainfall,stmtm);     
         }
         
         amc = amc_value;
    
        temp = fopen("temp_file","r");
        fscanf(temp,"%8i%15s%15s\n",&grid_res,in_fl_name,wshd_name);

        /* get watershed description */
        for (i = 0; i < 15; i++)
         fscanf(temp, "%c", &(wshd_des[i]));
        wshd_des[15] = 0;

        fclose(temp);         

}

