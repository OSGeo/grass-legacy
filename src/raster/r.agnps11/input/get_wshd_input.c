
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

#include "agnps_input.h"

void get_wshd_input()
{
        int V_clear(), V_line(), V_call(), V_ques(), V_intrpt_ok();

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

	if(!V_call()) exit(1);


	V_clear();
	V_line(1,"	Watershed Input ");
	V_line(3,"	1. Watershed Description ");
	V_line(5,"	2. Rainfall Amount in inches"); 
	V_line(7,"	3. Energy Intensity Value in English Units");
	V_line(9,"	4. Antecedent Moisture Condition (1,2 or 3)");
	V_line(11,"	5. Enter the cell size in meters ");
	V_line(13,"	6. Enter a file name to save in AGNPS format"); 
	V_line(14,"	with .dat extension");
	V_line(16,"	7. Enter the watershed name ");
	V_line(17,"	(all the input layers should have the same watershed name)"); 
	V_line(18,"	(with proper extension) ");
	V_ques(wshd_des,'s',3,60,15);
	V_ques(&rainfall,'f',5,60,5);
	V_ques(&ei,'f',7,60,5);
	V_ques(&amc,'i',9,60,1);
	V_ques(&grid_res,'i',11,60,4);
	V_ques(in_fl_name,'s',13,60,15);
	V_ques(wshd_name,'s',16,60,15);

	V_intrpt_ok();

	if(!V_call()) exit(1);

        return;
}
