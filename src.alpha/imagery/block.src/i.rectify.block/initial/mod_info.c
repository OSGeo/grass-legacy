#include <stdio.h>
#include "initial.h"

mod_init_info(have_old, init_info)
	int  have_old ;
        struct Camera_Exp_Init *init_info;
{
	if( ! have_old)
	{
          /* make zero */
	}
	V_clear() ;
	V_line(1,"                   Please provide the following information:") ;
        V_line(2,"+------------------------------------------------------------------------------+");
	V_line(4,"     Initial Camera Exposure X-coordinate Meters:");
	V_line(5,"     Initial Camera Exposure Y-coordinate Meters:");
	V_line(6,"     Initial Camera Exposure Z-coordinate Meters:");
	V_line(7,"     Initial Camera Omega (roll) radians:");
	V_line(8,"     Initial Camera Phi (pitch)  radians:");
	V_line(9,"     Initial Camera Kappa (yaw)  radians:");

	V_line(11,"     Apriori standard devation X-coordinate Meters:");
	V_line(12,"     Apriori standard devation Y-coordinate Meters:");
	V_line(13,"     Apriori standard devation Z-coordinate Meters:");
	V_line(14,"     Apriori standard devation Omega (roll) radians:");
	V_line(15,"     Apriori standard devation Phi (pitch)  radians:");
	V_line(16,"     Apriori standard devation Kappa (yaw)  radians:");

	V_line(18,"     Use these values at run time? (1=yes, 0=no)"); 
        V_line(19,"+------------------------------------------------------------------------------+");

	V_ques( &(init_info->XC_init),     'd', 4,  60, 15-1) ;
        V_ques( &(init_info->YC_init),     'd', 5,  60, 15-1) ;
	V_ques( &(init_info->ZC_init),     'd', 6,  60, 15-1) ;
	V_ques( &(init_info->omega_init),  'd', 7,  60, 15-1) ;
        V_ques( &(init_info->phi_init),    'd', 8,  60, 15-1) ;
	V_ques( &(init_info->kappa_init),  'd', 9,  60, 15-1) ;
        V_ques( &(init_info->XC_var),      'd',11,  60, 15-1) ;
	V_ques( &(init_info->YC_var),      'd',12,  60, 15-1) ;
        V_ques( &(init_info->ZC_var),      'd',13,  60, 15-1) ;
	V_ques( &(init_info->omega_var),   'd',14,  60, 15-1) ;
        V_ques( &(init_info->phi_var),     'd',15,  60, 15-1) ;
	V_ques( &(init_info->kappa_var),   'd',16,  60, 15-1) ;
	V_ques( &(init_info->status),      'i',18,  60, 2) ; 

	V_intrpt_ok();
	if(!V_call())
	    exit(0);
}





