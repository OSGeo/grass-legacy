/****************************************************************
  This file calculates the Energy Intensity value if the user
  enters in 0 for the EI value 
  
  by: Trey Askew                 7/2/96
****************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>

float EI_calc(char type[], double precip, double dur)
{
	double temp1;
	float temp2;
	float temp3;
	float answer;
	double Coeff_A;
	double Coeff_B;                /*Variable to hold coefficients */ 
	                              /*determined                    */
	int Valid_storm;              /*Checks to see if valid storm  */
	                              /*type (1=valid, 0=invalid)     */

	Valid_storm = 0;              /*set storm to invalid initially*/

	if((strcmp(type,"1")==0) || (strcmp(type,"I")==0)) {
	   Coeff_A = 15.03;
	   Coeff_B = 0.5780;
	   Valid_storm = 1;
	   }
	else if((strcmp(type,"1a")==0) || (strcmp(type,"Ia")==0) 
	         || (strcmp(type,"1A")==0) || (strcmp(type,"IA")==0)) {
	   Coeff_A = 12.98;
	   Coeff_B = 0.7488;
	   Valid_storm = 1;
	   }
    else if((strcmp(type,"2")==0) || (strcmp(type,"II")==0)) {
	   Coeff_A = 17.9000;
	   Coeff_B = 0.4134;
	   Valid_storm = 1;
	   }
	else if((strcmp(type,"3")==0) || (strcmp(type,"III")==0) 
	    || (strcmp(type,"IIa")==0) || (strcmp(type,"2A")==0)
	    || (strcmp(type,"2a")==0) || (strcmp(type,"IIA")==0)) {
	   Coeff_A = 21.5100;
	   Coeff_B = .2811;
	   Valid_storm = 1;
	   }
	else {
	   fprintf (stderr,"Invalid storm type!\n");
	   return 0;
	   }

	if(Valid_storm == 1) {
	   temp1 = 2.119*exp(0.0086*log(dur));
	   temp2 = Coeff_A*exp(temp1*log(precip));
	   temp3 = temp2/exp(Coeff_B*log(dur));
	   answer = temp3;
	   }
	return answer; 	                                
}
