/* Program to calculate the infiltration rate */
/******************************************************************** 
* Variables list for the Green and Ampt formulation					*
* ksat =>> Saturated hydraulic conductivity							*
* suwf =>> Suction at the wetting front								*
* phi =>> porosity													*
* degofs =>> Initial degree of saturation 							*
* delta_f =>> Incremental change in volume							*
* Fvolume =>> Infiltration volume									*
* del_t =>> Time at which infiltration is calculated				*
* de_theta =>> phi*(1 - degofs),Initial moisture deficit			*
* Ref. D.stephenson and M.E. Meadows pg. 15 - 19					*
*********************************************************************/
#include <stdio.h>
#include <math.h>

extern int max,delta_t;
extern double *infil,*hold,*Fvolume;
extern double *ksat,*suwf,*phi,*degofs;

void
infiltration()
{
	int i;
	double delta_f=0.0,del_theta=0.0,temp_var=0.0;
		for(i=0;i<max;i++){
			temp_var = (0.25 * ksat[i] * delta_t) - (Fvolume[i]);
			del_theta = phi[i] * (1 - degofs[i]);
			delta_f = temp_var + sqrt((double)((temp_var * temp_var) + ksat[i] * delta_t * (hold[i] * del_theta + suwf[i] * del_theta + Fvolume[i])));
			Fvolume[i] += delta_f;
			infil[i] = delta_f/delta_t;
		}
}

