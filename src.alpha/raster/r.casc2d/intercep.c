
#include "all.h"

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
void INTERCEPTION(vect,dt,Inter,sv,exp,rainrate,intrate)
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

int     vect;
float   *Inter,*sv,*exp,rainrate,*intrate;
double  dt;

{
double  tinc,intinc;

if(Inter[vect] < sv[vect])
{
   *intrate=rainrate;
   if((Inter[vect]+(*intrate)*dt) > sv[vect])
   { 
      tinc=(sv[vect] - Inter[vect])/rainrate;
      intinc=tinc*rainrate + exp[vect]*rainrate*(dt - tinc);
      *intrate=intinc/dt;
   }
}
else
{
   *intrate=exp[vect]*rainrate;
}

Inter[vect]=Inter[vect] + (*intrate)*dt;
return;

}
