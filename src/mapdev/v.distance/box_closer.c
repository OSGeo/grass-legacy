#include <stdio.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"


box_closer(Line,X,Y,d)
    double X, Y,d;
    P_LINE *Line;

 
{
    double S,N,W,E;
    double d1,d2;

    S=Line->S;
    N=Line->N;
    W=Line->W;
    E=Line->E;

    d1=sqrt( (pow((X-W),2) + pow((Y-S),2)) );

    if(d1 < d) return(1);
    d1=sqrt( (pow((X-W),2) + pow((Y-N),2)) );

    if(d1 < d) return(1);
    d1=sqrt( (pow((X-E),2) + pow((Y-N),2)) );

    if(d1 < d) return(1);
    d1=sqrt( (pow((X-E),2) + pow((Y-S),2)) );

    if(d1 < d) return(1);


    return(0);
}