#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <string.h>

double gradInterpolate(double x, double y, double deltaX, double deltaY,
		       int xNum, int yNum, double xMin, double yMin,
		       double *parVect)
{

    int row, col, N;
    double csi, eta, a, b, c, d;

    row = (int)((x - xMin) / deltaX);
    col = (int)((y - yMin) / deltaY);
    N = (yNum * col) + row;
    csi = x - (xMin + (col * deltaX));
    eta = y - (yMin + (row * deltaY));

    /* calcolo dei parametri della sup bilineare della maglia specifica */
    d = parVect[N];
    b = parVect[N + 1] - d;
    a = parVect[N + yNum] - d;
    c = parVect[N + 1 + yNum] - a - b - d;

    /* calcolo del gradiente */
    grad =
	sqrt((a * a) + (c * c * eta * eta) + (b * b) + (c * c * csi * csi) -
	     (2 * a * c * eta) - (2 * b * c * csi));

    return grad;
}				//end main

/***************************************************************************************************
			
				------- parametri passati alla procedura---------

grad = gradInterpolate(X osservata ,Y osservata, passo spline direzione E, passo spline direzione N,
			 numero di spline in direz E,  numero di spline in direz N,
			 limite Est della regione, limite Sud della regione,
			 vettore dei parametri stimati dall'interp. bilineare);	

******************************************************************************************************/
