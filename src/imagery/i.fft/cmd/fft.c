/***** Fast Fourier Transformation of Two Dimensional Sattelite Data *****/
/*

Programmer : Ali R. Vali
             Center for Space Research
             WRW 402
             University of Texas
             Austin, TX 78712-1085

             (512) 471-6824

Modfied by: David B. Satnik
            Central Washington University
            GIS Laboratory

*/
#include <math.h>

#define TWOPI 6.283185307179590 /* Constant value of 2 pi */
/* The "fft" (fourn) function is  */
/* Based on "Numerical Recipies in C; The Art of Scientific Computing"
   (Cambridge University Press, 1988).  Copyright (C) 1986, 1988 by
   Numerical Recipes Software.  Permission is granted for unlimited
   use within GRASS only.  */

/*****************************************************************************/
/*                                                                           */
/* Fast Fourier Transform Routine for two-dimensional array                  */
/* Adapted from N.M. Brenner Algorithm (Numerical Recipes in C pp. 407-412)  */
/*                                                                           */
/* Input arguments: i_sign - direction of transform :                        */
/*                                -1 -> transform, +1 -> inverse transform   */
/*                  DATA   - pointer to a complex linear array in row major  */
/*                           order containing the data and the result.       */
/*                  NN     - value of DATA dimension                         */
/*                  dimc   - value of image column dimension (max power of 2)*/
/*                  dimr   - value of image row dimension (max power of 2)   */
/*                                                                           */
/*****************************************************************************/

int fft (int i_sign, double *DATA[2], int NN, int dimc, int dimr)
{
    int    i, j, ip1, ip2, i1, i2, i3, i2rev, i3rev, ibit, ifp1, ifp2, k1, k2;
    int    N, Nprev, Nrem;
    double tempr, tempi, wr, wi, wpr, wpi, wtemp, theta, norm;
    double swr, swi;

    /* Apply normalization factor */

    norm = 1.0 / sqrt((double)NN);
    for (i = 0; i < NN; i++)
      for (j = 0; j < 2; j++)
        *(DATA[j]+i) *= norm;

    /* Begin FFT algorithm */
    Nprev = 1;
    N = dimr;

    for (i = 0; i < 2; i++)
    {
        Nrem = NN / (N * Nprev);
        ip1 = Nprev * N;

        /* This is the bit reversal section */

        i2rev = 0;
        for (i2 = 0; i2 < ip1; i2 += Nprev)
        {
            if (i2 < i2rev)
            {
                for (i1 = i2; i1 < (i2+Nprev); i1++)
                    for (i3 = i1; i3 < NN; i3 += ip1)
                    {
                        i3rev = i2rev + i3 - i2;
                        tempr = *(DATA[0]+i3);
                        tempi = *(DATA[1]+i3);
                        *(DATA[0]+i3) = *(DATA[0]+i3rev);
                        *(DATA[1]+i3) = *(DATA[1]+i3rev);
                        *(DATA[0]+i3rev) = tempr;
                        *(DATA[1]+i3rev) = tempi;
                    }
            }   /* end if */
            ibit = ip1 >> 1;
            while ((ibit >= Nprev) && (i2rev >= ibit))
            {
                i2rev -= ibit;
                ibit >>= 1;
            }
            i2rev += ibit;
        }   /* end bit reversal section */

        /* Begin Danielson-Lanczos section of algorithm */

        ifp1 = Nprev;
        while (ifp1 < ip1)
        {
            ifp2 = 2 * ifp1;
            theta = i_sign * TWOPI / (ifp2 / Nprev);
            wpr = cos(theta);
            wpi = sin(theta);
            wr = 1.0;
            wi = 0.0;
            for (i3 = 0; i3 < ifp1; i3 += Nprev)
            {
                for (i1 = i3; i1 < (i3+Nprev); i1++)
                    for (i2 = i1; i2 < NN; i2 += ifp2)

                    /* Danielson-Lanczos formula */
                    {
                        k1 = i2;
                        k2 = k1 + ifp1;
                        swr = wr;
                        swi = wi;
                        tempr = swr * *(DATA[0]+k2) - swi * *(DATA[1]+k2);
                        tempi = swr * *(DATA[1]+k2) + swi * *(DATA[0]+k2);
                        *(DATA[0]+k2) = *(DATA[0]+k1) - tempr;
                        *(DATA[1]+k2) = *(DATA[1]+k1) - tempi;
                        *(DATA[0]+k1) = *(DATA[0]+k1) + tempr;
                        *(DATA[1]+k1) = *(DATA[1]+k1) + tempi;
                    }

                /* trigonometric recurrence */
                wtemp = wr;
                wr = wr * wpr - wi * wpi;
                wi = wi * wpr + wtemp * wpi;
            }
            ifp1 = ifp2;

        }   /* end while */

        Nprev = N;
        N = dimc;
    }   /* end main loop */

    return(0);
}   /* end FFT */
