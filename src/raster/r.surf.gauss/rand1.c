/****************************************************************/
/***    Random Number Generator (Uniform Deviates 0.0 -> 1.0) ***/
/***                                                          ***/
/*** Based on three linear congruential generators:           ***/
/***  One for most significant part,                          ***/
/***  One for the least significant,                          ***/
/***  One for a shuffling routine (Knuth, 1981).              ***/
/***                                     [Press et al, 1988]  ***/
/***                                                          ***/
/*** Coded  Oct 23 1991                                       ***/
/*** Version 1.0                                              ***/
/***                                                          ***/
/****************************************************************/

			/* Arbitrary constants */
#define M1 259200	
#define IA1 7141
#define IC1 54773
#define RM1 (1.0/M1)
#define M2 134456
#define IA2 8121
#define IC2 28411
#define RM2 (1.0/M2)
#define M3 243000
#define IA3 4561
#define IC3 51349


float 
rand1 (int seed)
{
	static long 	ix1,ix2,ix3;
	float		temp;
	int		j;
	static float	r[98];
	static int	iff=0;
	
	if (seed < 0 || iff==0) /* Initialise if negative seed is given. */
				 /* This MUST be done on the first call.  */
	{
		iff=1;
		ix1=(IC1-seed) % M1;		/* Seed the 1st routine */
		ix1=(IA1*ix1+IC1) % M1;	/* and use it to seed the */
		ix2=ix1 % M2;			/* 2nd and 3rd routines */
		ix1=(IA1*ix1+IC1) % M1;
		ix3=ix1 % M3;
		
		for (j=1; j<=97; j++)
		{
			ix1=(IA1*ix1+IC1) % M1;  /* Fill table with sequen- */
			ix2=(IA2*ix2+IC2) % M2;  /* tial uniform deviates   */
			r[j]=(ix1+ix2*RM2)*RM1;  /* from the 1st 2 routines */
		}
		seed=1;
	}

	ix1=(IA1*ix1+IC1) % M1;	/* Generate the next number in the */
	ix2=(IA2*ix2+IC2) % M2;	/* sequence of routine		   */
	ix3=(IA3*ix3+IC3) % M3;

	j=1 + ((97*ix3) / M3);  /* Get random position ion array */
	temp = r[j];
	r[j]=(ix1+ix2*RM2)*RM1;        /* Refil array and return entry */
	return(temp);		        
}


