/*To calculate the constant Band width and the number of elements, This program 
is obviously used before the multiplication program */
void
find_row_no_and_element(cbw,noe,bw,max)
int *cbw,*noe,bw,max;
{
	int i;
	*noe = 0;
	*cbw = 0;

	/* case I */
	if( bw < (max -bw)){
		(*cbw) = 2 * bw - 1;
	/* To find the number of band elements */
	/* variable range is multiplied by 2 for the upper and lower portions */
	for(i=0; i< bw;i++)
		(*noe) += bw + i;
	(*noe) *= 2;
	/* Number of elements in the constant band width range */
	(*noe) += (*cbw) * ( max - (2 * bw));
	}

	/* case II */
	else{
	/* variable range multiplied by 2 for the upper and lower portions */
	for(i=0;i<(max -bw);i++)
		(*noe) += bw + i;
	(*noe) *= 2;
	/* Number of elelments in the constant band width range */
	(*noe) += (2 * bw - max) * max;
	}

}


/* This program stores the matrix,row by row in the array A */
void 
store_matrix(stiffness,A,max,cbw,bw)
short int **stiffness,*A;
int max,cbw,bw;
{
	int i,j,k,t=0;
	/* case I */
	if (bw < (max - bw)){

	/* storing the upper trapezium */
	   
	for(i=0,j=bw;i<(bw -1);i++,j++)						/* Row index */
			for(k=0;k<j;k++,t++)					/* Column index */
				A[t] = stiffness[i][k];
	
	/* storing the constant band part */	
	
	for(i=(bw-1),j=0;i<=(max -bw);i++,j++)				/* Row index */
			for(k=j;k< (j + cbw) ;k++,t++)			/* Column index */
				A[t] = stiffness[i][k];
	
	/* storing the lower trapezium */	
	
	for(i=(max - bw +1),j=(max-cbw+1) ;i < max ;i++,j++) /* Row index */
			for(k=j;k< max ;k++,t++)       			/* Column index */
				A[t] = stiffness[i][k];
	}
	 
	/* case II  */

	else{
		/* storing the upper trapezium */

		for(i=0,j=bw ;i <max -bw;i++,j++)			/* Row index */
			for(k=0;k<j;k++,t++)					/* Column index */
				A[t] = stiffness[i][k];

		/* storing the constant band part */

		for(i=max -bw ; i< bw ; i++)				/* Row index */
			for(j=0; j < max; j++, t++)				/* Column index */
				A[t] = stiffness[i][j];

		/* storing the lower trapezium */

		for(i=bw,j=1;i<max;i++,j++)					/* Row index */
			for(k=j;k<max;k++,t++)					/* Column index */
				A[t] = stiffness[i][k];

 	}

}



/*Tool to carry out banded-part multiplication */
void
band_multiply(A,b,x,max,cbw,bw)
short int *A;
int max,bw,cbw;
double *b, *x;
{
	int i,j,k,t=0;
	/* case I */
	if(bw < (max - bw)){

		/* Multiply the top trapezium, the elements are stored as a one
		dimensional array */
	   
	for(i=0,j=bw;i<(bw -1);i++,j++)					/* Row index */
			for(k=0;k<j;k++,t++)					/* Column index */
				b[i] += A[t] * x[k];
	
		/* Multiplying the constant Band width portion */
	for(i=(bw-1),j=0;i<=(max -bw);i++,j++)			/* Row index */
			for(k=j;k< (j + cbw) ;k++,t++)			/* Column index */
				b[i] += A[t] * x[k];
	
		/* Multiplying the lower trapezium */
	for(i=(max - bw +1),j=(max-cbw+1) ;i < max ;i++,j++) /* Row index */
			for(k=j;k< max ;k++,t++)       			/* Column index */
				b[i] += A[t] * x[k];
	}
	 

	/* case II */
	else{
		/* Top trapezium */
		for(i=0, j= bw; i< (max - bw); i++, k++)
			for(k=j;k<j;k++,t++)
				b[i] += A[t] * x[k];

		/* constant band */
		for(i=(max -bw);i<bw;i++)
			for(j=0;j<max;j++,t++)
				b[i] += A[t] * x[j];

		/* lower trapezium */
		for(i=bw, j=1; i< max;i++,j++)
			for(k=j;k<max;k++,t++)
				b[i] += A[t] * x[k];
	}

}
