/*  @(#)read_write.c	2.1  6/26/87 
*   Created by:  CERL, original in mapdev/dlg_to_bdlg
*   modified:    for ground coordinate conversions
*                                    R.L.Glenn, SCS,  12-1-87   
*   modified:    to handle ARC-INFO's universal poly is the WORLD 
*               		     R.L.Glenn, SCS,  12-8-87  
*/
#include <stdio.h>
#include "dlghead.h"          /* added  RLG */

#define FGET 	fgets(buff,82,file) /** 80 changed to 82 to pick up newline **/

read_int(file, num, buf)      /* for link records */
	FILE *file ;
	int num ;
	int buf[] ;
{
	char buff[82] ;  /*** 80 changed to 82 to pick up newline ***/
	int tmp[13] ;
	int nassign ;
	int n_read ;
	int todo ;
	int i ;
/*-------------------------------------------------------RLG---*/
	int strt=0 ;       /* added for world condition */
/*-------------------------------------------------------RLG---*/
	int error ;

	error = 0 ;
	nassign = 0 ;

	while (nassign < num)
	{
		FGET ;  /* read link record */
/*fprintf(stderr,"r_int:\n%s\n",buff);*/
		todo = num - nassign ;
		todo = (todo < 12) ? todo : 12 ;
		n_read = sscanf(buff,"%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d",
			tmp+0, tmp+1, tmp+2, tmp+3, tmp+4, tmp+5, 
			tmp+6, tmp+7, tmp+8, tmp+9, tmp+10, tmp+11) ;
		/*** n_read changed to todo in next line ***/

/*-------------------------------------------------------RLG---*/
/*                   if the first link number is zero, in the first area
			then modify it 			  */
		if ((nassign == 0) && (*(tmp) == 0))
		   {
		   strt = 1;
	           num = num - 1;
		   error = -1;    /* set a flag for later  */
		   }
/*                  with strt set to 1, elements are shifted left, removing
			the island indicator from the binary record   */
/*-------------------------------------------------------RLG---*/

		for(i=strt; i<todo; i++)
			buf[nassign++] = *(tmp+i) ;

/*-------------------------------------------------------RLG---*/
/*                   re-set the varibles so code works right, and
                          get all other links that may exist */
	        if (strt == 1) 
	          {
		   todo = todo - 1;
		   strt = 0;
		   n_read = n_read - 1;
		  }
/*-------------------------------------------------------RLG---*/

		if (n_read != todo)
		    return (num - nassign ) ;
	}
	return(error) ;
}

write_int (file, num, buf)     /* for link records */
	FILE *file ;
	int num ;
	int *buf ;
{
	int num_printed ;
	int at_num ;

	num_printed = 0 ;
	for (at_num=0; at_num<num; at_num++)
	{
		if (! (num_printed % 12) )
			if (num_printed)
				fprintf(file,"\n") ;
		fprintf(file,"%6d",buf[at_num]) ;
		num_printed++ ;
	}
	fprintf(file,"\n") ;
}

read_doubles(file, num, buf)     /* for coord. records */
	FILE *file ;
	int num ;
	double buf[] ;
{
	char buff[82] ;  /*** 80 changed to 82 to pick up newline ***/
	double tmp[6] ;
	int nassign ;
	int n_read ;
	int todo ;
	int i ;
	int error ;
/*-------------------------------------------------------RLG---*/
	double A1, A2, A3, A4;   
      
/*      set up coefficent parameters for conversion to ground 
             coordinates */
	A1 = *(coeff_params) ;
	A2 = *(coeff_params + 1) ;
	A3 = *(coeff_params + 2) ;
	A4 = *(coeff_params + 3) ;
/*-------------------------------------------------------RLG---*/

	error = 0 ;
	nassign = 0 ;

	while (nassign < num)
	{
		FGET ;  /* read coord. record */
		todo = num - nassign ;
		todo = (todo < 6) ? todo : 6 ;
		n_read = sscanf(buff,"%12lf %12lf %12lf %12lf %12lf %12lf",
			tmp+0, tmp+1, tmp+2, tmp+3, tmp+4, tmp+5) ;
		/*** n_read changed to todo in next line ***/
/*-------------------------------------------------------RLG---*/
			/* reset coord. value via parameters */
		for(i=0; i<todo; i++)
		     {
		     if (i == 0 || i == 2 || i == 4)	     
			*(tmp+i) = (*(tmp+i) * A1 + *(tmp+i) * A2 + A3);
		     if (i == 1 || i == 3 || i == 5)	     
			*(tmp+i) = (*(tmp+i) * A1 - *(tmp+i) * A2 + A4);
/*-------------------------------------------------------RLG---*/
		     buf[nassign++] = *(tmp+i) ;
		     }
		if (n_read != todo)
			return (num - nassign ) ;
	}
	return(error) ;
}

write_doubles (file, num, x_coors, y_coors)   /* for coord. records */
	FILE *file ;
	int num ;
	double *x_coors ;
	double *y_coors ;
{
	int num_printed ;
	int at_num ;
/*-------------------------------------------------------RLG---*/
	double A1, A2, A3, A4;
	double X,Y ;
      
/*      set up coefficent parameters for conversion to ground 
             coordinates */
	A1 = *(coeff_params) ;
	A2 = *(coeff_params + 1) ;
	A3 = *(coeff_params + 2) ;
	A4 = *(coeff_params + 3) ;
/*-------------------------------------------------------RLG---*/

	num_printed = 0 ;
	for (at_num=0; at_num<num; at_num++)
	{
		if (! (num_printed % 3) )
			if (num_printed)
				fprintf(file,"\n") ;
/*-------------------------------------------------------RLG---*/
		X = (A1 * x_coors[at_num] + A2 * y_coors[at_num] + A3);
		Y = (A1 * y_coors[at_num] + A2 * x_coors[at_num] + A4);
/*-------------------------------------------------------RLG---*/
		fprintf(file,"%12.2lf%12.2lf",X,Y) ;
		num_printed++ ;
	}
	fprintf(file,"\n") ;
}

write_double(file, num, buf)
	FILE *file ;
	int num ;
	double buf[] ;
{
	for(;;)
	{
		switch (num)
		{
		case 0:
			return(0) ;
		case 1:
			fprintf(file,"%12.2lf\n",
				*(buf+0)) ;
			return(0) ;
		case 2:
			fprintf(file,"%12.2lf%12.2lf%\n",
				*(buf+0), *(buf+1)) ;
			return(0) ;
		case 3:
			fprintf(file,"%12.2lf%12.2lf%12.2lf%\n",
				*(buf+0), *(buf+1), *(buf+2)) ;
			return(0) ;
		case 4:
			fprintf(file,"%12.2lf%12.2lf%12.2lf%12.2lf%\n",
				*(buf+0), *(buf+1), *(buf+2), *(buf+3)) ;
			return(0) ;
		case 5:
			fprintf(file,"%12.2lf%12.2lf%12.2lf%12.2lf%12.2lf%\n",
				*(buf+0), *(buf+1), *(buf+2), *(buf+3), *(buf+4)) ;
			return(0) ;
		default:
			fprintf(file,"%12.2lf%12.2lf%12.2lf%12.2lf%12.2lf%12.2lf\n",
				*(buf+0), *(buf+1), *(buf+2), *(buf+3), *(buf+4), *(buf+5)) ;
			num -= 6 ;
			buf += 6 ;
			break ;
		}
	}
}

_put_dtype(buff, value, width, deci)
	char *buff ;
	double *value ;
	int width ;
	int deci ;
{
	char form[64] ;
	char *strchr() ;

	if (*value == 0.0)
	{
		sprintf(buff,"%6.1lf",*value) ;
	}
	else
	{
		sprintf(form,"%%%d.%dle",width, deci) ;
		sprintf(buff,form,*value) ;
		*(strchr(buff,'e')) = 'D' ;
		return ;
	}
}
