/*  @(#)read_write.c	2.1  6/26/87  */
#include <stdio.h>
#include "dlghead.h"

#define FGET 		fgets(buff,128,file)


#define CHAR_NULL 		'\0'

read_int(file, num, buf)
	FILE *file ;
	int num ;
	int buf[] ;
{
	char buff[128] ;
	int tmp[12] ;
	int nassign ;
	int n_read ;
	int todo ;
	int i ;
	int error ;

	error = 0 ;
	nassign = 0 ;

	while (nassign < num)
	{
		FGET ;
		todo = num - nassign ;
		todo = (todo < 12) ? todo : 12 ;
		n_read = sscanf(buff,"%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d%6d",
			tmp+0, tmp+1, tmp+2, tmp+3, tmp+4, tmp+5, 
			tmp+6, tmp+7, tmp+8, tmp+9, tmp+10, tmp+11) ;
		for(i=0; i<n_read; i++)
			buf[nassign++] = *(tmp+i) ;
		if (n_read != todo)
			return (num - nassign ) ;
	}
	return(error) ;
}

write_int (file, num, buf)
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

read_doubles(file, num, buf)
	FILE *file ;
	int num ;
	double buf[] ;
{
	char buff[128] ;
	double tmp[6] ;
	double tmp1[6] ;
	int nassign ;
	int n_read ;
	int todo ;
	int i ;
	int error ;

	error = 0 ;
	nassign = 0 ;

	while (nassign < num)
	{
		FGET ;
		todo = num - nassign ;
		todo = (todo < 6) ? todo : 6 ;

		n_read = scan_doubles( todo, buff, tmp) ;


/**  notice that the case statements have no 'break;' and are designed to
*	fall thru.   take care of internal coordinates  
**/
		switch(n_read)
		{
			case 6:
			tmp1[5] = tmp[5] * int_params[0]
				-  tmp[4] * int_params[1]
				+  int_params[3] ;
			case 5:
			tmp1[4] = tmp[4] * int_params[0]
				+  tmp[5] * int_params[1]
				+  int_params[2] ;

			case 4:
			tmp1[3] = tmp[3] * int_params[0]
				-  tmp[2] * int_params[1]
				+  int_params[3] ;

			case 3:
			tmp1[2] = tmp[2] * int_params[0]
				+  tmp[3] * int_params[1]
				+  int_params[2] ;

			case 2:
			tmp1[1] = tmp[1] * int_params[0]
				-  tmp[0] * int_params[1]
				+  int_params[3] ;

			case 1:
			tmp1[0] = tmp[0] * int_params[0]
				+  tmp[1] * int_params[1]
				+  int_params[2] ;

			default:
				break ;

		}  /**  switch  **/


		for(i=0; i<n_read; i++)
			buf[nassign++] = *(tmp1+i) ;
		if (n_read != todo)
			return (num - nassign ) ;
	}
	return(error) ;
}


static scan_doubles( todo, coor_str, tmp )
	int  todo ;
	char *coor_str ;
	double  tmp[] ;
{

	int i ;
	int n_read ;
	char coors[6][13] ;

	double  atof() ;

/**************
  This sscanf fails on coordinates with no white space between them.
	n_read = sscanf(buff,"%12lf %12lf %12lf %12lf %12lf %12lf",
		tmp+0, tmp+1, tmp+2, tmp+3, tmp+4, tmp+5) ;

  scan_doubles() fixes the problem.
**************/

	n_read = sscanf(coor_str,"%12c%12c%12c%12c%12c%12c",
			coors[0], coors[1], coors[2],
			coors[3], coors[4], coors[5]) ;

/*  how many coordinates get converted  */
	n_read = n_read < todo ? n_read : todo  ;

/*  convert only those coordinates  */
	for ( i = 0; i < n_read; i++)
	{
	/*  make sure the string is null terminated  */
		coors[i][12] = CHAR_NULL ;

/*
printf(" str: '%s',", coors[i]) ;
*/

	/*  convert the coordinate  */
		tmp[i] = atof( coors[i]) ;
/*
printf("    coor: %lf\n", tmp[i]) ;
*/
	}

	return(n_read) ;

}


write_doubles (file, num, x_coors, y_coors)
	FILE *file ;
	int num ;
	double *x_coors ;
	double *y_coors ;
{
	int num_printed ;
	int at_num ;

	num_printed = 0 ;
	for (at_num=0; at_num<num; at_num++)
	{
		if (! (num_printed % 3) )
			if (num_printed)
				fprintf(file,"\n") ;
		fprintf(file,"%12.2lf%12.2lf",x_coors[at_num], y_coors[at_num]) ;
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

