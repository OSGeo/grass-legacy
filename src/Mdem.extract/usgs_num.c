/* %W% %G% */
#include "usgs.h"

get_int(num)
int *num;
{
	int  skip;
	int n;
	char str1[1024],str2[7];

	*str1 = 0;
	*str2 = 0;

	sscanf(buffer,"%1023[ ]%6s",str1,str2);
	sscanf(str2,"%d",&n);

	skip = strlen(str1) + strlen(str2);

	if((buffer + skip) >= buf_end){
		buffer = buf_start;
		*str1 = 0;
		*str2 = 0;
		if(filestat = get_buf())
		{	
			sscanf(buffer,"%1023[ ]%6s",str1,str2);
			sscanf(str2,"%d",&n);
			skip = strlen(str1) + strlen(str2);
		}
	}

	*num = n;
	return(skip);
}

get_dfloat(num)
float *num;
{
	int  i,exp,skip;
	char str1[1023],str2[25];
	double x;

	*str1 = 0;
	*str2 = 0;

	sscanf(buffer,"%1023[ ]%24s",str1,str2);
	exp = 0;
	sscanf(str2,"%lf%*c%*c%d",&x,&exp);

	skip = strlen(str1) + strlen(str2);
	if((buffer + skip) >= buf_end){
		buffer = buf_start;
		*str1 = 0;
		*str2 = 0;
		if(filestat = get_buf())
		{	
			sscanf(buffer,"%1023[ ]%24s",str1,str2);
			exp = 0;
			sscanf(str2,"%lf%*c%*c%d",&x,&exp);
			skip = strlen(str1) + strlen(str2);
		}
	}

	for(i = 0; i < exp; i++) x *= 10;
	*num = (float) x ;

	return(skip);
}

get_efloat(num)
float *num;
{
	int		skip;
	double x;
	char	str1[1023],str2[13];

	*str1 = 0;
	*str2 = 0;

	sscanf(buffer,"%1023[ ]%12s",str1,str2);

	skip = strlen(str1) + strlen(str2);

	if((buffer + skip) >= buf_end){
		buffer = buf_start;
		*str1 = 0;
		*str2 = 0;
		if(filestat = get_buf())
		{	
			sscanf(buffer,"%1023[ ]%12s",str1,str2);
			skip = strlen(str1) + strlen(str2);
		}
	}

	sscanf (str2,"%lf",&x);
	*num = (float) x;

	return(skip);
}
