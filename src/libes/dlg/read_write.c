#include <stdio.h>
#define FGET 		fgets(buff,80,file)

int dlg_read_int(
	FILE *file ,
	int num ,
	int **buf )
{
	char *calloc() ;

	*buf = (int *)(calloc(num,sizeof(int))) ;
	if (*buf == NULL)
	{
		fprintf(stderr,"ERROR: insufficient memory\n") ;
		return(-1) ;
	}

	if (fread (*buf, sizeof(**buf), num, file) == -1)
		return(-1) ;
	
	return(0) ;
}

int dlg_write_int (
	FILE *file ,
	int num ,
	int *buf)
{
	if (fwrite (buf, sizeof(*buf), num, file) == -1)
		return(-1) ;
	
	return(0) ;
}

int dlg_write_double(
	FILE *file ,
	int num ,
	double *buf )
{
	if (fwrite (buf, sizeof(*buf), num, file) == -1)
		return(-1) ;
	
	return(0) ;
}
