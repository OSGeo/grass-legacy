
#include	<stdio.h>
#include	"dig_atts.h"

main()
{

	char	type ;
	int	stat ;
	int	i, cat ;
	long	offset ;
	double	x,  y ;
	FILE	*fp,  *fopen() ;
	FILE	*fp_out ;

	if ( (fp = fopen("atts_sample", "r"))  ==  NULL)
	{
		fprintf(stderr, "Can't open file for read\n") ;
		exit(-1) ;
	}

	if ( (fp_out = fopen("atts.out", "w"))  ==  NULL)
	{
		fprintf(stderr, "Can't open file for write\n") ;
		exit(-1) ;
	}

/**  start from the beginning of the files  */
	rewind(fp) ;
	rewind(fp_out) ;

/**  read all the attributes, print to screen and write to file **/
	printf("\n\nATTS:\n\n") ;

	while( ! ( stat = read_att (fp, &type, &x, &y, &cat,  &offset)))
	{

		printf(WRITE_ATT_FORMAT, type, x, y, cat) ;
		printf("   offset: %ld \n", offset) ;
		printf("\n") ;

	    /*  write the attribute  */
		if (write_att (fp_out, type, x, y, cat))
		{
			printf("\n Cannot write att file\n") ;
			break ;
		}

	}

	if(stat < 0)
		printf("\n Cannot read attribute file. stat: %d\n", stat) ;
	if(stat > 0)
		printf("\n Finished reading attribute file.  stat: %d\n", stat) ;

	fclose(fp) ;
	fclose(fp_out) ;
}


