/* %W% %G% */
#define MAIN
#include "usgs.h"

main(argc,argv) char *argv[];
{
    int		i,t_min_elev,t_max_elev;
    char	*malloc();


/* check command line for accuracy */

   if (!getargs(argc,argv))
   {
       fprintf (stderr, "usage: [if=tapedev] bs=blocksize\n");
       exit(1);
   }
   
/* allocate buffer */

    if(!(buffer = malloc(blocksize + 1024))){
	fprintf(stderr,"buffer could not be allocated\n");
	exit(-1);
    }

/*  open tape drive */

    if((tapefile =  open(tapename,0))== -1)
    {
	G_fatal_error("can't open tape drive");
	exit(1);
    }


    buf_start = buffer;
    buf_end   = buffer + blocksize;

    t_min_elev = 99999;
    t_max_elev = 0;

    while(get_hdr()){
	    if(min_elev < t_min_elev) t_min_elev = min_elev;
	    if(max_elev > t_max_elev) t_max_elev = max_elev + 0.9999;

	    hdr_list(stdout);
	    get_profile();

	    fprintf(stdout,"file north UTM = %f\n",file_north);
	    fprintf(stdout,"file south UTM = %f\n",file_south);
	    fprintf(stdout,"file east UTM = %f\n",file_east);
	    fprintf(stdout,"file west UTM = %f\n",file_west);

	    skip_file();
    }

    fprintf(stdout,"tape min elevation:%d  tape max elevation:%d\n",t_min_elev,
	t_max_elev);

    for(i= 0; i< 80; i++)
	fprintf(stdout,"-") ;

}
