#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#define MAIN
#include "usgs.h"

int 
main (int argc, char *argv[])
{
	struct GModule *module;
    int		i,t_min_elev,t_max_elev, first, last;
    struct {
	struct Option *input, *blocksize, *first, *last;
    } parm;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Provides a terse description of USGS Digital Elevation Model "
		"(DEM) data files stored on 1/2-inch magnetic tape.";

    parm.input = G_define_option() ;
    parm.input->key        = "input" ;
    parm.input->type       = TYPE_STRING ;
    parm.input->required   = YES ;
    parm.input->description= "Tape input device" ;

    parm.blocksize = G_define_option() ;
    parm.blocksize->key        = "blocksize" ;
    parm.blocksize->type       = TYPE_INTEGER ;
    parm.blocksize->required   = NO ;
    parm.blocksize->answer = "10240";
    parm.blocksize->description= "Blocksize of tape data" ;

    parm.first = G_define_option() ;
    parm.first->key        = "first" ;
    parm.first->type       = TYPE_INTEGER ;
    parm.first->required   = NO ;
    parm.first->answer     = "1";
    parm.first->description= "First file to examine" ;

    parm.last = G_define_option() ;
    parm.last->key        = "last" ;
    parm.last->type       = TYPE_INTEGER ;
    parm.last->required   = NO ;
    parm.last->answer     = "999999";
    parm.last->description= "Last file to examine" ;

    if (G_parser(argc,argv))
	exit(1);
    
    tapename = parm.input->answer;

    if(sscanf(parm.blocksize->answer, "%d", &blocksize) != 1 || blocksize < 1)
    {
	fprintf (stderr, "%d - illegal blocksize\n", blocksize);
	G_usage();
	exit(1);
    }
    if(sscanf(parm.first->answer, "%d", &first) != 1 || first < 1)
    {
	fprintf (stderr, "%d - illegal start file\n", first);
	G_usage();
	exit(1);
    }
    if(sscanf(parm.last->answer, "%d", &last) != 1 || last < first)
    {
	fprintf (stderr, "%d - illegal end file\n", last);
	G_usage();
	exit(1);
    }

    if (blocksize <= 0)
    {
	fprintf (stderr, "%s=%s - blocksize must be a postive number\n",
		parm.blocksize->key, parm.blocksize->answer);
	G_usage();
	exit(1);
    }


/* allocate buffer */

    if(!(buffer = G_malloc(blocksize + 1024))){
	fprintf(stderr,"buffer could not be allocated\n");
	exit(-1);
    }

/*  open tape drive */

    if((tapefile =  open(tapename,0))== -1)
    {
	perror (tapename);
	G_fatal_error("can't open tape drive");
	exit(1);
    }

    record_pos = 0;
    buf_start = buffer;
    buf_end   = buffer + blocksize;

    t_min_elev = 99999;

    /* get full window information  */

    G_get_set_window(&cellhd);

    /* adjust borders to correspond to cell centers */

    n = cellhd.north - cellhd.ns_res/2;
    s = cellhd.south + cellhd.ns_res/2;
    e = cellhd.east - cellhd.ew_res/2;
    w = cellhd.west + cellhd.ew_res/2;

    t_max_elev = 0;

    count = 1;
    while((count<=last)&&get_hdr()){
      if(count>=first)
	 {
	    if(min_elev < t_min_elev) t_min_elev = min_elev;
	    if(max_elev > t_max_elev) t_max_elev = max_elev + 0.9999;

	    hdr_list(stdout);
	    get_profile();

	    fprintf(stdout,"file north UTM = %f\n",file_north);
	    fprintf(stdout,"file south UTM = %f\n",file_south);
	    fprintf(stdout,"file east UTM = %f\n",file_east);
	    fprintf(stdout,"file west UTM = %f\n",file_west);

            if(G_window_overlap(&cellhd, file_north, file_south,
	     file_east, file_west))
	      {}
	    else
	     fprintf(stdout,"\n(outside the current geographic region)\n"); 
         }
	 skip_file();
	 count++;
    }

    fprintf(stdout,"tape min elevation:%d  tape max elevation:%d\n",t_min_elev,
	t_max_elev);

    for(i= 0; i< 80; i++)
	fprintf(stdout,"-") ;

    G_free(buffer);

    return 0;
}
