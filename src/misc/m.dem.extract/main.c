#define MAIN
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"
#include "usgs.h"

int main (int argc, char *argv[])
{
	struct GModule *module;
   int examine_only,new,nrow,hdr_stat,i,first,last;
   int tape_min_elev, tape_max_elev, Unexp_C;
   char temp_buf[300];
   struct Option *opt1, *opt2, *opt3, *opt4, *opt5 ;

   G_gisinit(argv[0]);

   	module = G_define_module();
	module->description =
		"Extracts USGS Digital Elevation Model (DEM) data from "
		"1/2-inch magnetic tape.";

/* check command line for accuracy */

    opt1 = G_define_option() ;
    opt1->key        = "input" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->description= "Input device or file" ;

    opt2 = G_define_option() ;
    opt2->key        = "output" ;
    opt2->type       = TYPE_STRING ;
    opt2->gisprompt  = "new,cell,raster" ;
    opt2->required   = NO ;
    opt2->description= "Output raster map (examine only if none specified)";

    opt3 = G_define_option() ;
    opt3->key        = "blocksize" ;
    opt3->type       = TYPE_INTEGER ;
    opt3->required   = NO ;
    opt3->description= "Blocksize of tape data" ;
    opt3->answer     = "10240";

    opt4 = G_define_option() ;
    opt4->key        = "start" ;
    opt4->type       = TYPE_INTEGER ;
    opt4->required   = NO ;
    opt4->description= "First file to look at" ;
    opt4->answer     = "1";

    opt5 = G_define_option() ;
    opt5->key        = "end" ;
    opt5->type       = TYPE_INTEGER ;
    opt5->required   = NO ;
    opt5->description= "Last file to look at" ;
    opt5->answer     = "9999999";

    if (G_parser(argc, argv))
	    exit(1);

    tapename =  opt1->answer;
    outname = opt2->answer;
    if(sscanf(opt3->answer, "%d", &blocksize) != 1 || blocksize < 1)
    {
	fprintf (stderr, "%d - illegal blocksize\n", blocksize);
	G_usage();
	exit(1);
    }
    if(sscanf(opt4->answer, "%d", &first) != 1 || first < 1)
    {
	fprintf (stderr, "%d - illegal start file\n", first);
	G_usage();
	exit(1);
    }
    if(sscanf(opt5->answer, "%d", &last) != 1 || last < first)
    {
	fprintf (stderr, "%d - illegal end file\n", last);
	G_usage();
	exit(1);
    }

    examine_only = (outname == NULL);

/* initialize variables and buffers */
    
    usgs_init(examine_only);

    tape_max_elev = 0;
    tape_min_elev = 99999;

    Unexp_C = 0;  /* 1 when unexpected C type record was encountered */
    count=1;
/*  loop thru entire tape looking for relevant data */
/*  if any data found, call getgrid to extract it   */ 

    fprintf (stdout,"Reading Elevation Tape...     \n");
    while((count<=last)&&(hdr_stat = get_hdr())) 
    {
/* copy rotated file into raster file */
	if (hdr_stat == -1)
	{
	/*
	    fprintf(stdout, "hdr returns -1 \n");
	*/
	    next_record();
	    record_pos = record_pos + RECORD_SIZE;
	    /* if hdr_stat == -1 then the record read was too samll, so we are inbetween of files */
	    continue;	
	}
	if ((hdr_stat == -2 )||(hdr_stat == -3))
	{
            if((examine_only) &&(hdr_stat==-2))
	    {
	       /* in this case this was a file with wrong data format */
	       /*
	       fprintf(stdout, "hdr returns -2 \n");
	       next_record();
	       record_pos = record_pos + RECORD_SIZE;
	       */
	       hdr_list(stdout);
	       count ++;
	       continue;	
            }
            for(i=1;i<=80;i++) fprintf(stdout, "-");
	    if(hdr_stat!=-3)
	    {
	       fprintf(stdout, "\nthe file with icorrect data format was encountered\n");
	       fprintf(stdout, "most probably there was an incorrect information in the\n");
	       fprintf(stdout, "header record of the previous file .\n");
	       fprintf(stdout, "also the current file can have the wrong format\n");
	       fprintf(stdout, "In this case ask experienced unix user to help you dump\n");
	       fprintf(stdout, "only the correct files from the tape into the disk file\n");
	       fprintf(stdout, "and run m.dem.extract with resulting disk file as an input.\n");
	       fprintf(stdout, "quitting reading the tape\n");
	       break;
	    }
	    fprintf(stdout, "\nWARNING: unexpected C-type record encountered\n");
	    fprintf(stdout, "(See GRASS4.1 manual for detailes)\n");
	    fprintf(stdout,"Trying to recover...\n");
	    Unexp_C = 1;
	    /*
	    next_record();
	    record_pos = record_pos + RECORD_SIZE;
	    */
	    continue;
	}
	if(Unexp_C)
	{
	    fprintf(stdout, "OK! Recovered!\n");
	    Unexp_C = 0;
        }
	if(count<first) 
	{
	    count++;
            skip_file();
	    continue;	
	}
        hdr_list(stdout);

	    if(min_elev < tape_min_elev) tape_min_elev = min_elev;
	    if(max_elev > tape_max_elev) tape_max_elev = max_elev + 0.9999;

            G_format_northing(file_north, temp_buf, G_projection());
	    fprintf(stdout,"file north UTM = %s\n",temp_buf);
            G_format_northing(file_south, temp_buf, G_projection());
	    fprintf(stdout,"file south UTM = %s\n",temp_buf);
            G_format_easting(file_east, temp_buf, G_projection());
	    fprintf(stdout,"file east UTM = %s\n",temp_buf);
            G_format_easting(file_west, temp_buf, G_projection());
	    fprintf(stdout,"file west UTM = %s\n",temp_buf);

        /* (UTM can be in arc seconds)
	if(((file_west<0.)||(file_east<0.))&&(G_projection()!=PROJECTION_LL))
	{
	   G_fatal_error("The data is in lat-lon. Use lat_lon database!");
	   unlink(inf);
	   unlink(of);
	}
	*/

        if(G_window_overlap(&cellhd, file_north, file_south, 
	    file_east,file_west))
	    /*
        if(!((n < (double) (file_south + y_res/2.))||
	 (s > (double) (file_north - y_res/2.))||
	 (w > (double) (file_east - x_res/2.))||
	 (e < (double) (file_west + x_res/2.))))
	 */
        {
	    if(!examine_only)
	    {
	        i = getgrid();
                if(i==0)
                {
                    G_fatal_error("could not find data in correct file");
		    unlink(inf);
                }
            }
	    else skip_file();
	    fprintf(stdout, "(intersects with current geographic region)\n"); 
        }
        else
        {
	    fprintf(stdout, "(outside current geographic region)\n"); 
            skip_file();
        }
	count++;
    }
    close(tapefile);
    for(i=1;i<=80;i++) fprintf(stdout, "-");
if(!examine_only)
{
    close(fd);
       /* make system call to m.rot90 to rotate cell file 90 deg */
          fprintf (stdout,"Making system call to m.rot90\n\n");
          sprintf(command,"m.rot90 input=%s output=%s bpc=%ld rows=%d cols=%d",
              inf,of,sizeof(CELL),cellhd.cols,cellhd.rows);
          fprintf (stdout,"%s\n",command);
          if(system(command))
          {
              G_fatal_error("can't rotate raster file");
	      unlink(inf);
          }
          unlink(inf);

       fprintf (stdout,"copying rotated file to rasterfile %s\n", outname);
       /* open rotated file */
          fd = open(of,0);
          if (fd < 0)
          {
       	perror (of);
	       exit(1);
          }
       /* open new cell file */
   
          if((new = G_open_cell_new(outname)) < 0)
          {
             G_fatal_error("can't create new raster file ");
	     unlink(inf);
          }


          for(nrow = 0; nrow < cellhd.rows; nrow++)
          {
              read(fd,profile_buf,cellhd.cols * sizeof(CELL));
              if(G_put_c_raster_row(new,profile_buf) < 0)
              {
                 G_fatal_error("error while writing to raster file");
		 unlink(inf);
              }
          }

          close(fd);
          unlink(of);
          fprintf (stdout,"CREATING SUPPORT FILES FOR %s\n", outname);
          G_close_cell(new);
   }
   if(examine_only) 
    {
       fprintf(stdout, "tape min elevation:%d  tape max elevation:%d\n", tape_min_elev, tape_max_elev);
    }
   exit(0);
}
