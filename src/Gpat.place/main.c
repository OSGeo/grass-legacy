/****************************************************************/
/*								*/
/*	main.c		in	~/src/Gpat.place		*/
/*								*/
/*	This is the main program for pattern placement.		*/
/*	It takes a binary dlg-3 file of a basic pattern	or	*/
/*	of an already stored land pattern, transforms the	*/
/*	coordinate information according to the translation	*/
/*	and rotation specified on the command line and writes	*/
/*	out the transformed dlg-3 file in binary form.		*/
/*								*/
/****************************************************************/


#include 	<math.h>
#include 	<stdio.h>
#include 	"gis.h"
#include 	"dlg.h"
#define		MAIN
#include 	"command.h"
#define		 USAGE 		"pat_type=name bdlg_out_file=name utm_x=value utm_y=value [rotation=value]"
#define		PI	3.141592654
#define         PUT(x,y)        fwrite(x,y,1,fpw)

char buf[1024];

main(argc,argv) 
     int argc;
     char *argv[];
{

	FILE *fpr,*fpw, *fopen();
								
	struct dlg dlg; 	 /* dlg structure to hold       */
				 /* node, line & area info 	*/

	int stash_away(),set_default_options();
	char *current_mapset;
	double theta;	  /* rotation to be applied in rad. 	*/
 
	int i,j;		/* 	looping variables 	*/
	
	/* vertical axis of pattern initialized to 90 degrees   */
	double angle_vert_axis = PI/2.0;

	/* origin of the pattern initialized to (0.0, 0.0)	*/
	origin_x = 0.0;
	origin_y = 0.0;

	G_gisinit (argv[0]);

	set_default_options();
         
	/* check for correct number of command line arguments	*/
	if (argc != 6)
        {
                fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
                exit(-1) ;
        }

        if(D_parse_command(argc,argv,variables,
					n_variables,stash_away))
        {
         fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
         exit(-1) ;
	}

	/* check to see if the name of the output file is legal	*/
	if (G_legal_filename(bdlg_out_file) < 0)
	{
	sprintf(buf,"Illegal filename for bdlg output file.");
	G_fatal_error(buf);
	exit(1);
	}

	current_mapset= G_mapset();

	/* check to see if  output file has a unique name	*/
	if(G_find_file("landpatterns",
			bdlg_out_file,current_mapset))
	{
	sprintf(buf,"Pattern file exists. Cannot overwrite.");
	G_fatal_error(buf);
	}

	sprintf(buf,"%s/landpatterns/%s",G_gisbase(),pat_type);

        if ( !(fpr = fopen(buf,"r")) )
        {
	/* if pattern type is an already stored land pattern	*/

	/*	check to see if specified land pattern exists	*/
	if(G_find_file("landpat_info",pat_type,current_mapset)
	&& G_find_file("landpatterns",pat_type,current_mapset))
	  {

	G__file_name(buf,"landpat_info","",current_mapset);
        sprintf(buf,"%s/%s",buf,pat_type);

	fpr = fopen(buf,"r");
	if(fpr == NULL){
          sprintf(buf, " Cannot open landpattern info file. ");
          G_fatal_error(buf) ;
				    }
	else {
	/*      read the previously stored landpattern info   	*/
        fscanf(fpr,"%lf %lf %lf",
			&angle_vert_axis, &origin_x, &origin_y); 
	fclose(fpr); 
	      }
	  }
	else 
	  {
		sprintf(buf,"Landpattern file does not exist.");
                G_fatal_error(buf) ;
	  }


	/* open the landpattern file in user mapset for reading */ 
	G__file_name(buf,"landpatterns","",current_mapset);
	sprintf(buf,"%s/%s",buf,pat_type);
	
	if(! (fpr = fopen(buf,"r"))){
          sprintf(buf, " Cannot open landpattern file. ");
          G_fatal_error(buf) ;
				    }

        }

	/*	read in the dlg header information		*/
        if (dlg_init(fpr, &dlg) == -1)
        {
           fclose(fpr) ;
           G_fatal_error("Problem in initial read of dlg file") ;  
	 }

	/* read in address info about nodes, areas and lines	*/
	if (dlg_read(fpr,&dlg) == -1)
	{
		fclose(fpr) ;
                G_fatal_error("Problem in reading of dlg file") ;
         }

	/*  setup information for the desired transformation	*/
	translation_x = utm_x - origin_x;
	translation_y = utm_y - origin_y;
	theta	  = rotation * PI /180.0;
	cos_theta = cos(theta);
	sin_theta = sin(theta);

	/*	open a new binary dlg file for writing		*/
	fpw = G_fopen_new("landpatterns",bdlg_out_file);
	if (fpw == NULL) {
	sprintf(buf, "Cannot open bdlg file in database element 'landpatterns' for writing");
	G_fatal_error(buf);
	}

	/* in  'struct dlg_coors' , transformation of boundary  */
        for(i=0; i<4; i++)
        {
        trans_coors(&(dlg.coors.utm_e[i]),&(dlg.coors.utm_n[i]));
        }

	/* 	write header information into new file		*/
	if(dlg_write_header(fpw, &dlg) == -1)
	{
	fclose(fpw);
	G_fatal_error("Writing of header to output file failed");
	}

	/*	transformation of coordinates for nodes		*/
	for (i=1; i<= dlg.max_nodes ; i++)
	{
	dlg_read_node(fpr,&dlg,i);
	trans_coors(&(dlg.node.x), &(dlg.node.y));  

	PUT("N", sizeof(char));
        PUT( &i , sizeof(i));

	dlg_write_node(fpw,&dlg,i);
	}

	/*	transformation of coordinates for areas		*/
	for (i=1;i<= dlg.max_areas ; i++)
        {
	dlg_read_area(fpr,&dlg,i);
        trans_coors(&(dlg.area.x), &(dlg.area.y));

	PUT("A", sizeof(char));
        PUT(&i , sizeof(i));
        dlg_write_area(fpw,&dlg,i);
	}

	/*	transformation of coordinates for lines		*/
	for (i=1; i<= dlg.max_lines ; i++)
	{
	dlg_read_line(fpr,&dlg,i);


	/*	loop over coordinates				*/
	for(j=0;j<dlg.line.n_coors;j++){  

	trans_coors(dlg.line.coors+2*j,dlg.line.coors + 2*j + 1);
                        
					}


	/* find the bounding N,S,W,E of the transformed line	*/

	/* 	initialize	*/
	dlg.line.N = *(dlg.line.coors + 1);
	dlg.line.S = dlg.line.N;
	dlg.line.W = *dlg.line.coors;
	dlg.line.E = dlg.line.W;

	/*	search for limits	*/
	for(j=1;j<dlg.line.n_coors;j++){
	
	if(*(dlg.line.coors+2*j+1)>dlg.line.N)
		dlg.line.N = *(dlg.line.coors+2*j+1); 

	if(*(dlg.line.coors+2*j+1)<dlg.line.S)
		dlg.line.S = *(dlg.line.coors+2*j+1);

	if(*(dlg.line.coors+2*j)>dlg.line.E)
		dlg.line.E = *(dlg.line.coors+2*j);

	if(*(dlg.line.coors+2*j)<dlg.line.W)
		dlg.line.W = *(dlg.line.coors+2*j);

					}	
	PUT("L", sizeof(char));
        PUT(&i , sizeof(i));
	
	dlg_write_line(fpw,&dlg,i);
	}

	fclose(fpr);
	fclose(fpw);

	/* write out the incl. of vert. axis and origin location*/
	G__file_name(buf,"landpat_info","",current_mapset);
	sprintf(buf,"%s/%s",buf,bdlg_out_file);
	fpw = fopen(buf,"w");
	fprintf(fpw,"%lf %lf %lf",
	   angle_vert_axis+theta, utm_x, utm_y);
	fclose(fpw);

}

/************** END OF FUNCTION "MAIN" **************************/
