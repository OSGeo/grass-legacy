/* 
   parser version by Bob Covill 10/2001

   This is the CMD version of m.proj
   
   Example:   
   m.proj2 inproj="proj=utm,name=utm,a=6378137.0,es=0.006694380,zone=32,\
   unfact=1.0" outproj="proj=tmerc,name=tmerc,a=6377397.155,es=0.0066743720,\
   lat_0=0.0,lon_0=9.0,k=1.0,x_0=3500000.0" input=utm.coord output=new.gk.coord

   will convert file `utm.coord'' in UTM coords to file `new.gk.coord'' in
   Gauss-Krueger coordinates. The input file has to be written in plain
   ASCII format containing row-wise Easting and Northing. For the UTM the
   Easting shall not contain the zone information.
   
   (code borrowed from m.ll2utm)
   
   created by: Irina Kosinovsky 
   changed by: Morten Hulden <morten@ngb.se> October 1999, support for additional projections 
*/

#include <string.h>
#include <unistd.h>
#include "geo.h"
#include "gis.h"
#include "gprojects.h"
#define MAIN
#include "local_proto.h"

int main(int argc, char *argv[])
{
	struct pj_info info_in;
	struct pj_info info_out;
	struct Option *input, *output, *p_in, *p_out;

	int proj_changed_in = 0;
	int proj_changed_out = 0;
	char ebuf[256]="", nbuf[256]="";
	char b1[100]="", b2[100]="", label[512]="";
	char buf[1024]="";

	G_gisinit (argv[0]);

        p_in=G_define_option () ;
        p_in->key        = "inproj";
        p_in->type       = TYPE_STRING;
        p_in->required   = NO;
	p_in->multiple	 = YES;
        p_in->description= "Comma separated input projection parameters" ;

        p_out=G_define_option () ;
        p_out->key        = "outproj";
        p_out->type       = TYPE_STRING;
        p_out->required   = NO;
	p_out->multiple   = YES;
        p_out->description= "Comma separated output projection parameters" ;

	input=G_define_option () ;
	input->key        = "input";
	input->type       = TYPE_STRING;
	input->required   = NO;
	input->description= "Input coordinate file" ;

        output=G_define_option () ;
        output->key        = "output";
        output->type       = TYPE_STRING;
        output->required   = NO;
        output->description= "Output coordinate file" ;

	if (G_parser(argc, argv))
	exit (-1);

	conv_typ = conv_way = 0;
	input_typ = output_typ = 1;

	/* Initialize all the globals here */
	sprintf(proj_name_in, "None");
	sprintf(proj_name_out, "None");
	sprintf(ellps_name_in, "None");
	sprintf(ellps_name_out, "None");

	G_geo_init_table();
	init_used_table();
	

	input_typ = output_typ = 2;

	/* Input Projection */
        /* Get input projection parms */
        if (p_in->answers) {
        int i;
        char ibuf[1024];
        struct Key_Value *in_proj_info, *in_proj_units;
	char *paramkey, *paramvalue;
	   
	in_proj_info = G_create_key_value();
	in_proj_units = G_create_key_value();
	   
        for (i=0; p_in->answers[i]; i++) {
	  sscanf(p_in->answers[i], "%1023s", ibuf);
	  paramkey = strtok(ibuf, "=");
	  paramvalue = ibuf + strlen(paramkey) + 1;
	  if( strcmp(paramkey, "unfact") == 0 )
	    G_set_key_value("meters", paramvalue, in_proj_units);
	  else
	    G_set_key_value(paramkey, paramvalue, in_proj_info);
	}
        if (pj_get_kv(&info_in, in_proj_info, in_proj_units) < 0)
                G_fatal_error("Cannot initialize proj_info_in");
	G_free_key_value( in_proj_info );
	G_free_key_value( in_proj_units );
        } else {
		/* Get intercatively */
		parms_in[0] = '\0';
		proj_changed_in = process(1, parms_in, proj_name_in, proj_title_in, ellps_name_in, &radius_in, USED_in, units_in);
		if (pj_get_string(&info_in, parms_in) < 0)
		G_fatal_error("Cannot initialize proj_info_in");
	}

	/* Output Projection */
	/* Get output projections parms */
	if (p_out->answers) {
		int i;
		char obuf[1024];
	        struct Key_Value *out_proj_info, *out_proj_units;
		char *paramkey, *paramvalue;
	   
		out_proj_info = G_create_key_value();
		out_proj_units = G_create_key_value();
	   
	        for (i=0; p_out->answers[i]; i++) {
		  sscanf(p_out->answers[i], "%1023s", obuf);
		  paramkey = strtok(obuf, "=");
		  paramvalue = obuf + strlen(paramkey) + 1;
		  if( strcmp(paramkey, "unfact") == 0 )
		    G_set_key_value("meters", paramvalue, out_proj_units);
		  else
		    G_set_key_value(paramkey, paramvalue, out_proj_info);
		}
		if (pj_get_kv(&info_out, out_proj_info, out_proj_units) < 0)
        	        G_fatal_error("Cannot initialize proj_info_out");
	        G_free_key_value( out_proj_info );
	        G_free_key_value( out_proj_units );
		} else {
			/* Get interactively */
			parms_out[0] = '\0';
			proj_changed_out = process(2, parms_out, proj_name_out, proj_title_out, ellps_name_out, &radius_out, USED_out, units_out);
			if (pj_get_string(&info_out, parms_out) < 0)
				G_fatal_error("Cannot initialize proj_info_out");
		}
		proj_index_in = G_geo_get_proj_index(proj_name_in);
		proj_index_out = G_geo_get_proj_index(proj_name_out);
                pj_print_proj_params(&info_in, &info_out);

/* BOB start here */

/* Open Input */
    if (input->answer)
    {
        if ((In_file=freopen(input->answer, "r", stdin))==NULL)
        {
            fprintf (stderr, "%s: %s=", G_program_name(), input->key);
            perror (input->answer);
            exit(1);
        }
    }

/* Open Output */
    if (output->answer)
    {
        if ((Out_file=freopen(output->answer, "w", stdout))==NULL)
        {
            fprintf (stderr, "%s: %s=", G_program_name(), output->key);
            perror (output->answer);
            exit(1);
        }
    }
        rec_cnt = 0;

	if (isatty(0)) 
	{
	fprintf (stderr,"\nEnter one input coordinate pair per line (E N)\n");
	fprintf (stderr,"Enter the word <end> when done\n");
	}

	for (;;) {
	   if (get_input(b1,ebuf,b2,nbuf,label) == 0)
		break;
	   rec_cnt++;

	   if (proj_index_in == LL) {
		G_scan_easting (ebuf, &LON, PROJECTION_LL);
		G_scan_northing (nbuf, &LAT, PROJECTION_LL);
	   } else {
		sscanf(ebuf, "%lf", &EAS);
		sscanf(nbuf, "%lf", &NOR);
	   }

	   if (proj_index_in == LL) {
		X = LON;
		Y = LAT;
	   } else {
		X = EAS;
		Y = NOR;
	   }

	/* Convert Coordinates */
	if ((proj_index_in == proj_index_out) && (proj_index_out == LL)) {
		 LON_res = LON;
		 LAT_res = LAT;
		 cur_LAT = LAT;
		 cur_LON = LON;
		} else if (pj_do_proj(&X, &Y, &info_in, &info_out) < 0)
			G_fatal_error("Error in pj_do_proj()");

		if (proj_index_out != LL) {
		 EAS_res = X;
		 NOR_res = Y;
		 cur_LAT = LAT;
		 cur_LON = LON;
		 sprintf (buf, "%s%f%s%f", b1, EAS_res, b2, NOR_res);
		 strcat (buf, label);
		} else {
		 LON_res = X;
		 LAT_res = Y;
		 cur_LAT = LAT_res;
		 cur_LON = LON_res;
		 G_format_easting(LON_res, ebuf, PROJECTION_LL);
		 G_format_northing(LAT_res, nbuf, PROJECTION_LL);
		 sprintf (buf, "%s%s%s%s", b1, ebuf, b2, nbuf);
		 strcat (buf, label);
		}
		/* Write Out Results */
		fprintf(stderr,"%s",buf);
		put_output(buf, (int)output->answer);
		} /* end for loop */

		if (input->answer) fclose(In_file);
		if (output->answer) fclose(Out_file);
		
		return 0;
}


int min1(int a, int b)
{
	if (a < b)
		return a;
	else
		return b;
}
