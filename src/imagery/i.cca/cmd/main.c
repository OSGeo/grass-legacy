/***** Canonical Component Transformation of Satellite Data *****/
/*

             Center for Space Research
             WRW 402
             University of Texas
             Austin, TX 78712-1085

             (512) 471-6824

*/

#define MAIN

#include <string.h>
#include <math.h>
#include "imagery.h"
#include "globals.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
        /* Global variable & function declarations */

        int convert();        /* Convert character to numeric equivalent */
        int i,j,k;             /* Loop control variables */
        int rows,cols;        /* Number of rows & columns */
        int bands;            /* Number of image bands */
        int NN;               /* Total number of data points */
        int nclass;           /* Number of classes */
        int samptot;          /* Total number of sample points */
        double *DATA[MX];       /* Data structure containing image data values */
        double mu[MC][MX];      /* Mean vector for image classes */
        double w[MX][MX];       /* Within Class Covariance Matrix */
        double p[MX][MX];       /* Between class Covariance Matrix */
        double l[MX][MX];       /* Diagonal matrix of eigenvalues */
        double q[MX][MX];       /* Transformation matrix */
        double cov[MC][MX][MX]; /* Individual class Covariance Matrix */
        double nsamp[MC];       /* Number of samples in a given class */
        double eigval[MX];      /* Eigen value vector */
        double eigmat[MX][MX];  /* Eigen Matrix */
        char tempbuf[100], tempname[50];

        /* used to make the color tables */
        CELL outbandmax[MX];  /* will hold the maximums found in the out maps */
        CELL outbandmin[MX];  /* will hold the minimums found in the out maps */
        struct Colors color_tbl;
        struct Signature sigs;
        FILE *sigfp;
        struct Ref refs;
        int datafds[MX];
        int outfds[MX];

        int save_args();
        int error = 0 ;
		struct GModule *module;
        struct Option *opt1, *opt2, *opt3, *opt4 ;

        /***** Start of main *****/
        G_gisinit(argv[0]);

		module = G_define_module();
		module->description =
			"Canonical components analysis (cca) "
			"program for image processing.";

        opt1 = G_define_option() ;
        opt1->key        = "group";
        opt1->type       = TYPE_STRING;
        opt1->required   = YES;
        opt1->description= "Imagery files group" ;

        opt2 = G_define_option() ;
        opt2->key        = "subgroup";
        opt2->type       = TYPE_STRING;
        opt2->required   = YES;
        opt2->description= "Imagery files subgroup" ;

        opt3 = G_define_option() ;
        opt3->key        = "signature";
        opt3->type       = TYPE_STRING;
        opt3->required   = YES;
        opt3->description= "Ascii file containing spectral signatures" ;

        opt4 = G_define_option() ;
        opt4->key        = "output";
        opt4->type       = TYPE_STRING;
        opt4->required   = YES;
        opt4->description= "Output raster file prefix name" ;

        if (G_parser(argc, argv) < 0)
                exit(-1);

        if (G_legal_filename(opt1->answer)<0)
        {
                G_warning( "Warning: illegal group name");
                error++ ;
        }
        else
                strcpy(groupname, opt1->answer);

        if (G_legal_filename(opt2->answer)<0)
        {
                G_warning( "Warning: illegal subgroup name");
                error++ ;
        }
        else
                strcpy(subgroup, opt2->answer);

        if (G_legal_filename(opt3->answer)<0)
        {
                G_warning( "Warning: illegal signature file name");
                error++ ;
        }
        else
                strcpy(signame, opt3->answer);

        if (G_legal_filename(opt4->answer)<0)
        {
                G_warning( "Warning: illegal output file name");
                error++ ;
        }
        else
                strcpy(outputfile, opt4->answer);

        if(error)
                exit(-1) ;

        /* check group, subgroup */
        I_init_group_ref(&refs);
        if (I_find_group(groupname) <=0) {
                G_fatal_error("Unknown imagery group.");
        }
        if (I_get_subgroup_ref(groupname, subgroup, &refs) <= 0) {
                G_fatal_error("Unable to find subgroup reference information.");
        }

        /* open and input the signatures file */
        if ((sigfp = I_fopen_signature_file_old(groupname, subgroup, signame))
            == NULL)
                G_fatal_error("Unable to open the signature file.");
        I_init_signatures(&sigs, refs.nfiles);
        if (I_read_signatures(sigfp, &sigs)<0)
                G_fatal_error("Error while reading the signatures file.");
        fclose(sigfp);
        nclass = sigs.nsigs;
        if (nclass<2)
                G_fatal_error("Need at least two signatures in signature file.");

        /* check the number of input bands */
        bands = refs.nfiles;
        if (bands > MX-1) {
                sprintf(tempbuf,"Subgroup too large.  Maximum number of bands is%d\n",
                    MX-1);
                G_fatal_error(tempbuf);
        }

        /* check output file */
        if (outputfile[0] == '\0')
                G_fatal_error("An output cell map name is required.");
        if (strlen(outputfile) >=13)
                G_fatal_error("The output cell map name can not be longer than 12 characters.");

        rows = G_window_rows();
        cols = G_window_cols();
        NN = rows * cols;

        /*
    Here is where the information regarding
      a) Number of samples per class
      b) Covariance matrix for each class
    are read into the  program
    */

        samptot = 0;
        for (i=1; i<=nclass; i++) {
                nsamp[i] = sigs.sig[i-1].npoints;
                samptot += nsamp[i];
                for (j=1; j<=bands; j++) {
                        mu[i][j] = sigs.sig[i-1].mean[j-1];
                        for (k=1; k<=j; k++)
                                cov[i][j][k] = cov[i][k][j] = sigs.sig[i-1].var[j-1][k-1];
                }
        }



        within(samptot,nclass,nsamp,cov,w,bands);
        between(samptot,nclass,nsamp,mu,p,bands);
        jacobi(w,bands,eigval,eigmat);
        egvorder(eigval,eigmat,bands);
        setdiag(eigval,bands,l);
        getsqrt(w,bands,l,eigmat);
        solveq(q,bands,w,p);
        jacobi(q,bands,eigval,eigmat);
        egvorder(eigval,eigmat,bands);
        matmul(q,eigmat,w,bands);

        /* open the cell maps */
        for (i=1; i<=bands; i++) {
                outbandmax[i] = (CELL) 0;
                outbandmin[i] = (CELL) 0;
                if ((datafds[i]=G_open_cell_old(refs.file[i-1].name,
                    refs.file[i-1].mapset)) < 0) {
                        sprintf(tempbuf, "Unable to open cell map <%s> for input.",
                            refs.file[i-1].name);
                        G_fatal_error(tempbuf);
                }
                sprintf(tempname, "%s.%d", outputfile, i);
                if ((outfds[i]=G_open_cell_new(tempname)) < 0) {
                        sprintf(tempbuf, "Unable to open cell map <%s> for output.",
                            tempname);
                        G_fatal_error(tempbuf);
                }
        }

        /* do the transform */
        transform(datafds,outfds,rows,cols,q,bands,outbandmin,outbandmax);

        /* make grey scale color table */
        G_init_colors(&color_tbl);

        /* close the cell maps */
        for (i=1; i<=bands; i++) {
                G_close_cell(datafds[i]);
                G_close_cell(outfds[i]);

                if (outbandmin[i] < (CELL) 0 || outbandmax[i] > (CELL) 255) {
                        sprintf(tempbuf,
                            "The output cell map %s.%d has values outside the 0-255 range.",
                            outputfile, i);
                        G_warning(tempbuf);
                }
                G_make_grey_scale(&color_tbl, 0, outbandmax[i]);
                sprintf(tempbuf, "%s.%d", outputfile, i);
                /* write a color table */
                G_write_colors(tempbuf, G_mapset(), &color_tbl);
        }


        I_free_signatures(&sigs);
        I_free_group_ref(&refs);
        freemem(DATA,bands);

	exit(0);
}
