/* 
* 
* 4/2002:
* merged select_vNsds.c from NASA into this code. Markus Neteler
*
* HDF docs: http://hdf.ncsa.uiuc.edu/training/HDFtraining/RefManual
*           http://hdf.ncsa.uiuc.edu/training/class/sd/sdview.html
*           http://www.ece.arizona.edu/~rojas/hdf/UG_Book.LOP.html
* Read Data Example:
*           http://hdf.ncsa.uiuc.edu/training/class/sd/ho_rd_ex.html
*
* MODIS2QKM format (250m): http://mcstweb.gsfc.nasa.gov/documents/MOD02QKM-fs.txt
*
*IDL example (see hints for slice there):
* ftp://acdisx.gsfc.nasa.gov/data/modis/tools/hdf/geoview/geov.pro
**************************************************************************
* select_vNsds.c (public domain from NASA)
*
* http://daac.gsfc.nasa.gov/CAMPAIGN_DOCS/MODIS/software.shtml
* http://hdf.ncsa.uiuc.edu/tools.html
* -> ftp://acdisx.gsfc.nasa.gov/data/modis/tools/hdf/select_vNsds
*
*    Last modified February 10, 1999
*
*    Program to read in a  HDF-EOS data file, and write out 
*    user-selected SDS arrays and Vdata tables as separate flat binary 
*    files. Metadata (global attributes) are written out in a separate 
*    ascii file with extension .meta
*    Data in output files will be the same number type (float32, int32, 
*    int16, int8, uint8) as the corresponding array in the input (HDF) 
*    file. 
*
*    USAGE :
*    select_vNsds <hdf_file>
*
*    NOTES :
*    1) Please make sure that the buffer arrays are dimensioned properly
*       via the PARAMETER statements
*
*    2) Output filenames automatically created by replacing 'HDF' suffix
*       of input file with selected parameter (SDS) names
*
*************************************************************************
* GRASS 4.x r.in.hdf written by Bill Brown, USA-CERL, NCSA.
* December 28, 1992
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hdf.h"
#include "gis.h"
#include "mfhdf.h"


/*#define DEBUG*/


#define  FIELD_SIZE     1024	/* maximum length of all the field names */
#define  MAX_DIMS         20	/* maximum number of dimensions in sds  */
#define  MAX_SDS         200	/* maximum number of sds  */
#define  MAX_VDATA       200	/* maximum number of vdata  */
#define  IN_TAIL0      "hdf"	/* possible input filename extension */
#define  IN_TAIL1      "HDF"	/* possible input filename extension */


int main(int argc, char *argv[])
{
    struct Option *hdf_file, *rast, *mult;
    struct Flag *shh, *readall;
    CELL *cellbuf;
    char nextfile[200], errbuf[200];
    double mply;

    FILE *meta_fp, *sds_fp, *sds_fpA, *sds_fpB, *vd_fp;

    int32 sd_id, sds_id, dim_id, file_id;
    int32 index, attr_index, sds_idx, dim_index;
    int32 nindex, sds_index[MAX_SDS];

    int32 rank, attributes, n_datasets, n_file_attrs, num_type, count;
    int32 n_records, nparm, temparm, cal_data_type, num_element;
    int32 status, status2, i, j, ifield;

    int32 dim_sizes[MAX_VAR_DIMS];
    int32 start[MAX_DIMS], stride[MAX_DIMS], edges[MAX_DIMS];

    int32 interlace, vdata_size, field_index, field_size, field_order;
    int32 field_type, vdata_idx, vdata_ref, vdata_id;

    int32 vdata_index[MAX_VDATA], total_vref[MAX_VDATA], total_vidx = 0;

    char fields[FIELD_SIZE], attr_name[64], sds_name[64];

    uint8 *vdatabuf;		/* buffer to retrieve the vdata data   */
    char vdata_name[VSNAMELENMAX];	/* buffer to retrieve the vdata name   */

    char *str, *attr_str, *fieldname;
    char *infile, *tempfile, *nstr;
    char outfile[120];

    float64 gain, gain_err, offset, offset_err;

    char8 *c8;
    int8 *i8;
    uint8 *ui8;
    int16 *i16;
    uint16 *ui16, *ui16A, *ui16B;
    int32 *i32;
    uint32 *ui32;
    float *f32;
    float64 *f64;
    int row, col;

    char cmd[1000];
    int bytes, ptr;

    G_gisinit(argv[0]);

    hdf_file = G_define_option();
    hdf_file->key = "input";
    hdf_file->type = TYPE_STRING;
    hdf_file->required = YES;
    hdf_file->multiple = NO;
    hdf_file->description = "HDF file to be converted.";

/* commented: module will keep file names:
    rast = G_define_option();
    rast->key            	   = "output";
    rast->type           	   = TYPE_STRING;
    rast->required     	           = YES;
    rast->gisprompt    		   = "new,cell,raster";
    rast->description  		   = "Name of new raster file.";
*/

/* unused
    mult = G_define_option();
    mult->key                    = "mult";
    mult->type                   = TYPE_DOUBLE;
    mult->required               = NO;
    mult->multiple               = NO;
    mult->description = 
	"Floating point multiplier. (rastfile = (int)(file.hdf * multiplier))";
*/
    shh = G_define_flag();
    shh->key = 'q';
    shh->description = "Run quietly";

    readall = G_define_flag();
    readall->key = 'a';
    readall->description = "Convert ALL data sets in the HDF file";

    if (G_parser(argc, argv))
	exit(-1);

/*
    if(mult->answer)
	mply = atof(mult->answer);
    else mply = 1.0;

    if(!shh->answer)
	fprintf(stderr,"\nmultiplier = %.4lf\n", mply);
*/
    strcpy(nextfile, rast->answer);
    if (Hishdf(hdf_file->answer) == 0)
	G_fatal_error("%s is not a valid HDF file, or file not found\n\n",
		      hdf_file->answer);

/*
 * Open connections to original (input)  HDF file
 */
    file_id = Hopen(hdf_file->answer, DFACC_READ, 0);
    if (file_id == -1)
	G_fatal_error("Error opening input file : %s\n", hdf_file->answer);

/*
 * Open HDF SDS and Vdata Interfaces
 */
    sd_id = SDstart(hdf_file->answer, DFACC_READ);
    if (sd_id == -1) {
	fprintf(stderr, "SDstart failed.\n");
	HEprint(stdout, 0);
	exit(-1);
    }

    status = Vstart(file_id);
    if (status == -1) {
	fprintf(stderr, "Vstart failed.\n");
	HEprint(stdout, 0);
	exit(-1);
    }


/*
 * Generate the prefix for output file 
 */

    infile = strdup(hdf_file->answer);
    nstr = strstr(infile, IN_TAIL0);
    if (nstr == NULL)
	nstr = strstr(infile, IN_TAIL1);

    if (nstr != NULL)
	strcpy(nstr, "");
    else
	strcat(infile, ".");

/*=================================*/

/*
 * Obtain number of SDSs and Global Attributes in input file
 */
    status = SDfileinfo(sd_id, &n_datasets, &n_file_attrs);
    if (status != 0) {
	fprintf(stderr, "SDfileinfo failed. \n");
	exit(-1);
    }

/*
 * List out SDSs contained in input HDF file
 */
    fprintf(stderr, "\n Number of SDS arrays in file: %d\n\n", n_datasets);

    for (index = 0; index < n_datasets; index++) {
	sds_id = SDselect(sd_id, index);

	status =
	    SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &num_type,
		      &attributes);
	if (status != 0) {
	    fprintf(stderr, "SDgetingo failed.\n");
	    HEprint(stdout, 0);
	    exit(-1);
	}

	fprintf(stderr, "   %2d) %-25s   dimensions = ", index + 1, sds_name);
	for (dim_index = 0; dim_index < rank; dim_index++)
	    fprintf(stderr, "%7d", dim_sizes[dim_index]);
	fprintf(stderr, " \n");

	status = SDendaccess(sds_id);
    }


/*
 * Prompt user for desired parameters to be written as binary file 
 */

#ifdef ORIGCODE /* currently we can read only one map at time*/
    fprintf(stderr,
	    "\nEnter total NUMBER of maps to write out or 0 to exit program\n");
    scanf("%d", &nparm);
    if (nparm == 0) {
	nindex = 0;
	sds_index[nindex] = 0;
	fprintf(stderr, "\n End of HDF import.\n");
	exit(-1);
    }
    else if (nparm == n_datasets) {
	nindex = 0;
	for (index = 0; index < n_datasets; index++)
	    sds_index[nindex++] = index + 1;
    }
    else {
	fprintf(stderr,
		"\nEnter map(s) numbers from above list separated by white space\n");
	nindex = 0;
	for (index = 0; index < nparm; index++) {
	    scanf("%d", &temparm);
	    if (temparm <= n_datasets)
		sds_index[nindex++] = temparm;
	}
    }
    if (nparm > 0) {
	fprintf(stderr, " The valid parameter numbers you Enter are: ");
	for (index = 0; index < nindex; index++)
	    fprintf(stderr, " %d ", sds_index[index]);
    }
    else
	fprintf(stderr, " No sds selected -- # of parameters = 0 \n");
#endif

     nparm = 1; /*read one map */
     fprintf(stderr,
		"\nEnter map number from above list:\n");
     nindex = 0;
     for (index = 0; index < nparm; index++) {
	    scanf("%d", &temparm);
	    if (temparm <= n_datasets)
		sds_index[nindex++] = temparm;
     }
    if (nparm > 0) {
	fprintf(stderr, " The valid parameter numbers you Enter are: ");
	for (index = 0; index < nindex; index++)
	    fprintf(stderr, " %d ", sds_index[index]);
    }
    else
	fprintf(stderr, " No sds selected -- # of parameters = 0 \n");


/*
 * Now loop through SDSs and write selected array(s) to binary files
 */
    if (nindex > 0) {
	for (index = 0; index < nindex; index++) {
	    num_element = 1;
	    sds_idx = sds_index[index] - 1;
	    sds_id = SDselect(sd_id, sds_idx);
	    status =
		SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &num_type,
			  &attributes);
	    status =
		SDgetcal(sds_id, &gain, &gain_err, &offset, &offset_err,
			 &cal_data_type);

	    fprintf(stderr, "\n ****************************************\n");
	    fprintf(stderr, "  SDS name  = %s\n", sds_name);
	    fprintf(stderr, "  SDS type  = %4d\n", num_type);
	    fprintf(stderr, "  SDS rank  = %4d\n", rank);
	    fprintf(stderr, "  SDS scale = %f\n", gain);
	    fprintf(stderr, "  SDS dims  = ");
	    for (j = 0; j < rank; j++) {
		fprintf(stderr, "%6d", dim_sizes[j]);
		num_element *= dim_sizes[j];
	    }
	    fprintf(stderr, "\nrank: %i, num_element: %i = %i * %i * %i\n",
		    rank, num_element, dim_sizes[0], dim_sizes[1],
		    dim_sizes[2]);

/* HDF rank:
  A rank 2 dataset is an image read in scan-line order (2D). 
  A rank 3 dataset is a series of images which are read in an image at a time
  to form a volume.
  A rank 4 dataset may be thought of as a series of volumes.

  The "start" array specifies the multi-dimensional index of the starting
  corner of the hyperslab to read. The values are zero based.

  The "edge" array specifies the number of values to read along each
  dimension of the hyperslab.

  The "stride" array allows for sub-sampling along each dimension. If a
  stride value is specified for a dimension, that many values will be
  skipped over when reading along that dimension. Specifying stride = NULL
  in the C interface or stride = 1 in either interface specifies contiguous
  reading of data. If the stride values are set to 0, SDreaddata returns
  FAIL (or -1). No matter what stride value is provided, data is always
  placed contiguously in buffer.
 
  See also:
   http://www.dur.ac.uk/~dcs0elb/au-case-study/code/hdf-browse.c.html
   http://dao.gsfc.nasa.gov/DAO_people/yin/quads.code.html

Dimensions:
http://hdf.ncsa.uiuc.edu/training/UG_Examples/SD/write_slab.c
  dim_sizes[0] = Z_LENGTH;
  dim_sizes[1] = Y_LENGTH;
  dim_sizes[2] = X_LENGTH;

 3D data:
  note that edges[1] is set to 1 to define a 2-dimensional slab parallel to the ZX plane.  

Meaning of start, edges, stride:
http://www.swa.com/meteorology/hdf/tutorial/File_reading.html
YL = 30;
XL = 30;
dims[0] = YL;
 dims[1] = XL;
 start[0] =0;  row start - X axis
 start[1]=0; column start - Y axis
 edges[0] = dims[0];  - number elements to read  - rowmax
 edges[1] = dims[1]; - number elements to read - colmax
 stride[0] = 3;   - skip every 3rd element 
 stride[1] = 1; - no skip with value 1
*/
	    /* initialization */
	    switch (rank) {
	    case 4:
		/* 4Dim: volume-time */
		fprintf(stderr, "found 4D HDF - volume/time arrays \n");
		start[3] = 0;
		stride[3] = 1;
		edges[3] = dim_sizes[3];
		start[2] = 0;	/* 3Dim: slice# */
		stride[2] = 1;
		edges[2] = dim_sizes[2];
		start[0] = 0;
		stride[0] = 1;
		edges[0] = dim_sizes[0];
		start[1] = 0;
		stride[1] = 1;
		break;
	    case 3:
		/* 3Dim: volume */
		fprintf(stderr,
			"found MODIS 2Q product compliant type (3D HDF)\n");
		start[2] = 0;	/* 3Dim: slice# */
		stride[2] = 1;
		edges[2] = dim_sizes[2];
		start[1] = 0;
		stride[1] = 1;
		edges[1] = dim_sizes[1];
		start[0] = 0;
		stride[0] = 1;
		edges[0] = dim_sizes[0];
		break;
	    case 2:
		/* 2Dim: rows/cols */
		fprintf(stderr, "found ASTER compliant type (2D HDF)\n");
		start[1] = 0;
		stride[1] = 1;
		edges[1] = dim_sizes[1];
		start[0] = 0;
		stride[0] = 1;
		edges[0] = dim_sizes[0];
		break;
	    }

#ifdef DEBUG
	    /* check initialization */
	    for (i = 0; i < rank; i++)
		fprintf(stderr, "%i-start: %i  edges: %i stride: %i\n", i,
			start[i], edges[i], stride[i]);
#endif

/*
 * Read HDF sds arrays
 */
	    fprintf(stderr, "Allocating memory for temp file...\n");
	    switch (num_type) {
	    case DFNT_FLOAT32:
		f32 = (float *) malloc(num_element * sizeof(float));
		break;
	    case DFNT_FLOAT64:
		f64 = (float64 *) malloc(num_element * sizeof(float64));
		break;
	    case DFNT_INT8:
		i8 = (int8 *) malloc(num_element * sizeof(int8));
		break;
	    case DFNT_UINT8:
		ui8 = (uint8 *) malloc(num_element * sizeof(uint8));
		break;
	    case DFNT_INT16:
		i16 = (int16 *) malloc(num_element * sizeof(int16));
		break;
	    case DFNT_UINT16:
		if (rank == 2)	/* MODIS 500m, 1km resolution, ASTER etc. */
		    ui16 = (uint16 *) malloc(num_element * sizeof(uint16));
		if (rank == 3) {	/* MODIS 2Q products 250m resolution */
		    ui16 = (uint16 *) malloc(num_element * sizeof(uint16));
		}
		break;
	    case DFNT_INT32:
		i32 = (int32 *) malloc(num_element * sizeof(int32));
		break;
	    case DFNT_UINT32:
		ui32 = (uint32 *) malloc(num_element * sizeof(uint32));
		break;
	    case DFNT_CHAR8:
		c8 = (char8 *) malloc(num_element * sizeof(char8));
		break;
	    default:
		fprintf(stderr, "not valid data type - not implemented\n");
		break;
	    }

	    fprintf(stderr, "Reading HDF data...\n");
	    switch (num_type) {
	    case DFNT_FLOAT32:
		status = SDreaddata(sds_id, start, NULL, edges, f32);
		break;
	    case DFNT_FLOAT64:
		status = SDreaddata(sds_id, start, NULL, edges, f64);
		break;
	    case DFNT_INT8:
		status = SDreaddata(sds_id, start, NULL, edges, i8);
		break;
	    case DFNT_UINT8:
		status = SDreaddata(sds_id, start, NULL, edges, ui8);
		break;
	    case DFNT_INT16:
		status = SDreaddata(sds_id, start, NULL, edges, i16);
		break;
	    case DFNT_UINT16:
		if (rank == 2)	/* MODIS 500m, 1km resolution */
		    status = SDreaddata(sds_id, start, NULL, edges, ui16);
		if (rank == 3)	/* MODIS 2Q products 250m resolution */
		    status = SDreaddata(sds_id, start, NULL, edges, ui16);
		/*status = SDreaddata (sds_id, start, NULL, edges, (VOIDP) data_array); */
		break;
	    case DFNT_INT32:
		status = SDreaddata(sds_id, start, NULL, edges, i32);
		break;
	    case DFNT_UINT32:
		status = SDreaddata(sds_id, start, NULL, edges, ui32);
		break;
	    case DFNT_CHAR8:
		status = SDreaddata(sds_id, start, NULL, edges, c8);
		break;
	    default:
		fprintf(stderr, "not valid data type\n");
		break;
	    }

	    if (status != 0)
		fprintf(stderr, "\n SDreaddata failed on data set %s. \n",
			sds_name);
	    else
		fprintf(stderr, "\n SDreaddata successful\n");

/*
 * Out put sds to binary data file
 */
	    if (rank == 2) {
		/* write one temp file */
		tempfile = strdup(infile);
		strcpy(outfile, tempfile);
		strcat(outfile, sds_name);
		sds_fp = fopen(outfile, "wb");
		if (sds_fp == NULL)
		    fprintf(stderr, "Open output sds file failed. \n");
		/*   fprintf(stderr, "sds file %s. \n", outfile); */
	    }

	    if (rank == 3) {
		/* write two temp files for day and night images*/
		tempfile = strdup(infile);
		strcpy(outfile, tempfile);
		strcat(outfile, sds_name);
		strcat(outfile, "A");
		sds_fpA = fopen(outfile, "wb");
		if (sds_fpA == NULL)
		    fprintf(stderr, "Open output sds fileA failed. \n");
		/*  fprintf(stderr, "sds file %s. \n", outfile); */

		tempfile = strdup(infile);
		strcpy(outfile, tempfile);
		strcat(outfile, sds_name);
		strcat(outfile, "B");
		sds_fpB = fopen(outfile, "wb");
		if (sds_fpB == NULL)
		    fprintf(stderr, "Open output sds fileB failed. \n");
		/*  fprintf(stderr, "sds file %s. \n", outfile); */
	    }

	    switch (num_type) {
		/* see numbers in
		   4.1r5-linux/include/hntdefs.h
		 */
	    case DFNT_FLOAT32:
		status = fwrite(f32, num_element * sizeof(float), 1, sds_fp);
		break;
	    case DFNT_FLOAT64:
		status =
		    fwrite(f64, num_element * sizeof(float64), 1, sds_fp);
		break;
	    case DFNT_INT8:
		status = fwrite(i8, num_element * sizeof(int8), 1, sds_fp);
		break;
	    case DFNT_UINT8:	/* 21 */
		status = fwrite(ui8, num_element * sizeof(uint8), 1, sds_fp);
		bytes = 1;
		break;
	    case DFNT_INT16:
		status = fwrite(i16, num_element * sizeof(int16), 1, sds_fp);
		break;
	    case DFNT_UINT16:	/* 23 - MODIS, ASTER */

#ifdef DEBUG
		for (row = 0; row < edges[0]; row++)
		    for (col = 0; col < edges[1]; col++)
			fprintf(stdout, "row: %d col: %d val: 0x%2x\n", row, col,
				ui16[col]);
#endif
		/* write 1st half of array = night image */
		status =
		    fwrite(ui16, num_element * sizeof(uint16) / 2, 1,
			   sds_fpA);

		/* write 2nd half of array = day image */
		ptr = ui16 + num_element / 2;
		fprintf(stderr, "ui16 %d ptr %d sizeof(ui16) %d \n", ui16,
			ptr, sizeof(ui16));
		status =
		    fwrite(ui16 + num_element / 2,
			   num_element * sizeof(uint16) / 2, 1, sds_fpB);
		bytes = 2;
		break;
	    case DFNT_INT32:
		status = fwrite(i32, num_element * sizeof(int32), 1, sds_fp);
		break;
	    case DFNT_UINT32:
		status =
		    fwrite(ui32, num_element * sizeof(uint32), 1, sds_fp);
		break;
	    case DFNT_CHAR8:
		status = fwrite(c8, num_element * sizeof(c8), 1, sds_fp);
		break;
	    default:
		fprintf(stderr, "not valid data type\n");
		break;
	    }

	    if (status == 0)
		fprintf(stderr, "\n fwrite failed on data set %s. \n",
			sds_name);
	    else
		fprintf(stderr,
			"\n Binary temp file successfully created.\n");

	    if (rank == 3) {
		fclose(sds_fpA);
		fclose(sds_fpB);
	    }
	    else
		fclose(sds_fp);

	    G_free(tempfile);

	    if (rank == 3) {
		fprintf(stderr, "Importing 1st HDF into GRASS....\n");
		tempfile = strdup(infile);
		strcpy(outfile, tempfile);
		strcat(outfile, sds_name);
		strcat(outfile, "A");
		sprintf(cmd, "r.in.bin in=%s out=%s r=%i c=%i bytes=%i",
			outfile, outfile, dim_sizes[rank - 2],
			dim_sizes[rank - 1], bytes);
		G_system(cmd);
		/* coltable */
		fprintf(stderr, "Applying Grey.eq color table...\n");
		sprintf(cmd, "g.region save=%s.tmpreg.%s; g.region rast=%s;\
                      r.colors %s col=grey.eq;\
                      g.region region=%s.tmpreg.%s; g.remove region=%s.tmpreg.%s > /dev/null ", outfile, outfile, outfile, outfile, outfile, outfile, outfile, outfile);
		G_system(cmd);

		/* delete interim binary file */
		unlink(outfile);
		fprintf(stderr, "GRASS file successfully created:\n %s\n",
			outfile);

		fprintf(stderr, "Importing 2nd HDF into GRASS....\n");
		tempfile = strdup(infile);
		strcpy(outfile, tempfile);
		strcat(outfile, sds_name);
		strcat(outfile, "B");
		sprintf(cmd, "r.in.bin in=%s out=%s r=%i c=%i bytes=%i",
			outfile, outfile, dim_sizes[rank - 2],
			dim_sizes[rank - 1], bytes);
		G_system(cmd);
		/* coltable */
		fprintf(stderr, "Applying Grey.eq color table...\n");
		sprintf(cmd, "g.region save=%s.tmpreg.%s; g.region rast=%s;\
                      r.colors %s col=grey.eq;\
                      g.region region=%s.tmpreg.%s; g.remove region = %s.tmpreg. % s > /dev / null ", outfile, outfile, outfile, outfile, outfile, outfile, outfile, outfile);
		G_system(cmd);

		/* delete interim binary file - unlink */
		/*   sprintf(cmd, "rm -f %s",outfile);
		   G_system(cmd); */
		fprintf(stderr, "GRASS file successfully created:\n %s\n",
			outfile);

	    }
	    else {		/* single band data */
		/* must be improved for ASTER etc. */
		/* dim_sizes cols and rows */
		fprintf(stderr, "Importing into GRASS....\n");
		sprintf(cmd, "r.in.bin in=%s out=%s r=%i c=%i bytes=%i",
			outfile, outfile, dim_sizes[rank - 2],
			dim_sizes[rank - 1], bytes);
		G_system(cmd);

		/* coltable */
		fprintf(stderr, "Applying Grey.eq color table...\n");
		sprintf(cmd, "g.region save=%s.tmpreg.%s; g.region rast=%s;\
                      r.colors %s col=grey.eq;\
                      g.region region=%s.tmpreg.%s; g.remove region = %s.tmpreg. %s > /dev/null ", 
                                                    outfile, outfile, outfile, outfile, outfile, outfile, outfile, outfile);
		G_system(cmd);

		/* delete interim binary file */
		unlink(outfile);
		fprintf(stderr, "GRASS file successfully created:\n %s\n",
			outfile);
	    }

	    switch (num_type) {
	    case DFNT_FLOAT32:
		free(f32);
		break;
	    case DFNT_FLOAT64:
		free(f64);
		break;
	    case DFNT_INT8:
		free(i8);
		break;
	    case DFNT_UINT8:
		free(ui8);
		break;
	    case DFNT_INT16:
		free(i16);
		break;
	    case DFNT_UINT16:
		free(ui16);
		break;
	    case DFNT_INT32:
		free(i32);
		break;
	    case DFNT_UINT32:
		free(ui32);
		break;
	    case DFNT_CHAR8:
		free(c8);
		break;
	    default:
		fprintf(stderr, "not valid data type\n");
		break;
	    }

	    status = SDendaccess(sds_id);
	    strcpy(outfile, "");

	}			/* end of for loop for sds */
    }				/* end of if statement for nindex */

    status = SDend(sd_id);
    fprintf(stderr, " \n\n");


/*=================================*/

/*
 * Now read in the Vdata tables and write non-dimension Vdatas to
 * separate output files
 */
    vdata_ref = -1;
    vdata_ref = VSgetid(file_id, vdata_ref);
    if (vdata_ref == -1) {
	fprintf(stderr, "\n No Vdatas found in data set\n");
	exit(-1);
    }

    fprintf(stderr,
	    "\n\n  List of Vdata name:               (rec_num   rec_size)\n\n");
    while (vdata_ref != -1) {
	vdata_id = VSattach(file_id, vdata_ref, "r");

	status = VSinquire(vdata_id, &n_records, &interlace,
			   fields, &vdata_size, vdata_name);

	if (status != 0) {
	    fprintf(stderr, "\n VSinquire failed on vdata %s\n", vdata_name);
	    exit(-1);
	}

	if (strstr(vdata_name, "fakeDim") == NULL &&
	    strstr(vdata_name, "offset") == NULL &&
	    strstr(vdata_name, "factor") == NULL) {
	    total_vref[total_vidx++] = vdata_ref;
	    fprintf(stderr, " %2d)  %-30s   %5d     %5d \n", total_vidx,
		    vdata_name, n_records, vdata_size);
	}

	status = VSdetach(vdata_id);
	vdata_ref = VSgetid(file_id, vdata_ref);

    }

/*
 * Prompt user for desired vdata to be written as binary file
 */

    fprintf(stderr,
	    "\nEnter total number of vdata to write out or 0 to exit program\n");
    scanf("%d", &nparm);
    if (nparm == 0) {
	nindex = 0;
	vdata_index[nindex] = 0;
	fprintf(stderr, "\n End of HDF import.\n");
	exit(-1);
    }
    else if (nparm == total_vidx) {
	nindex = 0;
	for (index = 0; index < total_vidx; index++)
	    vdata_index[nindex++] = index + 1;
    }
    else {
	fprintf(stderr,
		"\nEnter parameter numbers from above list separated by white space\n");
	nindex = 0;
	for (index = 0; index < nparm; index++) {
	    scanf("%d", &temparm);
	    if (temparm <= total_vidx)
		vdata_index[nindex++] = temparm;
	}
    }

    if (nparm > 0) {
	fprintf(stderr, "The valid vdata numbers you Enter are: ");
	for (index = 0; index < nindex; index++)
	    fprintf(stderr, " %d ", vdata_index[index]);
    }
    else
	fprintf(stderr, " No vdata selected -- # of vdata = 0 \n");


/*
 * Now loop through vdatas and write selected data(s) to binary files
 */

    if (nindex > 0) {

	for (index = 0; index < nindex; index++) {
	    vdata_idx = vdata_index[index] - 1;
	    vdata_id = VSattach(file_id, total_vref[vdata_idx], "r");

	    strcpy(fields, "");
	    status = VSinquire(vdata_id, &n_records, &interlace,
			       fields, &vdata_size, vdata_name);

	    if (status != 0) {
		fprintf(stderr, "\n VSinquire failed on vdata %s\n",
			vdata_name);
		exit(-1);
	    }

	    fprintf(stderr,
		    "\n\n *************************************************\n");
	    fprintf(stderr, "   Vdata name     :   %s\n", vdata_name);
	    fprintf(stderr, "   Vdata recs     :   %d\n", n_records);
	    fprintf(stderr, "   Vdata recsize  :   %d\n", vdata_size);

	    status = VSsetfields(vdata_id, fields);

	    vdatabuf = (uint8 *) malloc(n_records * vdata_size);
	    status = VSread(vdata_id, vdatabuf, n_records, FULL_INTERLACE);

	    if (status != n_records)
		fprintf(stderr,
			"\n Vdata recs read and num_rec do not match");

	    ifield = VFnfields(vdata_id);

	    fprintf(stderr, "   Field Name list: \n");
	    for (field_index = 0; field_index < ifield; field_index++) {
		fieldname = VFfieldname(vdata_id, field_index);
		field_size = VFfieldisize(vdata_id, field_index);
		field_order = VFfieldorder(vdata_id, field_index);
		field_type = VFfieldtype(vdata_id, field_index);
		fprintf(stderr, "         %-20s  (size: %4d, type: %3d )\n",
			fieldname, field_size, field_type);
	    }

/*
 * Now write out the Vdata information to a binary file
 */

	    tempfile = strdup(infile);
	    strcpy(outfile, tempfile);
	    strcat(outfile, vdata_name);
	    vd_fp = fopen(outfile, "wb");
	    if (vd_fp == NULL)
		fprintf(stderr, "Open vdata file failed  %s\n", vdata_name);
	    status =
		fwrite(vdatabuf, sizeof(uint8), n_records * vdata_size,
		       vd_fp);
	    if (status == 0)
		fprintf(stderr, "\n fwrite failed on vdata %s. \n",
			vdata_name);
	    else
		fprintf(stderr, "\n Binary file successfully created : %s\n",
			outfile);


	    fclose(vd_fp);

	    status = VSdetach(vdata_id);


	}			/* end of for statement for index of total selected vdata */

    }				/* end of if statement for nindex */

    fprintf(stderr, "\n\n");
    status = Vend(file_id);
    status = Hclose(file_id);

}
