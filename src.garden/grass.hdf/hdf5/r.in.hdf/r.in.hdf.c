/* 
 * 
 * merged select_vNsds.c 3/2002 Markus Neteler
 *
 * GRASS 4.x r.in.hdf written by Bill Brown, USA-CERL, NCSA.
 * December 28, 1992
 */
 
/*************************************************************************
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
*
*    Data in output files will be the same number type (float32, int32, 
*    int16, int8, uint8) as the corresponding array in the input (HDF) 
*    file. 
*
*
*    USAGE :
*
*    select_vNsds <hdf_file>
*    
*
*    NOTES :
*
*    1) Please make sure that the buffer arrays are dimensioned properly
*       via the PARAMETER statements
*
*    2) Output filenames automatically created by replacing 'HDF' suffix
*       of input file with selected parameter (SDS) names
*
**************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hdf.h"
#include "gis.h"

#define  FIELD_SIZE     1024         /* maximum length of all the field names */
#define  MAX_DIMS         20         /* maximum number of dimensions in sds  */
#define  MAX_SDS         200         /* maximum number of sds  */
#define  MAX_VDATA       200         /* maximum number of vdata  */
#define  IN_TAIL0      "hdf"          /* possible input filename extension */
#define  IN_TAIL1      "HDF"          /* possible input filename extension */


int
main(int argc, char *argv[])
{
   struct Option       *hdf_file, *rast, *mult;
   struct Flag         *shh, *readall;
   CELL                *cellbuf;
   char                nextfile[200], errbuf[200];
   double              mply;
  
   FILE    *meta_fp, *sds_fp, *vd_fp;

   int32   sd_id, sds_id, dim_id, file_id;
   int32   index, attr_index, sds_idx, dim_index;
   int32   nindex, sds_index[MAX_SDS];

   int32   rank, attributes, n_datasets, n_file_attrs, num_type, count;
   int32   n_records, nparm, temparm, cal_data_type, num_element ;
   int32   status, status2, i,j, ifield;

   int32   dim_sizes[MAX_VAR_DIMS];
   int32   start[MAX_DIMS],  stride[MAX_DIMS], edges[MAX_DIMS];

   int32   interlace, vdata_size, field_index, field_size, field_order; 
   int32   field_type, vdata_idx, vdata_ref, vdata_id;

   int32   vdata_index[MAX_VDATA], total_vref[MAX_VDATA], total_vidx=0;

   char    fields[FIELD_SIZE], attr_name[64], sds_name[64];

   uint8   *vdatabuf;                  /* buffer to retrieve the vdata data   */
   char    vdata_name[VSNAMELENMAX];   /* buffer to retrieve the vdata name   */

   char    *str, *attr_str, *fieldname;
   char    *infile, *tempfile, *nstr;
   char    outfile[120];

   float64  gain, gain_err, offset, offset_err;

   char8   *c8;
   int8    *i8;
   uint8   *ui8;
   int16   *i16;
   uint16  *ui16;  
   int32   *i32;
   uint32  *ui32;
   float   *f32;
   float64 *f64;
   
   char    cmd[1000];
   int     bytes;


    G_gisinit (argv[0]);

    hdf_file = G_define_option();
    hdf_file->key                    = "input";
    hdf_file->type                   = TYPE_STRING;
    hdf_file->required               = YES;
    hdf_file->multiple               = NO;
    hdf_file->description            = "HDF file to be converted.";

/* commented: module will kepp file names:
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
    shh = G_define_flag ();
    shh->key = 'q';
    shh->description = "Run quietly";

    readall = G_define_flag ();
    readall->key = 'a';
    readall->description = "Convert ALL data sets in the HDF file";

    if (G_parser (argc, argv))
	exit (-1);
    
/*
    if(mult->answer)
	mply = atof(mult->answer);
    else mply = 1.0;

    if(!shh->answer)
	fprintf(stderr,"\nmultiplier = %.4lf\n", mply);
*/
    strcpy(nextfile, rast->answer);

 
    if (Hishdf(hdf_file->answer) == 0)
       G_fatal_error("%s is not a valid HDF file, or file not found\n\n", hdf_file->answer);


/*
 * Open connections to original (input)  HDF file
 */
     file_id = Hopen (hdf_file->answer, DFACC_READ, 0);
     if ( file_id == -1 )
        G_fatal_error("Error opening input file : %s\n", hdf_file->answer);

 
/*
 * Open HDF SDS and Vdata Interfaces
 */
    sd_id = SDstart(hdf_file->answer, DFACC_READ);
    if (sd_id == -1) {
        printf ("SDstart failed.\n");
        HEprint (stdout, 0);
        exit (-1);
    }

    status = Vstart (file_id);
    if (status == -1 ) {
        printf ("Vstart failed.\n");
        HEprint (stdout, 0);
        exit (-1);
    }
       

/*
 * Generate the prefix for output file 
 */

    infile = strdup(hdf_file->answer);
    nstr = strstr(infile, IN_TAIL0);
    if (nstr == NULL)
        nstr = strstr(infile, IN_TAIL1);

    if(nstr != NULL)
       strcpy(nstr, "");
    else 
       strcat(infile, ".");


/*=================================*/

/*
 * Obtain number of SDSs and Global Attributes in input file
 */

    status = SDfileinfo(sd_id, &n_datasets, &n_file_attrs);
    if (status != 0 ) {
        printf("SDfileinfo failed. \n");
        exit (-1);
    }


/*
 * List out SDSs contained in input HDF file
 */
    printf("\n Number of SDS arrays in file: %d\n\n",n_datasets);

    for (index = 0; index < n_datasets; index++) {
         sds_id = SDselect(sd_id, index);

         status = SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &num_type, &attributes);
         if (status != 0) {        
            printf ("SDgetingo failed.\n");
            HEprint (stdout, 0);
            exit (-1);
         }

         printf("   %2d) %-25s   dimensions = ", index+1, sds_name);
         for(dim_index = 0; dim_index < rank; dim_index++)
             printf("%7d", dim_sizes[dim_index]);
         printf(" \n");

         status = SDendaccess(sds_id);
    }


/*
 * Prompt user for desired parameters to be written as binary file 
 */

    printf("\n Enter total NUMBER of maps to write out or 0 to exit program\n");  
    scanf("%d", &nparm);
    if(nparm == 0) {
       nindex = 0;
       sds_index[nindex] = 0;
        printf("\n End of Slect_vNsds\n");
         exit(-1);
    }
    else if (nparm == n_datasets){
       nindex = 0;
       for (index = 0; index < n_datasets; index++)
           sds_index[nindex++] = index+1;
    }
    else {
       printf("\n Enter map(s) numbers from above list separated by white space\n");  
       nindex = 0;
       for (index = 0; index < nparm; index++) {
           scanf("%d", &temparm);
           if(temparm <= n_datasets)
              sds_index[nindex++] = temparm;
       }
    }

    if(nparm > 0) {
       printf(" The valid parameter numbers you Enter are: "); 
       for (index = 0; index < nindex; index++)
            printf(" %d ", sds_index[index]);
    }
    else
       printf(" No sds selected -- # of parameters = 0 \n");  


/*
 * Now loop through SDSs and write selected array(s) to binary files
 */
   if(nindex > 0) {
    for (index = 0; index < nindex; index++) {
        num_element = 1;
        sds_idx = sds_index[index]-1;
        sds_id = SDselect (sd_id, sds_idx); 
        status = SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &num_type, &attributes);
        status = SDgetcal(sds_id, &gain, &gain_err, &offset, &offset_err, &cal_data_type );


        printf("\n ****************************************\n");
        printf("  SDS name  = %s\n",  sds_name);
        printf("  SDS type  = %4d\n", num_type);
        printf("  SDS rank  = %4d\n", rank);
        printf("  SDS scale = %f\n",  gain);
        printf("  SDS dims  = ");
        for (j = 0; j < rank; j++) {
             printf("%6d", dim_sizes[j]);
             num_element *= dim_sizes[j];
        }
        printf ("\n");

        for (j = 0; j < rank; j++) {
            edges[j]  = dim_sizes[j];
            stride[j] = 1;
            start[j]  = 0;
        }

/*
 * Read HDF sds arrays
 */
        switch (num_type)
        {
          case DFNT_FLOAT32: f32 = (float *) malloc( num_element * sizeof(float)); break;
          case DFNT_FLOAT64: f64 = (float64 *) malloc( num_element * sizeof(float64)); break;
          case DFNT_INT8: i8 = (int8 *) malloc( num_element * sizeof(int8)); break;
          case DFNT_UINT8: ui8 = (uint8 *) malloc( num_element * sizeof(uint8)); break;
          case DFNT_INT16: i16 = (int16 *) malloc( num_element * sizeof(int16)); break;
          case DFNT_UINT16: ui16 = (uint16 *) malloc( num_element * sizeof(uint16)); break;
          case DFNT_INT32: i32 = (int32 *) malloc( num_element * sizeof(int32)); break;
          case DFNT_UINT32: ui32 = (uint32 *) malloc( num_element * sizeof(uint32)); break;
          case DFNT_CHAR8: c8 = (char8 *) malloc( num_element * sizeof(char8)); break;
          default: printf("not valid data type\n"); break;
        }

        switch (num_type)
        {
          case DFNT_FLOAT32: status = SDreaddata (sds_id, start, NULL, edges, f32); break;
          case DFNT_FLOAT64: status = SDreaddata (sds_id, start, NULL, edges, f64); break;
          case DFNT_INT8:    status = SDreaddata (sds_id, start, NULL, edges, i8); break;
          case DFNT_UINT8:   status = SDreaddata (sds_id, start, NULL, edges, ui8); break;
          case DFNT_INT16:   status = SDreaddata (sds_id, start, NULL, edges, i16); break;
          case DFNT_UINT16:  status = SDreaddata (sds_id, start, NULL, edges, ui16); break;
          case DFNT_INT32:   status = SDreaddata (sds_id, start, NULL, edges, i32); break;
          case DFNT_UINT32:  status = SDreaddata (sds_id, start, NULL, edges, ui32); break;
          case DFNT_CHAR8:   status = SDreaddata (sds_id, start, NULL, edges, c8); break;
          default: printf("not valid data type\n"); break;
        }

        if ( status != 0 )
           printf("\n SDreaddata failed on data set %s. \n", sds_name);
                 
/*
 * Out put sds to binary data file
 */

/* see number in
   4.1r5-linux/include/hntdefs.h
 */
        tempfile = strdup(infile);
        strcpy(outfile, tempfile);
        strcat(outfile, sds_name);
        sds_fp = fopen(outfile,"wb");
        if (sds_fp == NULL)
            printf("Open output sds file failed. \n");
        printf("sds file %s. \n", outfile);

        switch (num_type)
        {
          case DFNT_FLOAT32: 
            status = fwrite(f32, num_element * sizeof(float),1,sds_fp);
           
            break;
          case DFNT_FLOAT64: 
            status = fwrite(f64, num_element * sizeof(float64),1,sds_fp);
            break;
          case DFNT_INT8:    
            status = fwrite(i8, num_element * sizeof(int8),1,sds_fp);
            break;
          case DFNT_UINT8:   /* 21 */
            status = fwrite(ui8, num_element * sizeof(uint8),1,sds_fp);
            
            /* get dimensions (c, r) again - to be sure */
            status2 = SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &num_type, &attributes);
            bytes=1;
            break;
          case DFNT_INT16:   
            status = fwrite(i16, num_element * sizeof(int16),1,sds_fp);
            break;
          case DFNT_UINT16:  /* 23 */
/* not working yet for MODIS */
            status = fwrite(ui16, num_element * sizeof(uint16),1,sds_fp);

            /* get dimensions (c, r) again - to be sure */
            status2 = SDgetinfo(sds_id, sds_name, &rank, dim_sizes, &num_type, &attributes);
            bytes=4;
            break;
          case DFNT_INT32:   
            status = fwrite(i32, num_element * sizeof(int32),1,sds_fp);
            break;
          case DFNT_UINT32:  
            status = fwrite(ui32, num_element * sizeof(uint32),1,sds_fp);
            break;
          case DFNT_CHAR8:   
            status = fwrite(c8, num_element * sizeof(c8),1,sds_fp); 
            break;
          default: printf("not valid data type\n"); break;
        }

        if ( status == 0 ) 
           fprintf(stderr, "\n fwrite failed on data set %s. \n", sds_name);
        else
           fprintf(stderr, "\n Binary temp file successfully created.\n");

        fclose(sds_fp);

        G_free(tempfile);

       /* dim_sizes cols and rows*/
        fprintf(stderr, "Importing into GRASS....\n");
        sprintf(cmd, "r.in.bin in=%s out=%s r=%i c=%i bytes=%i",outfile,outfile,dim_sizes[rank-2],dim_sizes[rank-1], bytes);
        G_system(cmd);

       /* coltable*/
        sprintf(cmd, "r.colors %s col=grey.eq",outfile);
        G_system(cmd);

       /* delete interim binary file */
/*        sprintf(cmd, "rm -f %s",outfile);
        G_system(cmd);*/
        fprintf(stderr, "GRASS file successfully created:\n %s\n", outfile);

        switch (num_type)
        {
          case DFNT_FLOAT32: free(f32); break;
          case DFNT_FLOAT64: free(f64); break;
          case DFNT_INT8: free(i8); break;
          case DFNT_UINT8: free(ui8); break;
          case DFNT_INT16: free(i16); break;
          case DFNT_UINT16: free(ui16); break;
          case DFNT_INT32: free(i32); break;
          case DFNT_UINT32: free(ui32); break;
          case DFNT_CHAR8: free(c8); break;
          default: printf("not valid data type\n"); break;
        }

        status = SDendaccess(sds_id);
        

        strcpy(outfile, "");

    } /* end of for loop for sds */

   } /* end of if statement for nindex */

    status = SDend(sd_id);
    printf(" \n\n");


/*=================================*/



/*
 * Now read in the Vdata tables and write non-dimension Vdatas to
 * separate output files
 */
    vdata_ref = - 1;
    vdata_ref =  VSgetid(file_id, vdata_ref);
    if ( vdata_ref == -1 ) {
         printf("\n No Vdatas found in data set\n");
         exit(-1);
    }

    printf("\n\n  List of Vdata name:               (rec_num   rec_size)\n\n");
    while(vdata_ref != -1) {
      vdata_id    = VSattach(file_id, vdata_ref, "r");

      status = VSinquire(vdata_id, &n_records, &interlace,
                         fields, &vdata_size, vdata_name);

      if ( status != 0 ) {
          printf("\n VSinquire failed on vdata %s\n", vdata_name);
          exit(-1);
      }

      if ( strstr(vdata_name, "fakeDim") == NULL &&
           strstr(vdata_name, "offset") == NULL &&
           strstr(vdata_name, "factor") == NULL ) {
         total_vref[total_vidx++] = vdata_ref;
         printf(" %2d)  %-30s   %5d     %5d \n", total_vidx, vdata_name,
                 n_records, vdata_size);
      }

      status    = VSdetach( vdata_id );
      vdata_ref = VSgetid ( file_id, vdata_ref );

    }

/*
 * Prompt user for desired vdata to be written as binary file
 */

    printf("\n Enter total number of vdata to write out or 0 to exit program\n");
    scanf("%d", &nparm);
    if(nparm == 0) {
       nindex = 0;
       vdata_index[nindex] = 0;
        printf("\n End of Slect_vNsds\n");
         exit(-1);
    }
    else if (nparm == total_vidx){
       nindex = 0;
       for (index = 0; index < total_vidx; index++)
           vdata_index[nindex++] = index+1;
    }
    else {
       printf("\n Enter parameter numbers from above list separated by white space\n");
       nindex = 0;
       for (index = 0; index < nparm; index++) {
           scanf("%d", &temparm);
           if(temparm <= total_vidx)
              vdata_index[nindex++] = temparm;
       }
    }

    if(nparm > 0) {
       printf(" The valid vdata numbers you Enter are: ");
       for (index = 0; index < nindex; index++)
            printf(" %d ", vdata_index[index]);
    }
    else
       printf(" No vdata selected -- # of vdata = 0 \n");


/*
 * Now loop through vdatas and write selected data(s) to binary files
 */

   if(nindex > 0) {

    for (index = 0; index < nindex; index++) {
         vdata_idx = vdata_index[index]-1;
         vdata_id    = VSattach(file_id, total_vref[vdata_idx], "r");

         strcpy(fields, "");
         status = VSinquire(vdata_id, &n_records, &interlace,
                            fields, &vdata_size, vdata_name);

         if ( status != 0 ) {
             printf("\n VSinquire failed on vdata %s\n", vdata_name);
             exit(-1);
         }

         printf("\n\n *************************************************\n");
         printf("   Vdata name     :   %s\n", vdata_name);
         printf("   Vdata recs     :   %d\n", n_records);
         printf("   Vdata recsize  :   %d\n", vdata_size);

         status = VSsetfields( vdata_id, fields );

         vdatabuf = (uint8 *)malloc(n_records*vdata_size);
         status = VSread(vdata_id, vdatabuf, n_records, FULL_INTERLACE);

         if (status != n_records) 
            printf("\n Vdata recs read and num_rec do not match");
 
         ifield = VFnfields(vdata_id);

         printf("   Field Name list: \n");
         for (field_index = 0; field_index<ifield; field_index++) {
             fieldname = VFfieldname(vdata_id, field_index);
             field_size   = VFfieldisize( vdata_id, field_index );
             field_order  = VFfieldorder( vdata_id, field_index );
             field_type   = VFfieldtype( vdata_id, field_index );
             printf("         %-20s  (size: %4d, type: %3d )\n",
                     fieldname,field_size, field_type);
         }
  
/*
 * Now write out the Vdata information to a binary file
 */
            
         tempfile = strdup(infile);
         strcpy(outfile, tempfile);
         strcat(outfile, vdata_name);
         vd_fp = fopen(outfile,"wb");
         if (vd_fp == NULL)
             printf("Open vdata file failed  %s\n", vdata_name);
         status = fwrite(vdatabuf, sizeof(uint8), n_records * vdata_size, vd_fp);
         if ( status == 0 ) 
             printf("\n fwrite failed on vdata %s. \n", vdata_name);
         else
            printf("\n Binary file successfully created : %s\n", outfile);


         fclose(vd_fp);

         status    = VSdetach( vdata_id );


    } /* end of for statement for index of total selected vdata */

   } /* end of if statement for nindex */

   printf("\n\n");
   status = Vend ( file_id );
   status = Hclose( file_id );


}
