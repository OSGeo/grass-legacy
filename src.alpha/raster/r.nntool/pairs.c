/*=============================*/
/*           NETS              */
/*                             */
/* a product of the AI Section */
/* NASA, Johnson Space Center  */
/*                             */
/* principal author:           */
/*       Paul Baffes           */
/*                             */
/* contributing authors:       */
/*      Bryan Dulock           */
/*      Chris Ortiz            */
/*=============================*/


/*
----------------------------------------------------------------------
  Code For Setting up IO pairs for Network training (Prefix = PA_)
----------------------------------------------------------------------
  This code is divided into 4 major sections:

  (1) include files
  (2) externed functions
  (3) global variables
  (4) subroutines
 
  Each section is further explained below.
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDE FILES
----------------------------------------------------------------------
*/
#include "common.h"
#include "weights.h"
#include "layer.h"
#include "net.h"
#include "netio.h"


/*
----------------------------------------------------------------------
  EXTERNED FUNCTIONS
----------------------------------------------------------------------
  Below are the functions defined in other files which are used by the
  code here. They are organized by section.
----------------------------------------------------------------------
*/
extern int    PS_parse_iopairs();
extern float  C_Sint_to_float();
extern char   *sys_long_alloc();
extern char   *sys_alloc();
extern void   sys_long_free();
extern void   sys_free();
extern void   sys_delete_file();
extern void   sys_rename_file();
extern long   IO_find_file_size();
extern void   IO_print();

extern char   IO_str[MAX_LINE_SIZE];



/*
----------------------------------------------------------------------
  GLOBAL VARIABLES
----------------------------------------------------------------------
  Next come the global variables for this code.  All of them revolve
  around reading information from files. In order to speed things up,
  I buffer the input and do block reads into a large space in an array
  from which all other routines work (see documentation in 'netio.h').
  The 'fd_iofile' is an integer which is used as a file descriptor to 
  keep track of the INTERMEDIATE file which is created during training. 
  An intermediate file of IO pairs is created to facilitate training 
  speeds. These pairs are block read into the buffer (ie, "the_buffer" 
  below) and then referenced from there. 
----------------------------------------------------------------------
*/
static buffer  *the_buffer;      /* global here but not visible outside */

static FILE    *fp_iofile = NULL;/* used by rewind_workfile to close    */
                                 /* and reopen the file which has the   */
                                 /* IO pairs for teaching the net.      */


/*
======================================================================
  ROUTINES IN PAIRS.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "PA_" indicating that it is defined in the 
  "pairs.c" file.  The types returned by the routines are also shown here
  so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
    void                        PA_initialize                            
    void                        PA_setup_iopairs                            
    void                        PA_reset_iopairs                            
    FILE *                      PA_open_binary
    void                        PA_write_signature
    int                         PA_check_signature
    int                         PA_put_to_workfile
    void                        PA_flush
    void                        PA_rewind_workfile
    void                        PA_done_with_workfile
    Sint                        PA_retrieve
    int                         PA_get_from_workfile (returns Sint via args)
    void                        PA_randomize_file
    void                        PA_build_random_list
    void                        PA_transfer_blocks
    void                        PA_write_iop
    void						PA_put_ascii_wts
    Sint 						PA_get_ascii_wts
======================================================================
*/


void  PA_initialize()
/*
----------------------------------------------------------------------
 Because the buffer used for reading and writing Sint values from     
  temporary files is NOT VISIBLE outside of this code, this init      
  routine is provided to make sure that space is actually set asside  
  for the buffer.                                    
----------------------------------------------------------------------
*/
BEGIN
   the_buffer = (buffer *) sys_alloc((unsigned)sizeof(buffer));
   the_buffer->index = BUFFER_SIZE;
   the_buffer->last_elem = -1;       /* no elements in buffer */
   
END /* PA_initialize */


void  PA_setup_iopairs(ptr_net, filename)
Net   *ptr_net;
char  filename[];
/*
----------------------------------------------------------------------
 This routine actually does very little of the work involved with     
  setting up the io pairs.  Instead, most of the work is passed to an 
  IO routine (in netio.c) since so much of io is machine dependent.   
  (That is, our netio.c was created specifically to isolate any code  
  dependent on I/O).                                                  
 All this routine does is setup determine what the expected lenght of 
  the io-pairs should be (from the sizes of the input and output      
  layers) and then pass the work on to the IO routines.  The result   
  returned indicates how many io pairs were specified in the input    
  file, or an ERROR indicating some sort of error in the input file.    
----------------------------------------------------------------------
*/
BEGIN
   int  sum;
   
   ptr_net->num_inputs  = ptr_net->input_layer->num_nodes;
   ptr_net->num_outputs = ptr_net->output_layer->num_nodes;

   sum = ptr_net->num_inputs + ptr_net->num_outputs;
   ptr_net->num_io_pairs = PS_parse_iopairs(filename, sum);

   if (ptr_net->num_io_pairs == ERROR)
      sprintf(IO_str, "\n*** problems with IO pairs specification ***\n");
   else
      sprintf(IO_str, "\n %d IO pairs read\n", ptr_net->num_io_pairs);
/*   IO_print(0); */
   Menu_msg(IO_str);

END /* PA_setup_iopairs */


void  PA_reset_iopairs(ptr_net, bin_filename)
Net  *ptr_net;
char  bin_filename[];
/*
----------------------------------------------------------------------
  This routine resets the values in the ptr_net Net structure to match
  the number of iopairs found in the "filename" binary file. Several
  assumptions are made. First, it is assumed that the incoming filename
  indicates a binary file which was created at some earlier time by a
  call to the "i" option of NETS (recall that the "i" option has the
  effect of translating an ascii IOP file into a binary Sint format).
  This file's length is determined and compared against the number of
  inputs and outputs to determine the amount of iopairs in the file.
  If the sizes do not map evenly, then this routine fails.
----------------------------------------------------------------------
*/
BEGIN
   int  in_size, out_size, pair_size, file_len, num_iopairs;

   /*---------------------------------------*/
   /* get the sizes of the input and output */
   /* layers and the size of the file. NOTE */
   /* the size of one iopair must be multi- */
   /* plied by the size of a Sint in bytes. */
   /*---------------------------------------*/
   in_size   = ptr_net->input_layer->num_nodes;
   out_size  = ptr_net->output_layer->num_nodes;
   pair_size = sizeof(Sint) * (in_size + out_size);
   file_len  = IO_find_file_size(bin_filename);
   
   /*--------------------------------------*/
   /* determine how many iopairs could fit */
   /* in file. NOTE that "/" rounds down   */
   /*--------------------------------------*/
   num_iopairs = file_len / pair_size;
   
   /*------------------------------------------*/
   /* remultiplying the number of pairs by the */
   /* sum of the input and output sizes should */
   /* yield the file size if there is a match  */
   /*------------------------------------------*/
   if ( (num_iopairs * pair_size) != file_len )
      sprintf(IO_str, "\n*** net configuration does not match '%s' file ***\n", bin_filename);
   else BEGIN
      ptr_net->num_inputs   = in_size;
      ptr_net->num_outputs  = out_size;
      ptr_net->num_io_pairs = num_iopairs;
      sprintf(IO_str, "\n %d IO pairs read\n", num_iopairs);
   ENDELSE
   IO_print(0);

END /* PA_reset_iopairs */


FILE  *PA_open_binary(file_name, mode)
char  file_name[];
int   mode;
/*
----------------------------------------------------------------------
 Resets the file given in "file name" using the open call with the    
  mode specified by "mode".  Note, this will only reset for mass IO   
  reading and writing, and it also assumes the existence of a buffer  
  for holding all of the IO.  The result returned is a file pointer
  which can then be used for reading from or writing to the file.     
 NOTE THAT THE CALLER MUST ENSURE THAT THE FILE IS CLOSED.            
----------------------------------------------------------------------
*/
BEGIN
   FILE  *fp;

   if (mode == READ_MODE) BEGIN
      fp = fopen(file_name, "rb");
      the_buffer->index = BUFFER_SIZE;
      the_buffer->last_elem = -1;      /* no elements in buffer */
   ENDIF
   else BEGIN
      fp = fopen(file_name, "wb");
      the_buffer->index = 0;
   ENDELSE
   return(fp);

END /* PA_open_binary */


void  PA_write_signature(fp, signature)
FILE       *fp;
short int  signature;
/*
----------------------------------------------------------------------
  Writes out the 2-byte signature to the file indicated by "fp."  Note
  that only 2 bytes are written, regardless of whether Sint or float
  format is being used.
----------------------------------------------------------------------
*/
BEGIN

   /*-----------------------------------------*/
   /* the "2" indicates that the signature is */
   /* 2-bytes long. "1" means 1 sig written   */
   /*-----------------------------------------*/
   fwrite((char *)(&signature), SIG_SIZE, 1, fp);

END /* PA_write_signature */


int  PA_check_signature(filename, format)
char  filename[];
int   format;
/*
----------------------------------------------------------------------
  This routine opens the file "filename" and checks the first two bytes
  of the file to determine whether or not the file has a signature. IF
  it does, it will have the BIN_SIG of all binary weights files used by 
  NETS. The format of the file must match the format passed in by the 
  "format" parameter. If there is no match or if the file cannot be 
  opened, then an error is returned.
----------------------------------------------------------------------
*/
BEGIN
   FILE       *fp;
   short int  signature;
   int        PA_text_sig_check();
   
   /*--------------------------------------------*/
   /* if the file cannot be opened, return error */
   /*--------------------------------------------*/
   if ( (fp = fopen(filename, "rb")) == NULL ) BEGIN
      sprintf(IO_str, "\n*** can't open file %s ***\n", filename);
      IO_print(0);
      return(ERROR);
   ENDIF
   
   /*------------------------------------*/
   /* otherwize, read in the signature   */
   /* NOTE signatures are always 2 bytes */
   /*------------------------------------*/
   fread((char *)(&signature), SIG_SIZE, 1, fp);
   fclose(fp);
   
   /*-------------------------------------*/
   /* first check the desired file format */
   /*-------------------------------------*/
   if (format == FAST_FORMAT) BEGIN
   
      /*--------------------------------*/
      /* if signature is correct return */
      /*--------------------------------*/
      if (signature == BIN_SIG) return(OKAY);
      
      /*------------------------------------------------*/
      /* otherwise, signature could be binary but wrong */
      /*------------------------------------------------*/
      if (signature == WRONG_SIG) BEGIN
         sprintf(IO_str, "\n*** file is not in %s format",
                 ((USE_SCALED_INTS == 1)? "scaled integer" : "floating point") );
         IO_print(0);
      ENDIF
      
      /*------------------------------------------------*/
      /* or, lastly, signature might not even be binary */
      /*------------------------------------------------*/
      else BEGIN
         sprintf(IO_str, "\n*** file is not in FAST format");
         IO_print(0);
      ENDELSE
      
      /*----------------------------------*/
      /* in any case, if you get past the */
      /* BIN_SIG case you return an error */
      /*----------------------------------*/
      return(ERROR);
   ENDIF
   
   /*-----------------------------------*/
   /* else if format is PORTABLE_FORMAT */
   /*-----------------------------------*/
   else BEGIN
      if ((signature == BIN_SIG) || (signature == WRONG_SIG)) BEGIN
        sprintf(IO_str, "\n*** file not in PORTABLE format");
        IO_print(0);
        return(ERROR);
      ENDIF
      else 
         return(PA_text_sig_check(filename));
   ENDELSE
   
END /* PA_check_signature */


int  PA_text_sig_check(filename)
char  filename[];
/*
----------------------------------------------------------------------
  This routine checks PORTABLE format files only. The problem here is
  that if a file is written in ascii it cannot be given an initial 
  signature like a binary file can. Thus, the PA_check_signature routine
  must call this routine in the event that a portable file is successfully
  found. This routine returns OKAY or ERROR, depending upon whether or not
  the data in the file matches the Sint format (when USE_SCALED_INTS = 1)
  or the float format.
----------------------------------------------------------------------
*/
BEGIN
   FILE  *fp;
   char  in_str[MAX_WORD_SIZE];
   int   i, float_present = FALSEE;
   
   /*------------------------------------*/
   /* get the first string from the file */
   /*------------------------------------*/
   fp = fopen(filename, "rt");
   fgets(in_str, MAX_WORD_SIZE, fp);
   fclose(fp);
   
   /*-----------------------------*/
   /* try to find a decimal point */
   /*-----------------------------*/
   i = 0;
   while (in_str[i] != '\0') BEGIN
      if (in_str[i] == '.') BEGIN
         float_present = TRUEE;
         break;
      ENDIF
      i++;
   ENDWHILE
   
   /*----------------------------*/
   /* if formats match return OKAY */
   /*----------------------------*/
   if ( ((float_present == FALSEE) && (USE_SCALED_INTS == 1))
        || ((float_present == TRUEE) && (USE_SCALED_INTS == 0)) )
      return(OKAY);
   
   /*------------------------------------------------*/
   /* otherwise, printout error msg and return ERROR */
   /*------------------------------------------------*/
   sprintf(IO_str, "\n*** file is not in %s format",
           ((USE_SCALED_INTS == 1) ? "scaled integer" : "floating point") );
   IO_print(0);
   return(ERROR);
      
END /* PA_text_sig_check */


int  PA_put_to_workfile(fp, Snum)
FILE  *fp;
Sint  Snum;
/*
----------------------------------------------------------------------
 This routine puts out Sints to a temporary storage file for the net. 
  By default, I have decided to call the file "workfile.net" as it is 
  named in the above routine 'PS_parse_iopairs'.  Instead of putting   
  out Sints a word at a time, this routine first writes the Sint to a 
  buffer and then checks to see if the buffer is full. If so, then the
  entire buffer is mass written to the temporary file, which is much  
  quicker than trying to do things a word at a time.                  
 
 Returns OKAY if the write goes ok, otherwise returns ERROR.
----------------------------------------------------------------------
*/
BEGIN
   int  temp;

   temp = OKAY;
   if (the_buffer->index >= BUFFER_SIZE) BEGIN /* the buffer is full    */
      temp = fwrite((char *)the_buffer->values, sizeof(Sint), BUFFER_SIZE, fp);
      if (temp != BUFFER_SIZE) BEGIN
         sprintf(IO_str, "*** Internal Error writing to WorkFile ***");
	 Menu_msg(IO_str);
/*         IO_print(0);  */
         temp = ERROR;
      ENDIF
      else temp = OKAY;
      the_buffer->index = 0;
   ENDIF

   the_buffer->values[the_buffer->index] = Snum;
   the_buffer->index++;
   return(temp);

END /* PA_put_to_workfile */


void  PA_flush(fp)
FILE  *fp;
/*
----------------------------------------------------------------------
 This guy will print whatever is left in the buffer out to the temp   
  file.  That is, after the last character is "put" by the routine    
  above, there is no guarantee that the buffer was also printed to the
  (in fact, it would have only been printed in the case that the last 
  "put" exactly filled the buffer).  So this routine is needed to make
  sure that nothing gets lost in the buffer.                          
----------------------------------------------------------------------
*/
BEGIN
   fwrite((char *)the_buffer->values, sizeof(Sint), the_buffer->index, fp);
   the_buffer->index = 0;

END /* PA_flush */


void  PA_rewind_workfile()
/*
----------------------------------------------------------------------
 This routine resets the temporary file ("workfile.net") which has    
  all of the io pairs by opening or rewinding the file.  This   
  must be done EACH TIME through a learning cycle.  Also, the input   
  buffer ('the_buffer') must be reset to indicate that new values are 
  to be read in.  This is done by setting the index of the buffer to
  BUFFER_SIZE.
----------------------------------------------------------------------
*/
BEGIN
   if (fp_iofile == NULL)
      fp_iofile = fopen("workfile.net", "rb");
   else if (fseek(fp_iofile, 0L, SEEK_SET) != 0) BEGIN
      sprintf(IO_str, "\n*** INTERNAL ERROR: could not rewind WorkFile");
      IO_print(0);
   ENDELSE

   /*----------------------------------------------*/
   /* index set to end to force a read of the file */
   /*----------------------------------------------*/
   the_buffer->index = BUFFER_SIZE;
   the_buffer->last_elem = -1;

END /* PA_rewind_workfile */


void  PA_done_with_workfile()
/*
----------------------------------------------------------------------
  This routine makes sure that the "workfile.net" file is closed before
  training is completed. It is only called by "T_teach_net" routine.
----------------------------------------------------------------------
*/
BEGIN   
   if (fp_iofile != NULL) fclose(fp_iofile);
   fp_iofile = NULL;

END  /* PA_done_with_workfile */


Sint  PA_retrieve()
/*
----------------------------------------------------------------------
 This may seem like an absolutely stupid routine, and on one level I  
  agree; it is so small it seems ridiculous to waste time doing a     
  routine call just to set up another routine call.  However, I wanted
  to leave the 'PA_get_from_workfile', or 'get', routine isolated, so that
  it could be used for different kinds of IO.  For example, if I had     
  simply called the 'get' routine directly from 'net.c', then it would
  have had to assume the existence of the file descriptor 'fp_iofile' 
  just as this routine does.  Of course, then the 'get' routine would 
  only be good for reading from that file descriptor, which is really 
  not a good limitation, nor would it be consistent with the routine  
  'put_to_workfile'.  Thus this routine acts as a sort of filter to   
  any outside routines from other files from having to know any of the
  details of the IO.                                                  
 Note that this routine assumes the existence of a global file des-   
  criptor called 'fd_iofile' which points to the work file.           
 Returns positive or 0 if the read goes ok, otherwise returns a -1.   
----------------------------------------------------------------------
*/
BEGIN
   int   PA_get_from_workfile();
   Sint  result;

   if (PA_get_from_workfile(fp_iofile, &result) == ERROR) BEGIN
      sprintf(IO_str, "\n*** INTERNAL ERROR: could not access WorkFile");
      IO_print(0);
   ENDIF
   return(result);

END /* PA_retrieve */


int  PA_get_from_workfile(fp, ptr_Sint)
FILE  *fp;
Sint  *ptr_Sint;
/*
----------------------------------------------------------------------
 This routine is the converse of 'put_to_workfile' above.  Instead of 
  storing Sints, it reads them out of memory and places them into the 
  'ptr_Sint' variable passed in.  Again, the default file for reading 
  is the same one where the stuff was written; "workfile.net".        
 Returns OKAY if a value can be read, ERROR otherwise.
----------------------------------------------------------------------
*/
BEGIN
   int  temp;

   temp = OKAY;
   /*----------------------------------*/ 
   /* if the buffer contents exhausted */
   /* try reading more from file       */
   /*----------------------------------*/ 
   if (the_buffer->index >= BUFFER_SIZE) BEGIN
      temp = fread((char *)the_buffer->values, sizeof(Sint), BUFFER_SIZE, fp);
      if (temp == 0) BEGIN
         sprintf(IO_str, "\n*** Internal Error reading from file ***\n");
         IO_print(0);
         temp = ERROR;
      ENDIF
      else BEGIN
         the_buffer->last_elem = temp - 1;
         temp = OKAY;
         the_buffer->index = 0;
      ENDELSE
   ENDIF
   /*----------------------------------------*/
   /* otherwise, if already read past end of */
   /* buffer then return error and message   */
   /*----------------------------------------*/
   else if (the_buffer->index > the_buffer->last_elem) BEGIN
      temp = ERROR;
      sprintf(IO_str, "\n*** attempt to read past last value of file");
      IO_print(0);
   ENDELSE
   
   /*----------------------------------*/
   /* in any event, read the next elem */
   /*----------------------------------*/
   *ptr_Sint = the_buffer->values[the_buffer->index];
   the_buffer->index++;
   
   return(temp);

END /* PA_get_from_workfile */


void  PA_randomize_file(new_file, bin_file, num_blocks, block_size)
char  *new_file, *bin_file;
int   num_blocks, block_size;
/*
----------------------------------------------------------------------
   RANDOMIZE_FILE will take a binary file of I/O pairs and rearrange 
                  the I/O pairs in a random order.
                  
   Author : Chris Ortiz ( MPAD/AI NASA )
   Date   : 31-May-1989
   Cyclomatic Complexity = 1
   
   Parameters Used
   ---------------
      Num_Blocks - The number of I/O pairs ( Blocks ) in the binary file.
      Block_Size - The number of bytes in each I/O pair.
      File_Name  - Binary file to be sorted.
 
   Variables Used
   --------------
      Array      - Array with the random order for the binary blocks.
     
   Procedures / Functions
   ----------------------
   
      PA_build_random_list - will take an array, initialize it, and
          select and store a group of random block numbers.
      PA_transfer_blocks - will take an array and build a new file
          using random blocks from the old file.
 
   Algorithm
   ---------
      1. Build array of random numbers corresponding to random I/O pairs.
      2. Build I/O pair file in random order corresponding to the array.
 
----------------------------------------------------------------------
*/
BEGIN
   int   *array;
   void  PA_build_random_list(), PA_transfer_blocks();
   
   array = (int *) sys_alloc((unsigned)num_blocks * (unsigned)sizeof(int));
   num_blocks = num_blocks - 1;    /* Number of Blocks must start at 0 */
   PA_build_random_list(array, num_blocks);
   PA_transfer_blocks(array, new_file, bin_file, block_size, num_blocks);
   
   sys_free((char *) array);
   
END /* PA_randomize_file */


void  PA_build_random_list(array, num_block)
int   array[], num_block;
/*
----------------------------------------------------------------------
  BUILD_RANDOM_LIST will take an array, select non-repeating random numbers
                       from 0 to sizeof(array), and store the numbers back into
                       the array for use later.
                 
  Author : Chris Ortiz ( MPAD/AI NASA )
  Date   : 1-Jun-1989
  Cyclomatic Complexity = 3

  Parameters Used
  ---------------
     Array - Array containing the random order of block Id's. (Output)
     Num_Block - Number of blocks in a file. (Input)

  Variables Used
  --------------
     Last - Pointer to tail of array.
     Slot - Random Slot in array.
     Temp - Storage Variable
     
  Procedures / Functions
  ----------------------
     Rand() - Returns a pseudorandom-number 0..32,767 ( RAND_MAX ).

  Algorithm
  ---------
     1. Initialize array with numbers corresponding to the index.
     2. Initialize the random number generator.
     3. Pick a random index into the array from 0 to the last known element.
     4. Swap contents with the last known element in the array
     5. Move last known element up one index and goto step 3.
----------------------------------------------------------------------
*/
BEGIN
   int last, slot, temp;

   for (last =0; last <= num_block; last++)
       array[last] = last;

   for (last = num_block; last > 0; last--) BEGIN
       slot = rand() % last;
       temp = array [slot];
       array[slot] = array[last];
       array[last] = temp;
   ENDFOR
   
END /* PA_build_random_list */


void  PA_transfer_blocks (array, new_file, bin_file, block_size, num_blocks)
int   array[];
char  *new_file, *bin_file;
int   block_size, num_blocks;
/*
----------------------------------------------------------------------
  TRANSFER_BLOCKS will transfer a block of data from one file to a
                   new file in a specified order which is contained
                     in the array argument.
                 
  Author : Chris Ortiz ( MPAD/AI NASA )
  Date   : 2-Jun-1989
  Cyclomatic Complexity = 2

  Parameters Used
  ---------------
     Array       - Array containing the random order of block Id's.
     File_Name   - Binary File (Unsorted) Name
     Block_Size  - Number of bytes per block of data.
     Num_Block   - Number of blocks in a file.
  
  Variables Used
  --------------
     FP_Output   - Output File
     FP_Input    - Input File
     Ptr         - Element of file to be transfered
     Block       - Current Block Number
     Byte_Offset - Number of Bytes into file before reading.
     
  Procedures / Functions
  ----------------------
     Write_IOP   - Write a single I/O pair into an ascii format.

  Algorithm
  ---------
     1. Open Files.
     2. Read Array to get next random block to read.
     3. Jump into input file to the random Block. (Byte_offset)
     4. Read Block from input and write to output (Ascii and Binary).
     5. If not done with random blocks goto step 2.
     6. Close Files
     7. Delete input file ( non-random ).
     8. Rename output file to the name used for the input file.
----------------------------------------------------------------------
*/
BEGIN
   long int byte_offset;
   int    block;
   FILE  *fp_input, *fp_output, *fp_ascii;
   void  PA_write_iop();
   Sint  *ptr;
   
   ptr = (Sint *) sys_long_alloc((long)(block_size * sizeof(Sint)));
   
   fp_input  = fopen(bin_file, "rb");
   fp_output = fopen("workfile.rnd", "wb");
   fp_ascii  = fopen(new_file,"wt");
   
   for (block = 0; block <= num_blocks; block++) BEGIN
      byte_offset =  (long)(array[block] * block_size * sizeof(Sint));
      fseek(fp_input, byte_offset, SEEK_SET);
      
      fread((char *)ptr, sizeof(Sint), block_size, fp_input);
      PA_write_iop(ptr, block_size, fp_ascii);
      fwrite((char *)ptr, sizeof(Sint), block_size, fp_output);
   ENDFOR

   fclose(fp_input);
   fclose(fp_ascii);
   fclose(fp_output);

   sys_delete_file(bin_file);
   sys_rename_file("workfile.rnd", bin_file);
  
   sys_long_free((char *) ptr);
   
END /* PA_transfer_blocks */


void  PA_write_iop (ptr, num_ptr, fp_ascii)
Sint  *ptr;
int   num_ptr;
FILE  *fp_ascii;
/*
----------------------------------------------------------------------
  PA_WRITE_IOP will write the ascii version for the random I/O pairs to a file.
                 
  Author : Chris Ortiz ( MPAD/AI NASA )
  Date   : 3-Jun-1989
  Cyclomatic Complexity = 3

  Parameters Used
  ---------------
     Ptr        - Array containing one I/O pairs to be sent to the ascii file.
     Num_Ptr    - Size of Array.
     Fp_Ascii   - Ascii file where I/O pairs are to be written.

  Variables Used
  --------------
     N_Row      - number of fields to be printed on one line.
     byte       - loop counter.
     
  Procedures / Functions
  ----------------------
     C_Sint_to_float - converts type SINT to type float.

  Algorithm
  ---------
     1. Write '(' to ascii file.
     2. Write convert SINT to float and write to file.
     3. Write only N_Row number of floats to a line.
     4. Write ')' to finish I/O pair.
----------------------------------------------------------------------
*/
BEGIN
   int  n_row, byte;
   
   n_row = 3;
   fprintf (fp_ascii,"(");
         
   for (byte=1; byte <= num_ptr ; byte++) BEGIN
#if  USE_SCALED_INTS
      fprintf (fp_ascii, "%7.3f", C_Sint_to_float(ptr[byte - 1]));
#else
      fprintf (fp_ascii, " %f", ptr[byte - 1]);
#endif

      /*----------------------------------------------*/
      /* after each sint is printed, check to see if  */
      /* you have printed "n_row" of them. If so,     */
      /* then print a newline. Note that byte starts  */
      /* at "1" (not 0) because of the comparisons    */
      /* done here. If it started at 0, you would     */
      /* printout a newline after the first element   */
      /* (try it). Of course, you could equally as    */
      /* well start at "0" and do comparisons here at */
      /* byte+1, but that would mean two additions    */
      /* here, so I opted for byte starting at "1".   */
      /* Finally, note that this means byte must be   */
      /* decremented by one to be written out to file */
      /*----------------------------------------------*/
      if ((byte % n_row == 0) && (byte < num_ptr))
         fprintf(fp_ascii,"\n");
         
   ENDFOR
   fprintf (fp_ascii," )\n");
   
END /* PA_write_iop */


Sint  PA_get_ascii_wts(fp, weight)
FILE  *fp;
Sint  *weight;
/*
--------------------------------------------------------------------------------

 PA_GET_ASCII_WTS will return a single Sint weight from an ASCII file format.

 Author : Chris Ortiz ( MPAD/AI NASA )
 Date   : 4-Aug-1989
 Cyclomatic Complexity = 2

 Variables Used
 --------------
	  fp      - File pointer to ASCII file.
      weight  - Parameter the weight is sent to.
      line    - Line of characters from the input file.
      tmp     - Error condition that is returned from this procedure.

  Algorithm
  ---------
      1. Read a line of characters from input file.
      2. Convert characters to floating point number. (One Float per line)
      3. Convert floating point number to type Sint and store in return var.
      4. Return the error condition.

  Modified (8-15-89) to read in integer representations of the Sints rather
  than floating point representations. The rationale here is that saving floating
  point numbers does nothing except cause rounding errors. The user does not really
  need to be able to read the numbers in their floating point format since there 
  is no additional information in the file which indicates what nodes are attached
  to the weight. Thus, since the floating point conversion can cause round off
  errors, I decided to switch to saving the Sints in ascii (but integer) format.
-----------------------------------------------------------------------------------
*/
BEGIN
   char line[MAX_LINE_SIZE];
   int  tmp;
#if  USE_SCALED_INTS
   int  val;
#else
   float  val;
#endif
   
   tmp = ERROR;
   
   if ( !feof(fp) ) BEGIN
      fgets(line, MAX_LINE_SIZE, fp);
#if  USE_SCALED_INTS
      if (sscanf(line, "%d", &val) == 1) BEGIN
#else
      if (sscanf(line, "%f", &val) == 1) BEGIN
#endif
         *weight = (Sint) val;
         tmp = OKAY;
      ENDIF
   ENDIF
   return(tmp);
   
END /* PA_get_ascii_wts */


void  PA_put_ascii_wts(weights, num_weights, fp)
Sint  *weights;
int32 num_weights;
FILE  *fp;
/*
-------------------------------------------------------------------------------

 PA_PUT_ASCII_WTS will write an array of ASCII weights to a file.

 Author : Chris Ortiz ( MPAD/AI NASA )
 Date   : 4-Aug-1989
 Cyclomatic Complexity = 2

 Variables Used
 --------------
	  fp          - File pointer to ASCII file.
      weights     - array of weights to be written to a file.
      num_weights - number of weights in the array.
	  loop        - variable user to index array.

 Algorithm
 ---------
      For each location in the weights array
      1. convert from SINT to floating point.
      2. write floating point number on a single line to the output file.

---------------------------------------------------------------------------------
*/
BEGIN
   int32 loop;
   
   for ( loop=0; loop < num_weights ; loop++ )
#if  USE_SCALED_INTS
      fprintf(fp, "%d\n", (int)(weights[loop]) );
#else
      fprintf(fp, "%f\n", (float)(weights[loop]) );
#endif

END /* PA_put_ascii_wts */
