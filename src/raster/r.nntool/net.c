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
  Code For Manipulation Of Net Structures (Prefix = N_)
----------------------------------------------------------------------
  This code is divided into 3 major sections:

  (1) include files
  (2) externed functions 
  (3) subroutines
 
  Each section is further explained below.
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDE FILES
----------------------------------------------------------------------
*/
#include  "common.h"
#include  "weights.h"
#include  "layer.h"
#include  "net.h"
#include  "netio.h"
#include "gis.h"


/*
----------------------------------------------------------------------
  EXTERNED FUNCTIONS AND GLOBALS
----------------------------------------------------------------------
  Below are the functions defined in other files which are used by the
  code here. They are organized by section.
----------------------------------------------------------------------
*/
extern  void         P_prop_input();
extern  float        IO_my_get_float();
extern  int          PS_get_float_from_file();
extern  int          PS_skip_tokens();
extern  Sint         C_float_to_Sint();
extern  float        C_Sint_to_float();
extern  char         *sys_alloc();
extern  void         IO_print();
extern  void         IO_reset_more();
extern  int          IO_more();
extern  void         IO_insert_format();

extern  FILE         *PA_open_binary();
extern  void         PA_write_signature();
extern  int          PA_check_signature();
extern  int          PA_get_from_workfile();
extern  int          PA_put_to_workfile();
extern  void         PA_flush();
extern	void		 PA_put_ascii_wts();
extern  Sint         PA_get_ascii_wts();

extern  char         IO_str[MAX_LINE_SIZE];
extern char          IO_wkstr[MAX_LINE_SIZE];
CELL *putbuf, *G_alloc_cell_buf();

extern int ncols, nrows;

/*
======================================================================
  ROUTINES IN NET.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "N_" indicating that it is defined in the 
  "net.c" file.  The types returned by the routines are also shown here
  so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
    Layer *                     N_get_layer
    void                        N_query_net
    int                         N_propagate_and_print
    int                         N_reset_wts                    
    void                        N_save_wts    
======================================================================
*/


Layer  *N_get_layer(ptr_net, source)
Net    *ptr_net;
int    source;
/*
----------------------------------------------------------------------
 This routine searches through the list of layers associated with a   
  net (pointed to by 'ptr_net') and returns a pointer to the layer    
  with an ID equal to the 'source' number passed in.  If no such layer
  is found, then an 'error layer' is returned; ie, a new layer is     
  created with an ID = -1 (see the net.h file).                       
----------------------------------------------------------------------
*/
BEGIN
   Layer_lst  *next_layer;
   Layer      *dummy;

   if (ptr_net->input_layer->ID == source)
      return(ptr_net->input_layer);

   if (ptr_net->output_layer->ID == source)
      return(ptr_net->output_layer);

   next_layer = ptr_net->hidden_front;
   while (next_layer != NULL) BEGIN
      if (next_layer->value->ID == source)
         return(next_layer->value);
      next_layer = next_layer->next;
   ENDWHILE  

   dummy = (Layer *) sys_alloc((unsigned)sizeof(Layer));
   dummy->ID = ERROR;
   return(dummy);

END /* N_get_layer */

extern int thermo, Noclass, **FLAG;
void  N_query_net(ptr_net,num)
Net   *ptr_net;
int num;
{
   int    i, fout, ncol, k, row, col;
   int    count, N_propagate_and_print();
   float  tf1;
   FILE   *fp;
   struct Colors colors;
   struct Categories cats;
   char tmpstr[20];
   extern float **kINPUT[10], **OUT;
   extern int Noutput;

   OUT = (float **) malloc(sizeof(float)*(num+1));
   Menu_msg("Propagating input through the network ");
   for(row=0,count=0;row < nrows;row++) {
     for(col=0;col < ncols;col++)
      if(FLAG[row][col] == 1) {
	 OUT[count] = (float *) malloc(sizeof(float)*(Noutput+1));
         for (i = 0; i < ptr_net->input_layer->num_nodes; i++) {
	       tf1 = kINPUT[i][row][col];
               ptr_net->input_layer->node_outputs[i] = C_float_to_Sint(tf1);
         }
	 N_propagate_and_print(ptr_net, count);
	 count++;
      }
     G_percent(row,nrows,1);
   }
} /* N_query_net */

int  N_propagate_and_print(ptr_net, input_num)
Net   *ptr_net;
int   input_num;
/*
----------------------------------------------------------------------
 This routine is used for passing input to the net to see what sort of
  output comes out.  In other words, once you have taught a net you   
  probably want to give it inputs and have outputs returned.  This    
  routine takes care of that.   The inputs are passed to this function
  by the pointer ptr_net.   The inputs are propagated through the network
  and displayed at the terminal or written to a file depending of out_fp.
  The "input_num" indicates the number of the input being processed 
  which is used as part of the output.
 NOTE that this guy assumes the inputs have been properly setup on the
  input layer of the network BEFORE this routine is called.
---------------------------------------------------------------------
*/
BEGIN
   int    i, num_outs, col, k;
   float  tf1, out[2];
   extern float **OUT;

   /*---------------------*/
   /* propagate the input */
   /*---------------------*/
   P_prop_input(ptr_net);

   /*--------------------------------------------*/
   /* send output to screen if out_fp is == NULL */
   /*--------------------------------------------*/
      for(i = 0;i < ptr_net->output_layer->num_nodes;i++) BEGIN
         tf1 = C_Sint_to_float(ptr_net->output_layer->node_outputs[i]);
         OUT[input_num][i+1] = tf1;
      ENDFOR
   return(OKAY);

END /* N_propagate_and_print */


int  N_reset_wts(ptr_net, file_name, format)
Net   *ptr_net;
char  file_name[];
int   format;
/*
----------------------------------------------------------------------
 This routine resets the weights of a net using a file specified by   
  the user.  The idea is to move through the hidden layers list from  
  the front, reading in values for all the incoming weights to a layer
  Note that both the incoming and outgoing weight values do not need  
  to be read in since the outgoing weights of one layer are the same  
  as the incoming weights of the target layer.  Note also that the    
  order of the movement through the hidden layers list is significant.
  By starting at the front we skip over the input layer, thus we must 
  use INCOMING weights, otherwise we would never set the weights      
  coming OUT of layer 0!  Of course, we don't have to worry about any 
  weights coming out of layer 1 (the output) because the output layer 
  is defined as having no outgoing weights.      
 A new addition to this routine (8-16-89) checks to see if the file 
  has a "signature" which matches the format desired by the user. This
  is done using the BIN_SIG (see common.h file) which is a hexidecimal
  code written to a binary file (ie FAST_FORMAT file) at the very top.
  If this signature is not present, the file is assumed to be PORTABLE_
  FORMAT. If the presence/lack of the signature does not match with 
  the desired format, then an error is returned (see PA_check_signature).                     
----------------------------------------------------------------------
*/
BEGIN
   Weights_lst  *cur_weight;
   Layer_lst    *cur_layer;
   Sint         *wt_values, tmp_Sint, *bias_values;
   int          STATUS, flag, s_size, t_size, j;
   int32        num_weights, i;
   FILE         *fp;
   
   /*---------------------------------------------*/
   /* check that the file format matches "format" */
   /*---------------------------------------------*/
   if (PA_check_signature(file_name, format) == ERROR)
      return(ERROR);
      
   if (format == PORTABLE_FORMAT)
      fp = fopen(file_name, "rt");
   else
      fp = PA_open_binary(file_name, READ_MODE);
      
   if (fp == NULL) BEGIN
      sprintf(IO_str, "\n*** error, file %s cannot be opened ***\n", file_name);
      IO_print(0);
      STATUS = ERROR;
   ENDIF
   else BEGIN
      /*-------------------------------------------------*/
      /* once the file is open, init the value of STATUS */
      /* and if a bin file, throw away the first 2-bytes */
      /* since it will be the binary signature value     */
      /*-------------------------------------------------*/
      STATUS = OKAY;
      if (format == FAST_FORMAT)
         fread((char *)(&tmp_Sint), SIG_SIZE, 1, fp);

      cur_layer = ptr_net->hidden_front;
      while ((cur_layer != NULL) && (STATUS == OKAY)) BEGIN /* for each layer */
         /*--------------------------------------*/
         /* then read in weight values for layer */
         /*--------------------------------------*/
         cur_weight = cur_layer->value->in_weights;
         while ((cur_weight != NULL) && (STATUS == OKAY)) BEGIN
            wt_values = cur_weight->value->values;
            s_size    = cur_weight->value->source_size;
            t_size    = cur_weight->value->target_size;
            if (cur_weight->value->type == CONNECT_ALL)
               num_weights = ((int32) s_size) * ((int32) t_size);
            else if (s_size < t_size)
               num_weights = ((int32) s_size) * ((int32) cur_weight->value->map_area);
            else
               num_weights = ((int32) t_size) * ((int32) cur_weight->value->map_area);
            for (i = 0; i < num_weights; i++) BEGIN
               /*--------------------------------------------*/
               /* read in the next Sint from the appropriate */
               /* file, depending upon the reading format    */
               /*--------------------------------------------*/
               if ( format == PORTABLE_FORMAT )
                  flag = PA_get_ascii_wts(fp, &tmp_Sint);
               else
                  flag = PA_get_from_workfile(fp, &tmp_Sint);
               
               /*-------------------------------------*/
               /* if everything went well, keep going */
               /*-------------------------------------*/
               if (flag == OKAY)
                  wt_values[i] = tmp_Sint;
               else BEGIN
                  STATUS = ERROR;
                  break;                 /* break out of 'for' loop */
               ENDELSE
            ENDFOR                  
            cur_weight = cur_weight->next;
         ENDWHILE
         cur_layer = cur_layer->next;
      ENDWHILE
      
      /*--------------------------------------------*/
      /* if not enough values for weights then quit */
      /*--------------------------------------------*/
      if (STATUS == ERROR) BEGIN
         sprintf(IO_str, "\n*** Not enough values to reset all weights ***\n");
         IO_print(0);
      ENDIF
      /*--------------------------------*/
      /* otherwise, get the bias values */
      /*--------------------------------*/
      else BEGIN
         STATUS = OKAY;
         cur_layer = ptr_net->hidden_front;
         while ((cur_layer != NULL) && (STATUS == OKAY)) BEGIN /* for each layer */
            /*-------------------------------*/
            /* read in bias values for layer */
            /*-------------------------------*/
            bias_values = cur_layer->value->node_bias;
            for (j = 0; j < cur_layer->value->num_nodes; j++) BEGIN
               if (format == PORTABLE_FORMAT)
                  flag = PA_get_ascii_wts(fp, &tmp_Sint);
               else
                  flag = PA_get_from_workfile(fp, &tmp_Sint);
               if (flag == OKAY)
                  bias_values[j] = tmp_Sint;
               else BEGIN
                  STATUS = ERROR;
                  break;                 /* break out of 'for' loop */
               ENDELSE
            ENDFOR   
         cur_layer = cur_layer->next;
         ENDWHILE
      ENDELSE
      
      if (STATUS == ERROR) BEGIN
         sprintf(IO_str, "\n*** Not enough values to reset all biases ***\n");
         IO_print(0);
      ENDIF
      fclose(fp);
   ENDELSE
   return(STATUS);

END /* N_reset_wts */


void  N_save_wts(ptr_net, file_name, format)
Net   *ptr_net;
char  file_name[];
int   format;
/*
----------------------------------------------------------------------
 This routine is identical to the one above, except for the error     
  messages and the fact that the weights are being saved and not re-  
  stored.  Note that a call to 'PA_flush' is made here.  That is done 
  to make sure that the contents of the buffer are emptied to the     
  output file.                                                         
----------------------------------------------------------------------
*/
BEGIN
   Weights_lst  *cur_weight;
   Layer_lst    *cur_layer;
   Sint         *wt_values, *bias_values;
   int          STATUS, j, s_size, t_size;
   int32        num_weights, i;
   FILE         *fp;

   if (format == PORTABLE_FORMAT)
      fp = fopen(file_name, "wt");
   else
      fp = PA_open_binary(file_name, WRITE_MODE);
   
   if ( fp == NULL ) BEGIN
      sprintf(IO_str, "\n*** error, file %s cannot be opened ***\n", file_name);
      IO_print(0);
   ENDIF
   else BEGIN
      /*-----------------------------------------------*/
      /* once the file is open, set the status flag to */
      /* OK and output the binsignature if bin file    */
      /*-----------------------------------------------*/
      STATUS = OKAY;
      if (format == FAST_FORMAT)
         PA_write_signature(fp, BIN_SIG);

      cur_layer = ptr_net->hidden_front;
      while ((cur_layer != NULL) && (STATUS == OKAY)) BEGIN
         /*-----------------------------*/
         /* write out weights for layer */
         /*-----------------------------*/
         cur_weight = cur_layer->value->in_weights;
         while ((cur_weight != NULL) && (STATUS == OKAY)) BEGIN
            wt_values = cur_weight->value->values;
            s_size    = cur_weight->value->source_size;
            t_size    = cur_weight->value->target_size;
            if (cur_weight->value->type == CONNECT_ALL)
               num_weights = ((int32) s_size) * ((int32) t_size);
            else if (s_size < t_size)
               num_weights = ((int32)s_size) * ((int32)cur_weight->value->map_area);
            else
               num_weights = ((int32)t_size) * ((int32)cur_weight->value->map_area);
               
            /*------------------------------------------------*/
            /* after setting up, either print out as ascii or */
            /* as binary, depending on the value of "format"  */
            /*------------------------------------------------*/
            if (format == PORTABLE_FORMAT)
                PA_put_ascii_wts(wt_values, num_weights, fp);
            else
               for (i = 0; i < num_weights; i++)
                  if ((PA_put_to_workfile(fp, wt_values[i])) == ERROR) BEGIN 
                     STATUS = ERROR;
                     break;  /* break out of 'for' loop */
                  ENDIF
            cur_weight = cur_weight->next;
         ENDWHILE
         cur_layer = cur_layer->next;
      ENDWHILE
      
      /*----------------------------------------*/
      /* if error writing out weights then quit */
      /*----------------------------------------*/
      if (STATUS == ERROR) BEGIN
         sprintf(IO_str, "\n*** error, not all weights were written ***\n");
         IO_print(0);
      ENDIF
      /*--------------------------------------*/
      /* otherwise, write out the bias values */
      /*--------------------------------------*/
      else BEGIN   
         STATUS = OKAY;
         cur_layer = ptr_net->hidden_front;
         while ((cur_layer != NULL) && (STATUS == OKAY)) BEGIN
            /*---------------------------------*/
            /* write out bias values for layer */
            /*---------------------------------*/
            bias_values = cur_layer->value->node_bias;
            if ( format == PORTABLE_FORMAT )
                PA_put_ascii_wts(bias_values, (int32)cur_layer->value->num_nodes, fp);
            else
               for (j = 0; j < cur_layer->value->num_nodes; j++)
                  if ((PA_put_to_workfile(fp, bias_values[j])) == ERROR) BEGIN 
                     STATUS = ERROR;
                     break;  /* break out of 'for' loop */
                  ENDIF
            cur_layer = cur_layer->next;
         ENDWHILE
      ENDELSE /* loop for biases */
      
      /*---------------------------------------*/
      /* check again for error before flushing */
      /* the file and quitting the routine     */
      /*---------------------------------------*/
      if (STATUS == ERROR) BEGIN
         sprintf(IO_str, "\n*** error, not all biases were written ***\n");
         IO_print(0);
      ENDIF
      if ( format == FAST_FORMAT ) PA_flush(fp);
      fclose(fp);
   ENDELSE

END /* N_save_wts */
