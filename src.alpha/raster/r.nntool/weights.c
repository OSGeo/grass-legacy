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
  Code For Manipulating The Weight Structures 
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
#include  "common.h"
#include  "netio.h"
#include  "weights.h"
#include  "layer.h"


/*
----------------------------------------------------------------------
  EXTERNED FUNCTIONS AND GLOBALS
----------------------------------------------------------------------
*/
extern float   C_Sint_to_float();
extern char    *sys_long_alloc();
extern char    *sys_alloc();
extern void    sys_long_free();
extern void    sys_free();
extern float   IO_my_get_float();
extern void    IO_my_get_string();
extern void    IO_print();
extern void    IO_insert_format();
extern int     IO_more();

extern D_Sint  P_full_dot_forward();
extern D_Sint  P_s_pattern_dot_forward();
extern D_Sint  P_t_pattern_dot_forward();
extern D_Sint  P_full_dot_backward();
extern D_Sint  P_s_pattern_dot_backward();
extern D_Sint  P_t_pattern_dot_backward();
extern void    P_full_update_wts();
extern void    P_s_pattern_update_wts();
extern void    P_t_pattern_update_wts();

extern char   IO_str[MAX_LINE_SIZE];
extern char   IO_wkstr[MAX_LINE_SIZE];


/*
----------------------------------------------------------------------
  GLOBAL VARIABLES
----------------------------------------------------------------------
 There are two global values used by this code: "weight_max" and 
  "weight_min".  They keep track of the INITIAL maximum and minimum
  weights set by the user. I suppose there is no strict reason why 
  global variables are needed here, except as a mere convenience. Since
  several routines reference these variables, and since these routines 
  are called at different times, I thought it would be easier to have
  these two values stored as local globals (ie, only global to routines
  resident in this file).
 A third global variable, weight_range, was added at a later time. Its
  main purpose is to speed up the process of assigning the random weights
  for the network. The base is the difference between the maximum and 
  minimum values.
----------------------------------------------------------------------
*/
static Sint  weight_max, weight_min, weight_range;


/*
======================================================================
  ROUTINES IN WEIGHTS.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "W_" indicating that it is defined in the 
  "weights.c" file.  The types returned by the routines are also shown 
  here so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
                                                                      
  WEIGHT ROUTINES                                                   
    Weights *                   W_alloc_weights
    int32                       W_set_decoder
    void                        W_weight_range
    Sint                        W_random_weight
    void                        W_free_weights
    void                        W_show_weights

  WEIGHT_LST ROUTINES
    Weights_lst *               W_alloc_weights_lst
    Weights_lst *               W_insert_before
    Weights_lst *               W_insert_after
    void                        W_free_weights_lst
======================================================================
*/


Weights  *W_alloc_weights(ptr_source, ptr_target, ptr_layer_spec)
Layer       *ptr_source, *ptr_target;
Layer_spec  *ptr_layer_spec;
/*
----------------------------------------------------------------------
 This routine allocates a Weights structure with the correctly sized
  weights array.  The size of the weights array is determined by the
  two arguments 'source' and 'target' which are the numbers of nodes
  in the source layer and target layer respectively.  The weights are
  assumed to point FROM the source layer TO the target layer.
 This routine must also determine whether or not the weights form a 
  fully connected scheme or a pattern of connections. This is determined
  by looking at the layer spec which is passed as the last argument.
  This spec contains values for the pattern, if one was specified for
  the source/target pair, stored in the "targets" array. NOTE THAT THIS
  ARRAY will specify weights from the SOURCE to the TARGET, but the pattern
  will relate to the LARGER of the two targets. Thus, two checks must be
  made. First, a check must be made to find the larger of the two layers
  so that the decoder matrix can be set for the correct layer, and second
  it must be determined whether or not the pattern FITS THE TWO LAYERS.
  The targets array must be searched in order to find the patterns 
  corresponding to the correct source/target pair. If no pattern exists, 
  then the result is a CONNECT_ALL scheme, otherwise it is a PATTERNED scheme.
 Note that a weight array has no significance without two layers to
  which the weights refer.  Thus, pointers to actual LAYER STRUCTURES
  are passed as arguments to this function from which the size info
  is extracted.

 If any problems occur during weight allocation, then the ERROR value
  is returned as the "type" of the weights.
----------------------------------------------------------------------
*/
BEGIN
   int32    W_set_decoder(), i, num_weights;
   int      s_size, t_size, j;
   Weights  *result;
   Sint     W_random_weight();

   /*---------------------------------------*/
   /* get the source and target layer sizes */
   /*---------------------------------------*/
   s_size = ptr_source->num_nodes;
   t_size = ptr_target->num_nodes;
   
   /*-------------------------------------*/
   /* setup the default weights structure */
   /*-------------------------------------*/
   result = (Weights *) sys_alloc((unsigned)sizeof(Weights));
   result->source_size  = s_size;
   result->target_size  = t_size;
   result->source_layer = ptr_source;
   result->target_layer = ptr_target;
   result->values      = NULL;
   result->decoder     = NULL;
   result->map_area    = 0;
   result->prev_deltaW = NULL;

   /*----------------------------------*/
   /* Find the value of "targets" that */
   /* corresponds to the target layer  */
   /*----------------------------------*/
   for (j = 0; j < ptr_layer_spec->num_targets; j++)
      if (ptr_layer_spec->targets[j][0] == ptr_target->ID)
         break;
         
   /*----------------------------------------------*/
   /* check to see that the target layer was found */
   /*----------------------------------------------*/
   if (j == ptr_layer_spec->num_targets) BEGIN
      sprintf(IO_str, 
              "\n*** INTERNAL ERROR: no TARGET in targets array for Layer %d to %d",
              ptr_source->ID, ptr_target->ID);
      IO_print(0);
      result->type = ERROR;
      return(result);
   ENDIF
   
   /*-----------------------------------------------*/
   /* Now find out the connection scheme used. if   */
   /* targets not 0, then patterned connect is used */
   /*-----------------------------------------------*/
   if (ptr_layer_spec->targets[j][1] != 0) BEGIN      /* PATTERNED CONNECTION */
      result->type = PATTERNED;
      
      /*------------------------------------------*/
      /* set the function pointers depending upon */
      /* a comparison of source and target sizes  */
      /*------------------------------------------*/
      if (s_size > t_size) BEGIN
         result->f_prop   = P_s_pattern_dot_forward;
         result->b_prop   = P_s_pattern_dot_backward;
         result->w_update = P_s_pattern_update_wts;
      ENDIF
      else BEGIN
         result->f_prop   = P_t_pattern_dot_forward;
         result->b_prop   = P_t_pattern_dot_backward;
         result->w_update = P_t_pattern_update_wts;
      ENDELSE
      
      num_weights = W_set_decoder(s_size, t_size, result, ptr_layer_spec->targets[j]);
      if (num_weights == 0) BEGIN
         result->type = ERROR;
         return(result);
      ENDIF
   ENDIF
   
   /*----------------------------------------*/
   /* otherwise a connect-all scheme is used */
   /*----------------------------------------*/
   else BEGIN                                         /* FULLY CONNECTED      */
      result->type = CONNECT_ALL;
      num_weights = s_size * t_size;
      
      /*--------------------------------------------------*/
      /* set function pointers for fully connected scheme */
      /*--------------------------------------------------*/
      result->f_prop   = P_full_dot_forward;
      result->b_prop   = P_full_dot_backward;
      result->w_update = P_full_update_wts;
   ENDELSE
   
   /*--------------------------------------*/
   /* allocate space for the weight values */
   /* then set randomly and zero deltas.   */
   /*--------------------------------------*/
   result->values      = (Sint *) sys_long_alloc((long)(num_weights * sizeof(Sint)));
   result->prev_deltaW = (Sint *) sys_long_alloc((long)(num_weights * sizeof(Sint)));
   for (i = 0; i < num_weights; i++) BEGIN
      result->values[i]      = W_random_weight();
      result->prev_deltaW[i] = 0; 
   ENDIF

   return(result);

END /* W_alloc_weights */


void  W_free_weights(ptr_weights)
Weights  *ptr_weights;
/*
----------------------------------------------------------------------
  Frees just a weights structure. Assumes that the corresponding 
   weights_lst structure has been dealt with by the caller.
----------------------------------------------------------------------
*/
BEGIN

   if (ptr_weights == NULL) return;
   
   /*------------------------*/
   /* free values and deltas */
   /*------------------------*/
   sys_long_free((char *) ptr_weights->values);
   sys_long_free((char *) ptr_weights->prev_deltaW);
   
   /*--------------------------------*/
   /* free patterned info if present */
   /*--------------------------------*/
   if (ptr_weights->decoder != NULL)
      sys_long_free((char *) ptr_weights->decoder);
      
   sys_free((char *) ptr_weights);

END /* W_free_weights */


int32  W_set_decoder(s_size, t_size, ptr_weights, ptr_one_target)
int      s_size, t_size;
Weights  *ptr_weights;
int16    ptr_one_target[];
/*
----------------------------------------------------------------------
  Returns the number of weights which should be allocated or 0 if some
  error is detected.

  Some formulas concerning Nerual Net mappings between layers which
  are other than fully connected.  The backprop net I have at this time
  only allows for one kind of mapping: fully-connected.  The idea here
  is to add some versatality to the connection scheme by allowing the
  user to define "mapping areas" which would essentially model the 
  mapping of nodes which are close together to the same node of the
  intermediate layer.  Overlapping of these areas is permitted to
  allow for border nodes to map to multiple intermediate nodes. The
  idea is to specify two layers, one larger than the other, of 
  dimension M x N (smaller) and U x V (larger).  Then, a mapping area
  is defined of size P x Q and overlap R x S.  The R overlap is in 
  the P dimension and can be at most P-1 (and the R, S-1). The following
  describes how to translate between the weight number and the nodes
  of the LARGER layer.  To map to the smaller layer is easy: you just
  use (weight# DIV mapping-area).

  Note the following formulas:
    M x N   smaller layer dimensions
    U x V   larger layer dimensions
    P x Q   mapping rectangle dimensions
    R x S   overlap of mapping rectangles, in x,y dimensions respectively
 
  This gives the following relationships between M-U and N-V:
    U = P + [(P - R) * (M - 1)]
    V = Q + [(Q - S) * (N - 1)]
 
    or
 
    M = 1 + [(U - P) / (P - R)]
    N = 1 + [(V - Q) / (Q - S)]
 
  Now, given a weight number (the number of weights = P * Q * M * N) 
  we can determine the corresponding node of the input and output layers
  as follows.  First, note that each mapping can be considered to start
  at some point and have a series of rows each the same length.  In 
  addition, these rows will be separated in the Y direction by a distance
  of exactly U (the x dimension of the larger layer). Finally, each
  mapping area is separated in the X direction depending upon the P and
  R values; ie, the width minus the overlap. Thus, if we call the 
  starting node of an area Ts (for "Top start"), then each mapping area
  is separated by P - R in the X direction and Q - S in the Y direction.
  This gives Ts = (row in small layer) * U * (Q-S) as the start of the
  area and Ts + (row in map area) * U as the start of the row which 
  has the node connected to the weight in question. In the column (or X)
  direction, we need to "jump" over the correct number of areas (which
  corresponds to the column number of the smaller layer) and then add
  in the number of columns into the mapping area. This first value is
  Ts + [(col in small layer) * P] - [(col in small layer) * R]. Finally,
  the column of the mapping area is added in. Here are the formulas for
  this calculation:

   area-of-map        = P * Q
   area-number        = weight# DIV area-of-map
   row-in-small-layer = area-number DIV M
   col-in-small-layer = area-number MOD M
   row-in-map-area    = (weight# MOD area-of-map) DIV P
   col-in-map-area    = weight# MOD P

   start-row-of-map   = row-in-small-layer * U * (Q-S)
   rows-into-the-map  = row-in-map-area * U
   start-col-of-map   = col-in-small-layer * (P-R)
   cols-into-the-map  = col-in-map-area
   FINAL RESULT       = (start-row-of-map + rows-into-map)
                        + (start-col-of-map + cols-into-map)

  NOTE: the last parameter is a pointer to a ROW of integers of the
  "targets" array of some layer spec. This amounts to being a list of
  the P, Q, R, and S values for the pattern. Only this row is important,
  thus it may be passed in as "ptr_layer_spec->targets[i]" where "i"
  is the index of the desired target. The result is one row of the targets
  array of the layer spec with dimension 5 (see definition of a layer spec).

  NOTE: see documentation under 'PS_get_layer'
----------------------------------------------------------------------
*/
BEGIN
   int32  i, M, N, U, V, P, Q, R, S, area, num_weights, area_num;
   int32  start_row_map, rows_into_map, start_col_map, cols_into_map;
   int16  *decode_list;

   if (s_size > t_size) BEGIN
      M = (int32) ptr_weights->target_layer->X_dim;
      N = (int32) ptr_weights->target_layer->Y_dim;
      U = (int32) ptr_weights->source_layer->X_dim;
      V = (int32) ptr_weights->source_layer->Y_dim;
   ENDIF
   else BEGIN
      M = (int32) ptr_weights->source_layer->X_dim;
      N = (int32) ptr_weights->source_layer->Y_dim;
      U = (int32) ptr_weights->target_layer->X_dim;
      V = (int32) ptr_weights->target_layer->Y_dim;
   ENDELSE
   P = (int32) ptr_one_target[1];
   Q = (int32) ptr_one_target[2];
   R = (int32) ptr_one_target[3];
   S = (int32) ptr_one_target[4];
   if ((U != (P + ((P-R) * (M-1)))) 
       || (V != (Q + ((Q-S) * (N-1))))) BEGIN
      sprintf(IO_str, "\n*** ERROR: Layer %d will not map evenly to layer %d",
             ptr_weights->source_layer->ID, ptr_weights->target_layer->ID);
      IO_print(0);
      sprintf(IO_str, "\n\nUse the following formulas:");
      IO_print(0);
      sprintf(IO_str, "\n   X_big_layer = X_pattern + (X_pattern - X_overlap) * (X_small_layer - 1)");
      IO_print(0);
      sprintf(IO_str, "\n   Y_big_layer = Y_pattern + (Y_pattern - Y_overlap) * (Y_small_layer - 1)");
      IO_print(0);
      sprintf(IO_str, "\n\nNet specification produced the following:");
      IO_print(0);
      sprintf(IO_str, "\n   X_big_layer=%ld;  X_pattern=%ld;  X_overlap=%ld;  X_small_layer=%ld",
             U, P, R, M); 
      IO_print(0);
      sprintf(IO_str, "\n   Y_big_layer=%ld;  Y_pattern=%ld;  Y_overlap=%ld;  Y_small_layer=%ld\n",
             V, Q, S, N);
      IO_print(0); 
      return(0);
   ENDIF
   area = P * Q;
   num_weights = area * M * N;
   decode_list = (int16 *) sys_long_alloc((long)(num_weights * sizeof(int16)));
   ptr_weights->decoder = decode_list;
   for (i = 0; i < num_weights; i++) BEGIN
      area_num = i / area;
      start_row_map = (area_num / M) * U * (Q - S); 
      rows_into_map = ((i % area) / P) * U;
      start_col_map = (area_num % M) * (P - R);
      cols_into_map = (i % P);
      *decode_list++ = (int16)(start_row_map + rows_into_map 
                               + start_col_map + cols_into_map);
   ENDFOR
   ptr_weights->map_area = (int) area;
   return(num_weights);

END  /* W_set_decoder */


void W_weight_range(max_wts)
int  max_wts;
/*
----------------------------------------------------------------------
 This routine calculates the range of allowable weight values, given  
  a number indicating the maximum number of weights coming into a     
  node in the net.  The formula for this calculation is:              
      10 = .5 * (num_wts) * (maximum allowable weight)                
  for more discussion on the above formula, see the documentation with
  'check_layer_sizes' in buildnet.c                                        
 All that is done here is to make the calculation, solving for the    
  maximum allowable weights, and store the result in 'weight_max'     
  which is visible in this file only.                        
----------------------------------------------------------------------
*/
BEGIN
   char   temp_str[MAX_WORD_SIZE];
   float  result, temp, min_weight_value;
   
   /*-------------------------------------*/
   /* precalculate default minimum weight */
   /*-------------------------------------*/
   min_weight_value = 1.0 / ((float)WTS_SCALE);

   /*----------------------------------*/
   /* calculate default maximum weight */
   /*----------------------------------*/
   result = temp = (10.0 / 0.5 / (float)max_wts);
   if (result < min_weight_value)
      result = min_weight_value;
   
#if  !DELIVERY
   /*-------------------------------------*/
   /* if not in DELIVERY mode, prompt the */
   /* user using temp as a default        */
   /*-------------------------------------*/

/*   sprintf(IO_wkstr, "\n   Enter maximum weight value(default =%%.f): ");
   IO_insert_format(IO_wkstr);
   sprintf(IO_str, IO_wkstr, temp);
   IO_print(0);
   IO_my_get_string(temp_str); */

   temp_str[0] = ENDSTRING;

   if (temp_str[0] != ENDSTRING)
      if (sscanf(temp_str, "%f", &result) != 1) BEGIN
         sprintf(IO_str, "\n   sorry, I don't understand.  Try again: ");
         IO_print(0);
         result = IO_my_get_float();
      ENDIF
   /*-----------------------------------------*/
   /* check if the value given is appropriate */
   /* Use default in such a case              */
   /*-----------------------------------------*/
   if ((result > 32.0) || (result <= 0)) BEGIN
      sprintf(IO_str, "   OUT OF RANGE(> 0, <= 32). Default value used.");
      IO_print(0);
      result = temp;
   ENDIF
#endif

   /*--------------------------------------------*/
   /* in any event, set the weight max to result */
   /* and set result to the minimum weight value */
   /*--------------------------------------------*/
#if  USE_SCALED_INTS
   weight_max = (Sint)(result * (float)SINT_SCALE);
#else
   weight_max = result;
#endif
   result = min_weight_value;
   
#if  !DELIVERY
   /*--------------------------------------------*/
   /* if not delivery, prompt a second round for */
   /* the minimum weight value. Put valin result */
   /*--------------------------------------------*/

/*   sprintf(IO_wkstr, "\n   Enter minimum weight value(default =%%.f): ");
   IO_insert_format(IO_wkstr);
   sprintf(IO_str, IO_wkstr, result);
   IO_print(0);
   IO_my_get_string(temp_str); */

   temp_str[0] = ENDSTRING;

   if (temp_str[0] != ENDSTRING)
      if (sscanf(temp_str, "%f", &result) != 1) BEGIN
      sprintf(IO_str, "\n   sorry, I don't understand.  Try again: ");
      IO_print(0);
      result = IO_my_get_float();
   ENDIF
   /*-----------------------------------------*/
   /* check if the value given is not in the  */
   /* appropriate. Use default in such a case */
   /*-----------------------------------------*/
   if ((result >= 32.0) || (result < 0)) BEGIN
      sprintf(IO_str, "   OUT OF RANGE(>= 0, <= 32). Default value used.");
      IO_print(0);
      result = min_weight_value;
   ENDIF
#endif

   /*----------------------------------*/
   /* set the minimum weight to result */
   /*----------------------------------*/
#if  USE_SCALED_INTS
   weight_min = (Sint)(result * (float)SINT_SCALE);
#else
   weight_min = result;
#endif
   
   /*------------------------------------*/
   /* if max < min, then set base to min */
   /* Otherwise, leave it as max - min   */
   /*------------------------------------*/
   if ((weight_range = weight_max - weight_min) <= 0) BEGIN
      weight_range = weight_max;
      weight_min = 0;
   ENDIF

END /* W_weight_range */


Sint  W_random_weight()
/*
----------------------------------------------------------------------
 returns a positive or negative random number between weight_max and  
  weight_min.                                                         
----------------------------------------------------------------------
*/
BEGIN      
   D_Sint  temp;
   
#if  USE_SCALED_INTS
   /*-------------------------------------------*/
   /* for integers, we generate a random weight */
   /* by using the weight_range as a MOD value  */
   /* on the rand function. This works since we */
   /* know rand returns int and we know Sints   */
   /* are 16-bits long.                         */
   /*-------------------------------------------*/
   temp = (D_Sint)(rand() % weight_range + weight_min);
   
   /*------------------------------------------*/
   /* rand is again used to determine the sign */
   /*------------------------------------------*/
   if ((rand() % 1000) > 500)
      temp = temp * -1;
   
   /*--------------------------------------------------*/
   /* the result is converted to a Sint by multiplying */
   /* by the scale factor and dividing by precision    */
   /*--------------------------------------------------*/
   return( (Sint)((temp * SINT_SCALE) / WTS_SCALE) );
#else
   /*-------------------------------------------*/
   /* divide a random number by a number large  */
   /* enough to make the result between 0 and 1 */
   /* Note the "%" mod operator which keeps the */
   /* results of the rand small enough to make  */
   /* sure the divide returns a number < 1.     */
   /* Most rand functions return a number from  */
   /* 0 - 32767 but some (like the VAX) don't.  */
   /*-------------------------------------------*/
   temp = (float)(rand() % 32768) / (float)(1L<<15);
      
   /*---------------------------------------------*/
   /* convert temp to a weight by multiplying it  */
   /* by the range of possible weights and adding */
   /* that to the minimum weight value.           */
   /*---------------------------------------------*/
   temp = weight_min + weight_range * temp;

   /*--------------------------------------*/
   /* then pull the same old trick to make */
   /* it randomly positive or negative.    */
   /*--------------------------------------*/
   if ((rand() % 1000) > 500)
      temp = temp * -1;
   return((Sint)temp);
#endif

END /* W_random_weight */


Weights_lst  *W_alloc_weights_lst(ptr_weights)
Weights  *ptr_weights;
/*
----------------------------------------------------------------------
 Given a pointer to a Weights structure, this routine allocates a     
  Weights_lst structure and assigns its 'value' field to the incoming 
  argument.  As with other routines like this one (see layer.h), the  
  unassigned pointers are set to NULL.                                
----------------------------------------------------------------------
*/
BEGIN
   Weights_lst  *result;

   result = (Weights_lst *) sys_alloc((unsigned)sizeof(Weights_lst));
   result->value = ptr_weights;
   result->next  = NULL;

   return(result);

END /* W_alloc_weights_lst */


Weights_lst  *W_insert_before(ptr_node, ptr_lst)
Weights_lst  *ptr_node, *ptr_lst;
/*
----------------------------------------------------------------------
 As with the layer.h routines, this guy takes in two arguments, one   
  which is a pointer to a new element to be inserted into the list    
  indicated by the second argument.  The second argument is assumed   
  to be pointing to the element BEFORE WHICH the new node should be   
  inserted. (see documentation for L_insert_before).                  
----------------------------------------------------------------------
*/
BEGIN
   if (ptr_node == NULL)
      return(ptr_lst);
   else if (ptr_lst == NULL)
      return(ptr_node);
   else BEGIN
      ptr_node->next = ptr_lst;        /* set up new node pointer     */
      return(ptr_node);                /* return ptr to inserted node */
   ENDELSE

END /* W_insert_before */


Weights_lst  *W_insert_after(ptr_node, ptr_lst)
Weights_lst  *ptr_node, *ptr_lst;
/*
----------------------------------------------------------------------
 As with the layer.h routines, this guy takes in two arguments, one   
  which is a pointer to a new element to be inserted into the list    
  indicated by the second argument.  The second argument is assumed   
  to be pointing to the element AFTER WHICH the new node should be    
  inserted. (see documentation for L_insert_after).                   
----------------------------------------------------------------------
*/
BEGIN
   if (ptr_node == NULL)
      return(ptr_lst);
   else if (ptr_lst == NULL)
      return(ptr_node);
   else BEGIN
      ptr_node->next = ptr_lst->next;  /* set up new node pointer     */
      ptr_lst->next  = ptr_node;       /* reset prev node next ptr    */
      return(ptr_lst);                 /* return ptr to list          */
   ENDELSE

END /* W_insert_after */


void  W_free_weights_lst(ptr_lst)
Weights_lst  *ptr_lst;
/*
----------------------------------------------------------------------
  Frees the weights list structures and the corresponding weight arrays
   to which the list elements refer.
----------------------------------------------------------------------
*/
BEGIN
   void          W_free_weights();
   Weights_lst  *last;
   
   if (ptr_lst == NULL) return;
   
   while (ptr_lst != NULL) BEGIN
      W_free_weights(ptr_lst->value);
      last = ptr_lst;
      ptr_lst = ptr_lst->next;
      sys_free((char *) last);
   ENDWHILE

END /* W_free_weights_lst */


void  W_show_weights(ptr_weights)
Weights  *ptr_weights;
/*
----------------------------------------------------------------------
 Given a pointer to a set of weights, this guy prints out the weight  
  values in COLUMN major order (because that is how we assume the 2-d 
  array is stored in 1-d format).  Thus, if we had a set of weights   
  between layer A and layer B we would see weights in the following   
  order:  A1,B1; A2,B1; A3,B1;...A1,B2; A2,B2; A3,B2;...An-1,Bm;      
  An,Bm  where 'n' and 'm' are the sizes of layers A and B respec-    
  tively (when the connection scheme is CONNECT_ALL).
 Since our array is stored COLUMN MAJOR, we consider adjacent elements
  to be in the same column.  We use our first index, j, to indicate   
  which column we are on (starting with 0).  Now, moving from one     
  column to the next is tricky, because it has to do with the ROW SIZE
  and not the column size.  Think of it this way: for each set of     
  values, we want to print out i, or 's_size', elements.  Then we want
  to jump to the next set of values.  Since we just printed s_size    
  elements, it makes sense to jump s_size elements down and start the 
  whole thing again.  't_size' only has relevance in knowing HOW MANY 
  TIMEs to do the jumping!!!                
 If the connection scheme is PATTERNED, then the printout follows the
  smaller of the two layers. That is, since the weights are stored in
  relation to the smaller layer (in groups of 'map_area' size), the 
  printout also follows this format. 
----------------------------------------------------------------------
*/
BEGIN
   int    i, j, s_size, t_size, area;
   Sint   *wt_values;
   int16  *ptr_decoder;
   float  t1;                    /* temporary value for Sint conversion */

   s_size = ptr_weights->source_size;
   t_size = ptr_weights->target_size;
   
   if (ptr_weights->type == CONNECT_ALL)
      for (j = 0; j < t_size; j++) BEGIN
         for (i = 0; i < s_size; i++) BEGIN
            t1 = C_Sint_to_float( ptr_weights->values[i + (j * s_size)] );
            sprintf(IO_wkstr, "weight %d,%d = %%.f  ", i, j);
            IO_insert_format(IO_wkstr);
            sprintf(IO_str, IO_wkstr, t1);
            if (IO_more(0) == ERROR) return;
            if ((i+1) % 3 == 0) BEGIN
               sprintf(IO_str, "\n");
               if(IO_more(0) == ERROR) return;
            ENDIF
         ENDFOR
         sprintf(IO_str, "\n\n");
         if(IO_more(0) == ERROR) return;
      ENDFOR
   else BEGIN
      area      = ptr_weights->map_area;
      wt_values = ptr_weights->values;
      ptr_decoder = ptr_weights->decoder;
      if (t_size < s_size)
         for (j = 0; j < t_size; j++) BEGIN
            for (i = 0; i < area; i++) BEGIN
               t1 = C_Sint_to_float(*wt_values++);
               sprintf(IO_wkstr, "weight %d,%d = %%.f  ", *ptr_decoder++, j);
               IO_insert_format(IO_wkstr);
               sprintf(IO_str, IO_wkstr, t1);
               if(IO_more(0) == ERROR) return;
               if ((i+1) % 3 == 0) BEGIN
                  sprintf(IO_str, "\n");
                  if(IO_more(0) == ERROR) return;
               ENDIF
            ENDFOR
            sprintf(IO_str, "\n\n");
            IO_print(0);
            if(IO_more(0) == ERROR) return;
         ENDFOR
      else
         for (i = 0; i < s_size; i++) BEGIN
            for (j = 0; j < area; j++) BEGIN
               t1 = C_Sint_to_float(*wt_values++);
               sprintf(IO_wkstr, "weight %d,%d = %%.f  ", i, *ptr_decoder++);
               IO_insert_format(IO_wkstr);
               sprintf(IO_str, IO_wkstr, t1);
               if(IO_more(0) == ERROR) return;
               if ((j+1) % 3 == 0) BEGIN
                  sprintf(IO_str, "\n");
                  if(IO_more(0) == ERROR) return;
               ENDIF
            ENDFOR
            sprintf(IO_str, "\n\n");
            if(IO_more(0) == ERROR) return;
         ENDFOR
   ENDELSE

END /* W_show_weights */
