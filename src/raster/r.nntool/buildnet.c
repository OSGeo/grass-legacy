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
  Code For Construction Of Net Structures (Prefix = B_)
----------------------------------------------------------------------
  This code is divided into 4 major sections:

  (1) include files
  (2) externed functions
  (3) externed global variables
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
extern  float        LR_learn_default();
extern  float        LR_momentum_default();
extern  float        LR_scale_default();
extern  float        LR_learn_from_scale();

extern  Layer        *L_alloc_layer();
extern  Layer_lst    *L_alloc_layer_lst();
extern  Layer_lst    *L_insert_before();
extern  void         L_free_layer();
extern  void         L_free_layer_lst();

extern  Weights      *W_alloc_weights();
extern  Weights_lst  *W_alloc_weights_lst();
extern  Weights_lst  *W_insert_before();
extern  void         W_weight_range();

extern  void         IO_my_get_string();
extern  float        IO_get_default_float();
extern  void         IO_print();
extern  void         IO_insert_format();

extern  Layer_spec   *PS_get_layer();
extern  void         PS_reset_for_layer_parse();
extern  int          PS_global_spec_check();

extern  Sint         C_float_to_Sint();
extern  char         *sys_alloc();
extern  void         sys_free();


/*
----------------------------------------------------------------------
  EXTERNED GLOBALS
----------------------------------------------------------------------
  Note that one global here is externed ONLY if we are in delivery mode.
  If so, this global is used to indicate whether or not the delivery
  network uses bias values. That is, the delivery code will define the
  "bias_flag" global properly to indicate the presence of biases.
----------------------------------------------------------------------
*/
extern float  global_learn_base;
extern float  global_momentum;
extern char   IO_str[MAX_LINE_SIZE];
extern char   IO_wkstr[MAX_LINE_SIZE];


/*
======================================================================
  ROUTINES IN BUILDNET.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "B_" indicating that it is defined in the 
  "buildnet.c" file.  The types returned by the routines are also shown 
  here so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
    Net *                       B_create_net                            
    int                         B_process_config                        
    int                         B_create_wts                         
    int                         B_check_layer_sizes   
    void                        B_setup_net                             
    void                        B_setup_learning  
    void                        B_set_layer_learning  
    void                        B_set_layer_momentum  
    Net *                       B_free_net                          
======================================================================
*/


Net  *B_create_net(id_num, file_name)
int    id_num;                   /* identificaion # of net         */
char   file_name[];              /* file with configuration of net */
/*
----------------------------------------------------------------------
 The routine generates a net in the following way. As each Layer_spec 
  is processed, a Layer is created but WITHOUT any connections to     
  weight arrays.  The trick is that the weight arrays cannot be made  
  until all of the Layers are made! Thus as the layers are processed, 
  an intermediate array of weight connections between layers is also  
  generated.  Then, when all the layers have been processed, this     
  array of weight connections is traversed and the Weights are created.
 The array of weight connections (called 'wts_matrix') is a 2-d array 
  whose first dimension represents SOURCE layers, and whose second    
  dimension represents TARGET layers.  If a SOURCE-to-TARGET weight   
  connection exists, then the corresponding entry in the array will    
  have the value 1; otherwize, the value is zero.                     
 The work of this routine is split up among subroutines: first, one  
  named 'B_process_config' which creates the intermediate wts_matrix; 
  then 'B_create_wts' which allocates space for the weights and sets up 
  the pointers between the layers and weights; and finally 'B_setup_net'
  which sets up the values in the final net structure.                
 Notice how 'result->ID' is initially set to -1.  This translates to  
  a bad net and is this routine's signal for returning an error.     
  Thus if 'B_process_config' hits a snag in reading from the file, or 
  if the file contains too many layer definitions, this routine can   
  detect it and send a signal back to its caller.                     
----------------------------------------------------------------------
*/
BEGIN
   int         B_process_config(), B_create_wts();
   void        B_setup_net();
   Net         *result;                              /* the end product */
   Layer_spec  *wts_matrix[MAX_LAYERS][MAX_LAYERS];  /* for making wts  */
   Layer       *layer_ptrs[MAX_LAYERS];        /* holds ptrs to layers  */
   int         num_layers, i, j;               /* actual # of layers    */

   result = (Net *) sys_alloc((unsigned)sizeof(Net));
   result->ID            = ERROR;
   result->num_layers    = 0;
   result->input_layer   = NULL;
   result->output_layer  = NULL;
   result->hidden_front  = NULL;
   result->hidden_back   = NULL;
   result->use_biases    = TRUEE;
   result->num_inputs    = 0;
   result->num_outputs   = 0;
   result->num_io_pairs  = 0;

   for (i = 0; i < MAX_LAYERS; i++) BEGIN      /* initialize wts_matrix */
      layer_ptrs[i] = NULL;                    /* and layer_ptrs array  */
      for (j = 0; j < MAX_LAYERS; j++)         /* to all 'NULL' values  */
         wts_matrix[i][j] = NULL;
   ENDFOR

   if (B_process_config(file_name, wts_matrix, layer_ptrs, &num_layers) != OKAY)
      return(NULL);
   if (B_create_wts(wts_matrix, layer_ptrs, num_layers) != OKAY)
      return(NULL);
   B_setup_net(result, id_num, layer_ptrs, num_layers);

   return(result);

END /* B_create_net */


int  B_process_config(file_name, wts_matrix, layer_ptrs, ptr_num_layers)
char        file_name[];
Layer_spec  *wts_matrix[];
Layer       *layer_ptrs[];
int         *ptr_num_layers;
/*
----------------------------------------------------------------------
 This guy processes the configuration passed in from the create_net   
  routine above, creating Layers and the intermediate wt_matrix in    
  the process.                                                        
 Two important notes. First, note that the last argument is a pointer 
  to an int. I realize that this procedure could have been made a     
  function which returned the value of the int, but nearly all of the 
  incoming args get modified by this routine and so I chose to remain 
  consistent in how the information calculated was passed back from   
  this routine (namely, via the parameters themselves).               
  Second, note that this guy opens the file specified by 'file_name'  
  and then uses the routine 'PS_get_layer' to read in the specs for a 
  file.  Like the 'B_create_net' routine, the IO routine will return  
  a negative number as one of the structure elements to indicate a    
  problem in reading the file (ie, config->OKAY = ERROR).  Aside from  
  a reading error or an incomplete layer spec, this routine also will 
  check for too many layers in the input file and return an error. By 
  convention, routines will return an integer indicating an error,    
  unless they must return a structure, in which case an element of the
  structure will be set to ERROR.    
                                    
 This routine returns OKAY if everything goes ok.  Otherwise, ERROR is   
  returned.                                                           
----------------------------------------------------------------------
*/
BEGIN
   FILE        *fp;
   int         i, DONE = FALSEE;
   Layer_spec  *config;

   if((fp = fopen(file_name, "rt")) == NULL) BEGIN
      sprintf(IO_str, "\n*** can't open file %s ***\n", file_name);
      IO_print(0);
      return(ERROR);
   ENDIF
   *ptr_num_layers = 0;                       /* initialize num_layers */

   PS_reset_for_layer_parse();
   if (PS_global_spec_check(fp) == ERROR)
      return(ERROR);
   config = PS_get_layer(fp);                 /* get first layer specs */
   while ((DONE == FALSEE) && (config->status != ERROR)) BEGIN
      if(*ptr_num_layers == MAX_LAYERS) BEGIN
         sprintf(IO_str, "\n*** too many layers ***\n");
	 Menu_msg(IO_str);
/*         IO_print(0); */
         return(ERROR);
      ENDIF
      (*ptr_num_layers)++; 
      layer_ptrs[config->ID] = L_alloc_layer(config);
      for (i = 0; i < config->num_targets; i++) 
         wts_matrix[config->ID * MAX_LAYERS + config->targets[i][0]] = config;
      if (config->status == EOF) DONE = TRUEE; 
      else config = PS_get_layer(fp);
   ENDWHILE
   fclose(fp);
   if (config->status == ERROR) 
      return(ERROR);
   else return(OKAY);

END /* B_process_config */


int  B_create_wts(wts_matrix, layer_ptrs, num_layers)
Layer_spec  *wts_matrix[];
Layer       *layer_ptrs[];
int         num_layers;
/*
----------------------------------------------------------------------
 Refer to comments under create_net.                                  
                                                                      
 This routine takes the input array 'wts_matrix' and creates Weight   
  structures with the appropriate information.  It also links these   
  structures into the corresponding lists within the Layer structures 
 NOTE that no particular order is necessary when linking the weights  
  into the Layers.  Thus, new weights are simply inserted into the    
  the front of the lists by using the 'W_insert_before' routine  
     
 Returns OKAY if the weights can be created, ERROR otherwise.                
----------------------------------------------------------------------
*/
BEGIN
   Weights      *new_wts;
   Weights_lst  *new_wts_node;
   Layer        *source, *target;
   Layer_spec   *ptr_layer_spec;
   int          i, j, B_check_layer_sizes();

   if (B_check_layer_sizes(wts_matrix, layer_ptrs, num_layers) == ERROR) 
      return(ERROR);

   for (i = 0; i < num_layers; i++) BEGIN
      source = layer_ptrs[i];
      for (j = 0; j < num_layers; j++) BEGIN
         ptr_layer_spec = wts_matrix[i * MAX_LAYERS + j];
         if (ptr_layer_spec != NULL) BEGIN
            target = layer_ptrs[j];
            if ((source == NULL) || (target == NULL)) BEGIN
               sprintf(IO_str, "\n*** Warning: can't create weights between %d, %d ***",
                      i, j);
               IO_print(0);
               sprintf(IO_str, "\n*** one of the layers was never created ***\n");
               IO_print(0);
            ENDIF
            else BEGIN
               new_wts = W_alloc_weights(source, target, ptr_layer_spec);
               if (new_wts->type == ERROR) return(ERROR);
               new_wts_node = W_alloc_weights_lst(new_wts);
               source->out_weights = 
                 W_insert_before(new_wts_node, source->out_weights);
               new_wts_node = W_alloc_weights_lst(new_wts);
               target->in_weights = 
                 W_insert_before(new_wts_node, target->in_weights);
             ENDELSE
         ENDIF
      ENDFOR
   ENDFOR
   return(OKAY);

END /* B_create_wts */


int  B_check_layer_sizes(wts_matrix, layer_ptrs, num_layers)
Layer_spec  *wts_matrix[];
Layer       *layer_ptrs[];
int         num_layers;
/*
----------------------------------------------------------------------
 This routine checks through all of the layers, noting the layer with 
  the most nodes.  This number is then used to calculate the weight   
  range for initializing the random weights.  The whole concept for   
  this routine comes from the formula for calculating the output of a 
  node:  output = f(SUM) where SUM = sum i,j [ Wij * Oi].  That is,   
  you sum all of the incoming weight/output products, and the result  
  is a sum fed through the semi linear activation function above.     
  Problems arise if this SUM is much greater than 10 because the      
  activation function starts to reach 0 or 1 at that point.  Thus, we 
  want to set our weights so that problem doesn't occur.  Taking the  
  worst case, we would have all of the output values "Oi" at .9, and  
  all of the weights the same.  That gives:    
                         
       10 = .9 * (num_weights) * (weight value).   
                          
  Two conclusions can be drawn from the above.  First, if you let the 
  weights be as small as possible, you can find the maximum number of 
  nodes allowable in a layer, since this program generates a weight   
  for each node of a layer.  That makes the maximum number of nodes   
  about equal to 10 * 10^precision where 'precision' is equal to the  
  the number of significant digits after the decimal point that the   
  the Sint configuration allows (Thus for a Sint with 3 sig figs, you 
  get a maximum layer size of 10 * 10^3 or 10,000).                   
  The second conclusion we can draw is that, given the maximum number 
  of nodes in a layer, we can determine the range of values which the 
  initial weights should have.  That is what is done here.            
 Note: this routine returns 0 if all goes well, otherwise 1.          
----------------------------------------------------------------------
*/
BEGIN
   int         i, j, k;
   int         max, temp;
   Layer_spec  *ptr_layer_spec;
   Layer       *source, *target;

   max = 0;
   /*----------------------------------------*/
   /* find the maximum number of connections */
   /* per node by looping through all the    */
   /* layer specs in the wts_matrix variable */
   /*----------------------------------------*/
   for (i = 0; i < num_layers; i++) BEGIN
      source = layer_ptrs[i];
      for (j = 0; j < num_layers; j++) BEGIN
         ptr_layer_spec = wts_matrix[i * MAX_LAYERS + j];
         if (ptr_layer_spec != NULL) BEGIN
            target = layer_ptrs[j];
            /*----------------------------------------------------*/
            /* find the corresponding target in the targets array */
            /*----------------------------------------------------*/
            for (k = 0; k < ptr_layer_spec->num_targets; k++)
               if (ptr_layer_spec->targets[k][0] == target->ID)
                  break;
            /*----------------------------------*/
            /* now we check the connection type */
            /* by looking at targets array [0]  */
            /* if 0 (connect all) then the num  */
            /* of connections = source_nodes.   */
            /*----------------------------------*/        
            if (ptr_layer_spec->targets[k][1] == 0)
               temp = source->num_nodes;
            else
               /*-------------------------------------*/
               /* if targets[k][0] != 0, then we have */
               /* a patterned scheme. targets[1] and  */
               /* targets[2] hold P & Q, respecitvely */
               /* and P * Q = num of connections      */
               /*-------------------------------------*/
               temp = ptr_layer_spec->targets[k][1]
                       * ptr_layer_spec->targets[k][2];
            /*----------------------------------*/
            /* use temp to set the max_incoming */
            /* field of the TARGET layer        */
            /*----------------------------------*/
            target->max_incoming = temp;
            max = ((temp > max) ? temp : max);
         ENDIF
      ENDFOR
   ENDFOR

   /*-----------------------------------*/
   /* check the number of connections   */
   /* to see if it's too large for NETS */
   /*-----------------------------------*/
   if (max > MAX_NODES) BEGIN
      sprintf(IO_str, "\n*** Layers are too large; reduce the nodes in each layer to within %d ***\n",
              MAX_NODES);
      IO_print(0);
      return(ERROR);
   ENDIF
   W_weight_range(max);
   return(OKAY);

END /* B_check_layer_sizes */


void  B_setup_net(ptr_net, id_num, layer_ptrs, num_layers)
Net    *ptr_net;
int    id_num;
Layer  *layer_ptrs[];
int    num_layers;
/*
----------------------------------------------------------------------
 This routine collects all the data created by the other two routines 
  and assembles it into the net structure pointed to by 'ptr_net'. The 
  final product is NOT a completely initialized net structure. The    
  input/output pairs for learning are not yet present.                
 There really is nothing hard going on here; most of the work has     
  already been done at this point by 'B_process_config' and           
  'B_create_wts'.                                                     
  All that is left is to proceed through the layer_ptrs array and in- 
  sert those layers into the net.  Of course, the id, learn_rate, and 
  momentum must also be set.                                           
 Two notes: first, notice that the learn_rates and momentum must be con-
  verted into Sint format, since this is our internal representation  
  for floating point numbers (see Sint in 'netpool.h'). Also, note    
  that it is ASSUMED THAT THE LAYERS IN THE layer_ptrs ARRAY ARE IN   
  THE ORDER THEY SHOULD BE COMPUTED.  This assumption carries all the 
  way back to the actual input, since that input is also assumed to   
  be in the correct order (ie, nothing is done at ANY stage of the net
  creation to set an order).  This is absolutely essential for the    
  calculations on the net to be done correctly.  Lastly, since order  
  is so important, we cannot simply use an 'insert_before' routine for
  placing the layers into the net.  Instead, we have to work BACKWARDS
  from the end of the array while using 'insert_before'.              
 Also, note that at least two layers are assumed to be present in the 
  array of layer pointers, namely the input and output layers.  A net 
  without at least these would be ridiculous anyway.  By convention,  
  these two layers are assumed to be layer 0 and 1, respectively, in  
  the layer_ptrs array.  Layers 0 and 1 ARE linked into the hidden    
  list (even though they are not hidden) since they are both included 
  included in the forward and backward propagation steps. Actually,   
  layer 1 is included in forward prop, layer 0 in backward prop. Once 
  all the layers have been linked into the hidden list, the two ends  
  of the list are moved in by one.  This is because we want the front 
  of the list to point to the first layer of forward propagation, not 
  layer 0.  The same is true for backward propagation at the other end
  of the list.                                                        
----------------------------------------------------------------------
*/
BEGIN
   void       B_setup_learning();
   Layer_lst  *new_layer_node;
   int        i;
   char       tmp_str[MAX_WORD_SIZE];

   /*-----------------------------------*/
   /* if no input layer or output layer */
   /* specified, then return error      */
   /*-----------------------------------*/
   if (layer_ptrs[IN_LAYER] == 0) BEGIN
      sprintf(IO_str, "\n*** ERROR, no input layer ***\n");
      IO_print(0);
      return;
   ENDIF
   if (layer_ptrs[OUT_LAYER] == 0) BEGIN
      sprintf(IO_str, "\n*** ERROR, no output layer ***\n");
      IO_print(0);
      return;
   ENDIF

   /*----------------------*/
   /* setup learning rates */
   /*----------------------*/
   B_setup_learning(layer_ptrs, num_layers);

#if  !DELIVERY
   /*------------------------------------------------*/
   /* if not in delivery mode, prompt for bias value */
   /*------------------------------------------------*/
   sprintf(IO_str, "   Use biases in network(y/n, default=y)? ");
   Curses_prompt_gets(IO_str, tmp_str);

/*   IO_print(0); 
   IO_my_get_string(tmp_str); */
   
   if (tmp_str[0] != ENDSTRING)
      if ( (tmp_str[0] == 'y') || (tmp_str[0] == 'Y') )
         ptr_net->use_biases = TRUEE;
      else
         ptr_net->use_biases = FALSEE;
#endif

   /*--------------------------------------*/
   /* set the rest of the network elements */
   /*--------------------------------------*/
   ptr_net->ID            = id_num;
   ptr_net->num_layers    = num_layers;
   ptr_net->input_layer   = layer_ptrs[IN_LAYER];
   ptr_net->output_layer  = layer_ptrs[OUT_LAYER];
   
   /*-------------------------------------------*/
   /* link the output layer to the hidden lists */
   /*-------------------------------------------*/
   ptr_net->hidden_back   = L_alloc_layer_lst(layer_ptrs[OUT_LAYER]);
   ptr_net->hidden_front  = L_insert_before(ptr_net->hidden_back, NULL); 

   /*--------------------------------------*/
   /* link the hidden layers into the list */
   /*--------------------------------------*/
   for (i = (num_layers-1); i > 1; i--) BEGIN
      new_layer_node = L_alloc_layer_lst(layer_ptrs[i]);
      ptr_net->hidden_front = L_insert_before(new_layer_node,
                                             ptr_net->hidden_front);
   ENDFOR
   
   /*-------------------------------------------*/
   /* link the rest of the layers into the list */
   /*-------------------------------------------*/
   new_layer_node = L_alloc_layer_lst(layer_ptrs[IN_LAYER]);
   ptr_net->hidden_front = L_insert_before(new_layer_node,
                                          ptr_net->hidden_front);

   ptr_net->hidden_front  = ptr_net->hidden_front->next;
   ptr_net->hidden_back   = ptr_net->hidden_back->prev;

END /* B_setup_net */


void  B_setup_learning(layer_ptrs, num_layers)
Layer  *layer_ptrs[];
int    num_layers;
/*
----------------------------------------------------------------------
  This routine loops through the layers twice. The first time it records
  the maximum number of weights found coming into any layer for calculating
  default values. This is done if global values were not specified by
  the user. Next, the second loop through the layers is performed so 
  that all layers are given some learning rate and momentum. 
  Two items are of note here. First, note that if the user does not
  enter a scale value then a default is NOT used here. Second, note
  that the "cur_learn" field IS NOT SET BEFORE THIS ROUTINE CALL. Thus,
  the second loop through the layers must be performed even if the global
  values were defined by the user (I had originally thought that this
  second loop could be skipped if both globals were already defined since
  the B_process_config routine would have already set the learn and 
  momentum values base on the global defaults. But, alas, I forgot about
  the cur_learn field!).
----------------------------------------------------------------------
*/
BEGIN
   void   B_set_layer_learning(), B_set_layer_momentum();
   int    i, max_wts;
   Layer  *cur_layer;

   /*------------------------------------------*/
   /* loop through layers to find max weights  */
   /* then calc learning and momentum defaults */
   /*------------------------------------------*/
   max_wts = 0;
   for (i = 1; i < num_layers; i++)      
      max_wts = (layer_ptrs[i]->max_incoming > max_wts)
                ? layer_ptrs[i]->max_incoming : max_wts;
   
   /*----------------------------------------*/
   /* loop through all layers, setting the   */
   /* learn rate and momentum if not defined */
   /*----------------------------------------*/
   for (i = 1; i < num_layers; i++) BEGIN
      cur_layer = layer_ptrs[i];
      B_set_layer_learning(cur_layer, max_wts);
      B_set_layer_momentum(cur_layer, max_wts);
   ENDFOR
   
END /* B_setup_learning */


void  B_set_layer_learning(ptr_layer, max_wts)
Layer  *ptr_layer;
int     max_wts;
/*
----------------------------------------------------------------------
  Given a pointer to a layer and the maximum weights connected to a
  node in the network, this routine goes through the motion of checking
  the state of the current layer to see if the user should be prompted
  for learning rates and/or scale values. Essentially, there are eight
  possible cases, depending upon the states of three values: the global
  learning base, the learn_base for the layer, and the learn_scale for
  the layer. The actions for these cases are as follows: (note that
  G = global_learn_base, L=layer learn_base, S=layer scale_base, and
  "N" means not defined, "Y" means defined)
  
  G L S   Action taken
  -----   ------------
  Y Y Y   do nothing
  Y Y N   set learn_scale = 0
  Y N Y   set learn_base = G
  Y N N   set learn_base = G, set learn_scale = 0
  N Y Y   do nothing
  N Y N   set learn_scale = 0
  N N Y   prompt for G/L; global default prompt for G, local for L
  N N N   prompt for G/L/S; global for G, local for L, default for S,L
  
  Note that default values can be figured either on a global basis
  (using max_wts) or on a local basis based on the maximum incoming 
  weights FOR THE LAYER. Also, a default scale can be figured from a
  learning rate and vice-versa when needed. 
  Finally, note that this routine keeps track of a "prompt_state"
  variable so that it only prompts the user once to ask whether or not
  he/she wants a global learning rate. Using a static variable, one 
  can reset its state every time a new net is created and its value 
  will be retained between calls for each layer of a single network.
----------------------------------------------------------------------
*/
BEGIN
   static float  prompt_state = TRUEE;
   float         learn, scale;
   char          tmp_str[MAX_WORD_SIZE];
   
   /*--------------------------------------*/
   /* reset the state of g_learn_state if  */
   /* we are starting a new network. If so */
   /* then we will be starting w/layer 1   */
   /*--------------------------------------*/
   if (ptr_layer->ID == 1)
      prompt_state = TRUEE;
   
   /*-------------------------------------------*/
   /* handles the top half of the table in the  */
   /* comment, setting the learning base to the */
   /* global learning rate and the learing rate */
   /* to 0 (zero) if it's undefined.            */
   /*-------------------------------------------*/
   if (global_learn_base != UNDEFINED) BEGIN
      if (ptr_layer->learn_base == UNDEFINED)
         ptr_layer->learn_base = global_learn_base;
      if (ptr_layer->learn_scale == UNDEFINED)
         ptr_layer->learn_scale = 0;
   ENDIF
   
   else BEGIN
      /*--------------------------------------------------*/
      /* handle cases 5 and 6 of the table in the comment */
      /* In these cases, just set the scale if undefined  */
      /*--------------------------------------------------*/
      if (ptr_layer->learn_base != UNDEFINED) BEGIN
         if (ptr_layer->learn_scale == UNDEFINED)
            ptr_layer->learn_scale = 0;
      ENDIF
      
      else BEGIN
         /*----------------------------------------*/
         /* for the last two cases, if user hasn't */
         /* been prompted for global, then prompt  */
         /*----------------------------------------*/
         if (prompt_state == TRUEE) BEGIN
            prompt_state = FALSEE;
            sprintf(IO_str, "Use a global learning rate (y/n, default=y)? ");
	    Curses_prompt_gets(IO_str,tmp_str);

/*            IO_print(0);
            IO_my_get_string(tmp_str); */
 
            if ((tmp_str[0] == ENDSTRING)
                 || (tmp_str[0] == 'y') || (tmp_str[0] == 'Y')) BEGIN
               /*-------------------------------------*/
               /* if global is selected, then prompt  */
               /* and use value as default learn rate */
               /*-------------------------------------*/
               learn = LR_learn_default(max_wts);

               sprintf(IO_wkstr,"Enter global learning rate (default=%%.f): ");
               IO_insert_format(IO_wkstr);
               sprintf(IO_str, IO_wkstr, learn);
	       Curses_prompt_gets(IO_str,tmp_str);
	       global_learn_base = learn = atof(tmp_str);

/*               IO_print(0);
               global_learn_base = IO_get_default_float(learn); */

               ptr_layer->learn_base = global_learn_base;
               if (ptr_layer->learn_scale == UNDEFINED)
                  ptr_layer->learn_scale = 0;
            ENDIF
         ENDIF
         
         /*----------------------------------------------*/
         /* if the global is still undefined, that means */
         /* the user has elected against a global learn  */
         /*----------------------------------------------*/
         if (global_learn_base == UNDEFINED) BEGIN
            
            /*---------------------------------------*/
            /* first, prompt for default learn value */
            /*---------------------------------------*/
            if (ptr_layer->learn_scale == UNDEFINED) BEGIN
               learn = LR_learn_default(ptr_layer->max_incoming);
               sprintf(IO_str, "   Use constant learning rate for layer ");
/*               IO_print(0); */
               sprintf(IO_wkstr,"%s %d (y/n, default=y)? ",IO_str,ptr_layer->ID);
		
/*               IO_print(0);
               IO_my_get_string(tmp_str); */

		Curses_prompt_gets(IO_wkstr,tmp_str);

               /*-----------------------------------------*/
               /* if denied, then the user wants a varied */
               /* learning rate. Divide default by 2. If  */
               /* "y", the constant learn; set scale = 0. */
               /*-----------------------------------------*/
               if ((tmp_str[0] != ENDSTRING) 
                   && (tmp_str[0] != 'y') && (tmp_str[0] != 'Y'))
                  learn = learn / 2.0;
               else ptr_layer->learn_scale = 0;
            ENDIF
            else          
               learn = LR_learn_from_scale(ptr_layer->learn_scale);

            /*-----------------------------------------------*/
            /* prompt for the learning rate based on default */
            /*-----------------------------------------------*/
            sprintf(IO_str, "     Enter learning rate for layer ");
/*            IO_print(0); */
            sprintf(IO_wkstr, "%d (default=%%.f): ", ptr_layer->ID);
            IO_insert_format(IO_wkstr);
            sprintf(IO_str, IO_wkstr, learn);
	    Curses_prompt_gets(IO_str,tmp_str);
	    ptr_layer->learn_base = learn = atof(tmp_str);

/*            IO_print(0);
            ptr_layer->learn_base = IO_get_default_float(learn); */
            
            /*-------------------------------------*/
            /* finally, prompt for the learn_scale */
            /* if it is undefined at this point    */
            /*-------------------------------------*/
            if (ptr_layer->learn_scale == UNDEFINED) BEGIN
               scale = LR_scale_default(ptr_layer->learn_base);
               sprintf(IO_str, "     Enter scaling factor for layer ");
/*               IO_print(0); */
               sprintf(IO_wkstr, "%d (default=%%.f): ", ptr_layer->ID);
               IO_insert_format(IO_wkstr);
               sprintf(IO_str, IO_wkstr, scale);
		Curses_prompt_gets(IO_str,tmp_str);
		ptr_layer->learn_scale = scale = atof(tmp_str);

/*               IO_print(0);
               ptr_layer->learn_scale = IO_get_default_float(scale); */
            ENDIF
         ENDIF
      ENDELSE
   ENDELSE
   
   /*-------------------------------------*/
   /* when the dust clears, set the first */
   /* learning rate value for the layer   */
   /*-------------------------------------*/
   ptr_layer->cur_learn = (D_Sint)C_float_to_Sint(ptr_layer->learn_base);
      
END /* B_set_layer_learning */


void  B_set_layer_momentum(ptr_layer, max_wts)
Layer  *ptr_layer;
int   max_wts;
/*
----------------------------------------------------------------------
  Sets the momentum value for the layer, given a pointer to the layer
  and the maximum weights connected into a node in the network. There 
  are four cases which must be handled, based on the values of the 
  global momentum (G) and the local momentum defined for the layer (M).
  The table below gives the appropriate actions for the four states, 
  based upon whether or not G,M are specified ("Y") or not ("N"):
  
  G M     Action
  ---     -------
  Y Y     do nothing
  Y N     set momemtum = G
  N Y     do nothing
  N N     prompt for global G, set momentum to G or prompt for local.
  
  As with the B_set_layer_learning routine, the last case prompts the
  user for a global momentum ONLY ONCE by keeping track of a local static
  variable on the prompt_state. 
----------------------------------------------------------------------
*/
BEGIN
   static float  prompt_state = TRUEE;
   float         momentum;
   char          tmp_str[MAX_WORD_SIZE];
   
   /*--------------------------------------*/
   /* reset the state of g_learn_state if  */
   /* we are starting a new network. If so */
   /* then we will be starting w/layer 1   */
   /*--------------------------------------*/
   if (ptr_layer->ID == 1)
      prompt_state = TRUEE;
      
   if (ptr_layer->momentum == UNDEFINED) BEGIN
      /*------------------------------------------*/
      /* if user hasn't been prompted, then do so */
      /*------------------------------------------*/
      if ((prompt_state == TRUEE) && (global_momentum == UNDEFINED)) BEGIN
         prompt_state = FALSEE;
         sprintf(IO_str, "   Use a global momentum (y/n, default=y)? ");
	 Curses_prompt_gets(IO_str,tmp_str);

/*         IO_print(0);
         IO_my_get_string(tmp_str); */

         if ((tmp_str[0] == ENDSTRING)
              || (tmp_str[0] == 'y') || (tmp_str[0] == 'Y')) BEGIN
            /*-------------------------------------*/
            /* if global is selected, then prompt  */
            /* and use value as default momentum   */
            /*-------------------------------------*/
            momentum = LR_momentum_default(max_wts);         

            sprintf(IO_wkstr, "   Enter global momentum (default=%%.f): ");
            IO_insert_format(IO_wkstr);
            sprintf(IO_str, IO_wkstr, momentum);
	    Curses_prompt_gets(IO_str,tmp_str);
	    global_momentum = atof(tmp_str);

/*            IO_print(0);
            global_momentum = IO_get_default_float(momentum); */
         ENDIF
      ENDIF
      
      if (global_momentum != UNDEFINED) 
         ptr_layer->momentum = global_momentum;
      else BEGIN
         /*---------------------------------------------*/
         /* if global undefined, then local momentum    */
         /* is desired. Calc default using max incoming */
         /*---------------------------------------------*/
         momentum = LR_momentum_default(ptr_layer->max_incoming);
         sprintf(IO_str, "     Enter momentum for layer ");
         IO_print(0);
         sprintf(IO_str, "%d (default=%%.f): ", ptr_layer->ID);
         IO_insert_format(IO_wkstr);
         sprintf(IO_str, IO_wkstr, momentum);
	 Curses_prompt_gets(IO_str,tmp_str);
	 ptr_layer->momentum = momentum = atof(tmp_str);	 

/*         IO_print(0);
         ptr_layer->momentum = IO_get_default_float(momentum);          */
      ENDELSE
   ENDIF
   
END /* B_set_layer_momentum */
      

Net  *B_free_net(ptr_net)
Net  *ptr_net;
/*
----------------------------------------------------------------------
  The purpose of this routine is memory management. It frees the memory
   allocated to a network specified by the network pointer and resets 
   the pointer to NULL. 
  To accomplish this task, this routine passes the buck to the
   L_free_layer_lst routine which simply loops through all of the
   layers of the network calling L_free_layer for each. Note that the
   weights are doubly referenced and can thus cause a problem. To avoid
   freeing something twice, I use the convention of freeing only a layer's
   INCOMING weights. This means that the INPUT LAYER can be SKIPPED by
   this routine( see L_free_layer_lst for details ).
  NOTE: The OUTPUT layer is included in the list of hidden layers. Recall
   that the list "hidden_front" includes all those layers which must be
   updated for a forward propagation. This includes the output layer. 
   Thus there is an implicit freeing of the output layer when the hidden
   layers are freed.
----------------------------------------------------------------------
*/
BEGIN
   if (ptr_net == NULL) return(NULL);
   
   L_free_layer(ptr_net->input_layer);
   L_free_layer_lst(ptr_net->hidden_front);   
   
   sys_free((char *)ptr_net);
   return(NULL);

END /* B_free_net */
