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
  Code For Construction Of Net Structures (Prefix = PS_)
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
#include "netio.h"


/*
----------------------------------------------------------------------
  MACRO DEFINITIONS
----------------------------------------------------------------------
  Below are eight different macros used for range checking while the
  network spec file is being parsed. Each commands in the file takes
  a numeric argument, and these macros can check ranges for any of the
  eight possible cases given an upper bound (u) and a lower bound (l)
  on the number "n". 
----------------------------------------------------------------------
*/
#define GTLT(n,l,u)  (((n) >  (l) && (n) <  (u)) ? 1 : 0)
#define GELT(n,l,u)  (((n) >= (l) && (n) <  (u)) ? 1 : 0)
#define GTLE(n,l,u)  (((n) >  (l) && (n) <= (u)) ? 1 : 0)
#define GELE(n,l,u)  (((n) >= (l) && (n) <= (u)) ? 1 : 0)

#define LTGT(n,l,u)  (((n) <  (l) || (n) >  (u)) ? 1 : 0)
#define LEGT(n,l,u)  (((n) <= (l) || (n) >  (u)) ? 1 : 0)
#define LTGE(n,l,u)  (((n) <  (l) || (n) >= (u)) ? 1 : 0)
#define LEGE(n,l,u)  (((n) <= (l) || (n) >= (u)) ? 1 : 0)


/*
----------------------------------------------------------------------
  EXTERNED FUNCTIONS AND GLOBALS
----------------------------------------------------------------------
  Below are the functions defined in other files which are used by the
  code here. They are organized by section.
----------------------------------------------------------------------
*/
extern Sint      C_float_to_Sint();
extern char     *sys_alloc();
extern FILE     *PA_open_binary();
extern int       PA_put_to_workfile();
extern void      PA_flush();
extern void      IO_print();

extern char      IO_str[MAX_LINE_SIZE];


/*
----------------------------------------------------------------------
  GLOBAL VARIABLES
----------------------------------------------------------------------
  First, some markers which are used by several routines and are just
  declared here a globals for convenience. A parsing variable is next,
  called "use_last_command" which is needed for the parsing of layer
  specifications. It determines whether or not to reuse the last 
  command read in during the parsing. Then come two globals which are
  used during the creation of a network to indicate whether or not a
  global learning rate and/or momentum has been used. If so, then these
  two globals will contain the floating point equivalent of the given
  values.
----------------------------------------------------------------------
*/
static int     line_count = 1;   /* used by parse_iopairs, PS_get_token */
static char    last_char;        /* holds the last character read in    */
static char    last_str[MAX_LINE_SIZE];   /* holds last command read in */
static int     use_last_command = FALSEE;  /* used for layer parse (see  */
                                          /* PS_get_command).           */
float          global_learn_base;   /* used during parse to set default */
float          global_momentum;     /* global learn/momentum values     */


/*
======================================================================
  ROUTINES IN PARSER.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "PS_" indicating that it is defined in the 
  "parser.c" file.  The types returned by the routines are also shown 
  here so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
    void                        PS_reset_for_layer_parse
    int                         PS_global_spec_check
    Layer_spec *                PS_get_layer
    void                        PS_get_learn_values
    void                        PS_get_nodes
    void                        PS_get_node_dimensions
    void                        PS_get_target
    void                        PS_get_pattern
    int                         PS_get_command
    void                        PS_print_command
    int                         PS_range_check
    int                         PS_int_check
    int                         PS_parse_iopairs
    int                         PS_check_items
    int                         PS_skip_tokens
    int                         PS_get_token
    int                         PS_get_float_from_file
======================================================================
*/


void  PS_reset_for_layer_parse()
/*
----------------------------------------------------------------------
  The only thing done here, as of now, is to set the global variable
  'use_last_command' to FALSEE. This must be done from the OUTSIDE via
  this routine because the global is static to this file.
----------------------------------------------------------------------
*/
BEGIN
   use_last_command = FALSEE;

END /* PS_reset_for_layer_parse */


int  PS_global_spec_check(ptr_file)
FILE  *ptr_file;
/*
----------------------------------------------------------------------
 This routine checks for the existence of two optional specs located 
  at the very beginning of a network specification file. They are the
  GLOBAL-LEARN-RATE and GLOBAL-MOMENTUM respectively. Either or both
  of these may be specified by the user. If they are present, they 
  define default values for the learn_base and momemtum elements of 
  each layer. These values may be overridden later by using the 
  LEARN-RATE, SCALE-FACTOR, or MOMENTUM commands for each layer which
  needs overriding.
  
  Returns ERROR if EOF encountered while trying to check for these 
  first two optional commands.
----------------------------------------------------------------------
*/
BEGIN
   float  num;
   int    temp, PS_get_command(), PS_range_check();
   void   PS_print_command();
   
   /*--------------------------------------*/
   /* first, set the global values to -1.0 */
   /* symbolizing UNKNOWN values           */
   /*--------------------------------------*/
   global_learn_base = UNDEFINED;
   global_momentum   = UNDEFINED;
   
   /*-----------------------------------*/
   /* loop on PS_get_command, until you */
   /* hit EOF or a LAYER command.       */
   /*-----------------------------------*/
   temp = PS_get_command(ptr_file, &num);
   while (temp != LAYER) BEGIN
      if (temp == EOF) BEGIN
         sprintf(IO_str, "\n*** ERROR: no specifications found in file");
         IO_print(0);
         return(ERROR);
      ENDIF
      else if (temp == GLOBAL_LEARN_RATE) BEGIN
         if (PS_range_check( GTLE(num, 0, LEARN_MAX) ) == OKAY)
            global_learn_base = num;
      ENDELSE
      else if (temp == GLOBAL_MOMENTUM) BEGIN
         if (PS_range_check( GELE(num, 0, MOMENTUM_MAX) ) == OKAY)
            global_momentum = num;
      ENDELSE
      else BEGIN
         sprintf(IO_str, "\n*** WARNING: spurious ");
         IO_print(0);
         PS_print_command(temp);
         sprintf(IO_str, " command found before LAYER specification.");
         IO_print(0);
         sprintf(IO_str, "\nCommand ignored.");
         IO_print(0);
      ENDELSE
      temp = PS_get_command(ptr_file, &num);
   ENDWHILE
   
   /*-----------------------------------*/
   /* set USE_LAST_COMMAND flag to TRUEE */
   /*-----------------------------------*/
   use_last_command = TRUEE;
   return(OKAY);

END /* PS_global_spec_check */


Layer_spec  *PS_get_layer(ptr_file)
FILE  *ptr_file;
/*
----------------------------------------------------------------------
 This routine reads lines from the file pointed to by 'ptr_file' and  
  converts them into the 'Layer_spec' format described in the file    
  'netio.h'.  Now, to understand how this is done, you must know the  
  the specifics of how the file is set up.  In a sense, the logic of  
  this routine describes that setup.  In conjunction with the def-    
  inition of 'Layer_spec', that setup is described here.              
 First, an example of what ONE layer specification might look like in 
  a file:                                                             
                                                                      
   LAYER : 25         -- comment for line 1                           
   NODES : 5          -- comment for line 2                           
                      -- line 2 comment continued                     
     TARGET : 2                                                       
     TARGET : 4                                                       
     TARGET : 6                                                       
                                                                      
  notice that the first two lines have comments (those comments would 
  not necessarily appear in the file; they are here as an example     
  only).  There are several important features I would point out in   
  this example.  First, note that a Layer specification consists of   
  three types of commands:  ONE "layer", ONE "nodes" and some (optional)
  number of "target" commands.  The layer command specifies the ID
  number which will be assigned to the layer.  The nodes command 
  indicates the number of nodes contained in the layer.  The target         
  commands (of which there may be zero!) denote the OTHER layers to   
  which this layer has OUTGOING connections. For example, the layer   
  above, layer 25, has 3 outgoing connections to layers 2, 4, and 6   
  respectively.  This also means that these three layers have incoming
  weights from layer 25 (see Layer structure definition in layer.h);
  but NOTE THAT THOSE LAYERS WOULD NOT NEED TARGET SPECIFICATIONS. Only
  the layer acting as a SOURCE needs to have the target specification.
  Putting the specification in both places is redundant and more 
  difficult on the user. 
 A couple of notes about how this routine reads the input file. First 
  note that each of the commands above looks like "command : number"  
  THE SPACES HERE ARE SIGNIFICANT.  This routine does a pattern match 
  rather than a sophisticated scanning, so if you leave the spaces out
  you get an error.  Also, note that the comments have "--" in front  
  of them.  This is not necessary, but it is a good idea.  The reason 
  lies in the fact that EACH LINE MUST HAVE AT MOST ONE COMMAND and   
  THAT COMMAND MUST NOT BE PRECEDED BY ANY OTHER TEXT.  Thus, if you  
  had a comment that ran over one line, and which also happened to    
  include something that looked like a command syntax, it could be    
  interpreted as a command.  To avoid this, always preceed any        
  comments with the double dash.

  To summarize the input file syntax:

    each line must have AT MOST one command
    commands look like "command : number"; SPACES ARE SIGNIFICANT
    comments should be preceeded by a double dash: ie "--"
    layer connections are specified from SOURCE LAYER ONLY
    LAYER and NODES commands are essential for a layer, TARGET is optional
----------------------------------------------------------------------
  4-7-88  I've added a new twist to the layer specification to allow for
  PATTERNED CONNECTION schemes between layers, in addition to the fully
  connected scheme already available. Here's the deal. Imagine that you
  have two layers, one of which is larger than the other. Let's call them
  layer B (Big) and layer L (Little). You might be in a position where
  you did NOT want all of the nodes of layer B to be connected to all
  of the nodes in layer L. One reason might be that there would simply 
  be two many connections. A more plausible reason might be that you 
  only want LOCAL ASSOCIATIONS between layer B and layer S. That is, you
  might want groups of nodes in layer B which are "close together" to 
  map to a single node in layer L. This is often the case with nets which
  are attempting to work with visual input. Often, one tries to associate
  small areas of the input layer (usually a screen image) and map them to
  separate nodes of a hidden layer. In this way, one simulates "piecing
  together" the bits of visual information on the screen by trying to 
  build larger and larger primitive shapes (line, curve, angle, etc) out
  of small regions of the image. Anyway, the point is, you might want to
  have some pattern of connections between layer B and L, and I have added
  syntax to allow for that.
  OKAY, first things first. Assume two layers B and L where nodes(B) > nodes(L)
  (B has more nodes). Assume further that the dimensions of B are UxV (in the 
  X and Y dimensions respectively) and those of L are MxN.  Now, one can 
  define "Pattern areas" of layer B which are to be mapped as a group 
  onto SINGLE nodes of L. Let this "pattern area" be of dimension PxQ 
  (also in terms of X,Y). Finally, one might imagine that these pattern 
  areas overlap with eachother (why not?) and this overlap may occur in
  either, or both, the X and Y dimensions. Let this overlap be defined
  as RxS (X overlap = R, Y overlap = S). Here's and example:

  Layer B:          _______
                   |0|1|2|3|       UxV = 4x2
                   |4|5|6|7|       12 nodes, numbered 0-C
                   |8|A|B|C|
                    -------

  Pattern area:     _____
                   |_|_|_|         PxQ = 3x2
                   | | | |         Overlap 2 in the X direction, 1 in the Y
                    -----            so RxS = 2x1

  with the above specifications, lets walk through how layer L would have 
  to look. The first pattern area would start at node 0 of B and go 3 in 
  X direction, and 2 in the Y direction.  This would involve nodes 0, 1, 2
  and 4, 5, 6 of layer B. This area would map to the first node of layer L.
  The second pattern area STARTS AT NODE 1 OF LAYER B!!! This is because the
  user specified an overlap of 2 in the X direction. Thus the second pattern
  area covers nodes 1, 2, 3 and 5, 6, 7 of layer B. The third pattern area
  is now found by moving DOWN (in the Y direction) and since the user 
  specified a Y overlap of 1, the third pattern area STARTS AT NODE 4 of B.
  This area covers nodes 4,5,6 and 8,A,B of layer B. Finally, the last
  pattern area again overlaps 2 in the X direction and covers nodes 5,6,7
  and A,B,C.  Layer L then has only 4 nodes as follows:

  Layer L:          ___
                   |0|1|           Mxn = 2x2
                   |2|3|           4 nodes, numbered 0-3
                    ---

  In all, 24 weights are needed. That is, 4 pattern areas, each with a
  3x2 area mapped to 1 node = 3 * 2 * 4 or 6 * 4 = 24. The following is
  a list of weights and the nodes which each connects:

  Weight number           Node in B         Node in L
  -------------           ---------         ---------
       0                      0                 0
       1                      1                 0
       2                      2                 0
       3                      4                 0
       4                      5                 0
       5                      6                 0

       6                      1                 1
       7                      2                 1
       8                      3                 1
       9                      5                 1
      10                      6                 1
      11                      7                 1

      12                      4                 2
      13                      5                 2
      14                      6                 2
      15                      8                 2
      16                      9                 2
      17                      A                 2

      18                      5                 3
      19                      6                 3
      20                      7                 3
      21                      A                 3
      22                      B                 3
      23                      C                 3

  SYNTAX: The new syntax has all the same rules as the old plus the 
  following:
  (1) X and Y dimensions are specified separately
  (2) dimensions not specified for the layer will default
  (3) if No dimension is specified for NODES, then the layer is
      assumed to be X = NODES, Y = 1.
  (4) if only one dimension is missing, it is assumed = 1.
  (5) pattern areas are defined along with the TARGET command
  (6) only the overlap specs are optional for a pattern specification.
      These X and Y dimensions MUST be specified. If no overlaps are
      specified, they will default to 0.
  (7) if TARGET specs do not include dimensions, a CONNECT ALL scheme
      is assumed.

  EXAMPLE: To generate the net above (say there were only two 
  layers) we would do:

  LAYER : 0                  -- only one command per line
  NODES : 12                 -- spacing only significant around ":'s"
    X-DIMENSION : 4          -- optional dimensions 
    Y-DIMENSION : 3
    TARGET : 1
      PATTERN-X-DIMENSION : 3  -- optional. If not present, then
      PATTERN-Y-DIMENSION : 2  -- "connect all" is assumed.
      X-OVERLAP : 2
      Y-OVERLAP : 1

  LAYER : 1
  NODES : 4
    X-DIMENSION : 2          -- a good idea to specify both layer's
    Y-DIMENSION : 2          -- dimensions since system will verify.

  Note that the dimensions of BOTH layers will, in general, have to be
  specified as the system will check to see that the dimensions are 
  correct for the given pattern area dimension/overlap specification.

  The full BNF for the layer spec syntax is ("{}" indicate optional,
  and commands are in all caps):

  layer-spec   :== LAYER  node-spec  {target-spec}
  node-spec    :== NODES  {X-DIMENSION}  {Y-DIMENSION}
  target-spec  :== TARGET  {pattern-spec} {target-spec}
  pattern-spec :== PATTERN-X-DIMENSION  PATTERN-Y-DIMENSION  {X-OVERLAP}
                   {Y-OVERLAP}
----------------------------------------------------------------------
  5-3-89 Im adding another piece of syntax to the net specification file
   which will be optional. I want the user to be able to specify the
   learning rate, scaling factor, and momentum in the file so that 
   delivery networks can be easily created. This will also allow for
   automatic generation of network specification files from a simple
   list of IO pairs. The new syntax lookes like:
   
   node-spec   :== NODES {dimensions} {l-factors}
   dimensions  :== X-DIMENSION {dimensions} | Y-DIMENSION {dimensions}
   l-factors   :== LEARN-RATE {l-factors} | SCALE-FACTOR {l-factors}
                   | MOMENTUM {l-factors}
                   
   i.e., any or all of the three learning constants may be entered 
   after the optional x,y dimension info. Also, I have changed the
   syntax to allow for global specification of the learning rate and
   momentum. These parameters are considered optional, before the
   specification of the layers in the network, as the BNF below shows
   (see PS_global_spec_check and B_process_config):
   
   network     :== {globals} layer-spec layer-spec {hiddens}
   globals     :== GLOBAL-LEARN-RATE {globals}
                   | GLOBAL-MOMENTUM {globals}
   hiddens     :== layer-spec {hiddens}
----------------------------------------------------------------------
*/
BEGIN
   Layer_spec  *result;
   float       num;
   int         temp, PS_get_command(), PS_range_check(), PS_int_check();
   void        PS_get_nodes(), PS_get_target(), PS_print_command(),
               PS_get_learn_values();

   /*----------------------------*/
   /* start out as a good layer  */
   /*----------------------------*/
   result = (Layer_spec *) sys_alloc((unsigned)sizeof(Layer_spec));
   result->status = OKAY;                      

   /*------------------------------*/
   /* read until good layer or EOF */
   /*------------------------------*/
   temp = PS_get_command(ptr_file, &num);
   while (TRUEE) BEGIN
      if (temp == EOF) BEGIN
         result->status = EOF;
         break;
      ENDIF
      if ( (temp == LAYER)
           && (PS_int_check(num) == OKAY)
           && (PS_range_check(GELE(num, 0, MAX_LAYERS)) == OKAY) ) 
         break;
      sprintf(IO_str, "\n*** WARNING: spurious ");
      IO_print(0);
      PS_print_command(temp);
      sprintf(IO_str, " command found outside of LAYER specification.");
      IO_print(0);
      sprintf(IO_str, "\nCommand ignored.");
      IO_print(0);
      temp = PS_get_command(ptr_file, &num);
   ENDWHILE

   /*---------------------------------------------*/
   /* once you have a layer, read its nodes specs */
   /*---------------------------------------------*/
   if (result->status == OKAY) BEGIN
      result->ID = (int)num;
      
      /*-------------------------------------------*/
      /* look for any learning rate specifications */
      /*-------------------------------------------*/
      PS_get_learn_values(ptr_file, result);

      PS_get_nodes(ptr_file, result);
   ENDIF
   
   /*----------------------------*/
   /* then read the target specs */
   /*----------------------------*/
   if (result->status == OKAY)
      PS_get_target(ptr_file, result);
      
   /*-------------------------------------*/
   /* if delivery code is being run, then */
   /* get the learning values from the    */
   /* arrays defined in the delivery file */
   /*-------------------------------------*/
#if  DELIVERY
   result->learn_base  = 0;
   result->learn_scale = 0;
   result->momentum    = 0;
#endif
      
   /*-------------------------------------------------*/
   /* if any errors occur, print message, then return */
   /*-------------------------------------------------*/
   if (result->status == ERROR) BEGIN
      sprintf(IO_str, "\n\n*** ERROR: Problems in Net Specification file");
      IO_print(0);
   ENDIF
   return(result);

END /* PS_get_layer */


void  PS_get_learn_values(ptr_file, ptr_layer_spec)
FILE        *ptr_file;
Layer_spec  *ptr_layer_spec;
/*
----------------------------------------------------------------------
  This routine checks for the optional learning rate, scale, and momentum
  specifications for a given layer.  It is called only from the
  "PS_get_nodes" routine. All three of these values will default to 0 
  unless specified in the file. A zero value for either a learn_base or
  the momentum is considered as an "unknown" value when the layer is 
  allocated. At a later point, the user is prompted for learning rates
  and momentum values if none exist in the file.
  NOTE: since these three commands are optional, this routine has no way
  of knowing when to stop checking except by detecting when its gone
  too far. That is, you only know that there are no X and Y dimensions
  specified when you (1) read LAYER, (2) read TARGET, or (3) hit EOF.
  Thus, you will end up reading some command which may be needed in
  another checking routine.  As a result, this routine sets the 
  "use_last_command" flag to TRUEE before it exits.
----------------------------------------------------------------------
*/
BEGIN
   int    temp, PS_get_command(), PS_range_check();
   float  num;
   void   PS_print_command();

   /*------------------------------------*/
   /* first set the default learning and */
   /* momentum values. These will be     */
   /* used unless overwritten below.     */
   /*------------------------------------*/
   ptr_layer_spec->learn_base  = global_learn_base;
   ptr_layer_spec->momentum    = global_momentum;
   
   /*----------------------------------------------------------*/
   /* the scale factor is undefined ONLY if no global learning */
   /* rate exists. Else, 0 = no scaling unless overwritten     */
   /*----------------------------------------------------------*/
   if (global_learn_base == UNDEFINED)
      ptr_layer_spec->learn_scale = UNDEFINED;
   else ptr_layer_spec->learn_scale = 0;
   

   /*--------------------------------------------*/
   /* read in commands until NODES command found */
   /*--------------------------------------------*/
   temp = PS_get_command(ptr_file, &num);
   while (temp != NODES) BEGIN
      if (temp == EOF) BEGIN
         ptr_layer_spec->status = EOF;
         break;
      ENDIF
      if (temp == SCALE_FACTOR) BEGIN
         if (PS_range_check(GELE(num, 0, SCALE_MAX)) == OKAY)
            ptr_layer_spec->learn_scale = num;
      ENDIF
      else if (temp == LEARN_RATE) BEGIN
         if (PS_range_check(GTLE(num, 0, LEARN_MAX)) == OKAY)
            ptr_layer_spec->learn_base = num;
      ENDELSE
      else if (temp == MOMENTUM) BEGIN
         if (PS_range_check(GELE(num, 0, MOMENTUM_MAX)) == OKAY)
            ptr_layer_spec->momentum = num;
      ENDELSE
      else BEGIN
         sprintf(IO_str, "\n*** WARNING: spurious ");
         IO_print(0);
         PS_print_command(temp);
         sprintf(IO_str, " command found in the middle of a LAYER specification.");
         IO_print(0);
         sprintf(IO_str, "\nCommand ignored.");
         IO_print(0);
      ENDELSE
      temp = PS_get_command(ptr_file, &num);
   ENDWHILE
   use_last_command = TRUEE;

END /* PS_get_learn_values */


void  PS_get_nodes(ptr_file, ptr_layer_spec)
FILE        *ptr_file;
Layer_spec  *ptr_layer_spec;
/*
----------------------------------------------------------------------
  The NODES section of a layer specification is a non-optional delimeter
  of how many nodes should be allocated to the layer.  It does have two
  optional arguments, X-DIMENSION and Y-DIMENSION, which can be used to
  determine the geometry of the layer. Furthermore, one can make the
  assumption that this routine is ONLY called by the PS_get_layer routine
  above. Thus the last command read by the parser is the LAYER command
  and we can continue reading PAST that command.
  This routine operates as follows. First, a loop is entered which continues
  until a NODES command is reached or the EOF is found.  If a successful
  NODES command is found, then its value is recorded and the routine
  'PS_get_node_dimensions' is called to check for the optional X and
  Y dimension specifications.
----------------------------------------------------------------------
*/
BEGIN
   void   PS_get_node_dimensions(), PS_print_command();
   float  num;
   int    temp, PS_get_command(), PS_int_check(), PS_range_check();

   temp = PS_get_command(ptr_file, &num);
   while (TRUEE) BEGIN
      if (temp == EOF) BEGIN
         sprintf(IO_str, "\n*** ERROR: could not find NODES specification for Layer %d",
                ptr_layer_spec->ID);
         IO_print(0);
         ptr_layer_spec->status = ERROR;
         break;
      ENDIF
      if ( (temp == NODES)
           && (PS_int_check(num) == OKAY)
           && (PS_range_check(GTLE(num, 0, MAX_NODES)) == OKAY) ) 
         break;
      else if (temp == LAYER) BEGIN
         sprintf(IO_str, "\n*** Warning: No NODES specification provided for LAYER %d",
                ptr_layer_spec->ID);
         IO_print(0);
         sprintf(IO_str, "\nLayer %d ignored", ptr_layer_spec->ID);
         IO_print(0);
         ptr_layer_spec->ID = (int)num;
      ENDELSE
      else BEGIN
         sprintf(IO_str, "\n*** WARNING: spurious ");
         IO_print(0);
         PS_print_command(temp);
         sprintf(IO_str, " command found before NODES specification.");
         IO_print(0);
         sprintf(IO_str, "\nCommand ignored.");
         IO_print(0);
      ENDELSE
      temp = PS_get_command(ptr_file, &num);
   ENDWHILE

   if (ptr_layer_spec->status == OKAY) BEGIN
      ptr_layer_spec->num_nodes = num;
      PS_get_node_dimensions(ptr_file, ptr_layer_spec);
   ENDIF
   
END /* PS_get_nodes */


void  PS_get_node_dimensions(ptr_file, ptr_layer_spec)
FILE        *ptr_file;
Layer_spec  *ptr_layer_spec;
/*
----------------------------------------------------------------------
  This routine checks for the optional X and Y dimension specifications
  for a given layer's NODES specification.  It is called only from the
  "PS_get_nodes" routine. The X and Y dimension specs are optional and 
  will default as follows. If neither is specified, then the X dimension 
  is set to the NODES value and the Y dimension is set to 1. If only 
  one dimension is set, then the other dimension is set to 1. In any 
  event, the product of the X and Y dimensions is found and compared 
  to the NODES value. If they are not equal and error is sighted.
  NOTE: since these two commands are optional, this routine has no way
  of knowing when to stop checking except by detecting when its gone
  too far. That is, you only know that there are no X and Y dimensions
  specified when you (1) read LAYER, (2) read TARGET, (3) hit EOF or
  (4) read a LEARN-RATE, SCALE-FACTOR, or MOMENTUM command. Thus, you 
  will end up reading some command which may be needed in another 
  checking routine.  As a result, this routine sets the "use_last_command"
  flag to TRUEE before it exits.
----------------------------------------------------------------------
*/
BEGIN
   int    temp, PS_get_command(), PS_int_check(), PS_range_check();
   float  num;
   void   PS_print_command();

   ptr_layer_spec->X_dim = -1;      /* initialize dimensions to -1 */
   ptr_layer_spec->Y_dim = -1;

   /*---------------------------------------------------------------------*/
   /* read in commands until LAYER or TARGET, checking for X,Y dimensions */
   /*---------------------------------------------------------------------*/
   temp = PS_get_command(ptr_file, &num);
   while ((temp != LAYER)
          && (temp != TARGET)) BEGIN
      if (temp == EOF) BEGIN
         ptr_layer_spec->status = EOF;
         break;
      ENDIF
      if (temp == X_DIMENSION) BEGIN
         if ( (PS_int_check(num) == OKAY)
              &&(PS_range_check(GTLE(num, 0, MAX_NODES)) == OKAY) )
            ptr_layer_spec->X_dim = num;
      ENDIF
      else if (temp == Y_DIMENSION) BEGIN
         if ( (PS_int_check(num) == OKAY)
              &&(PS_range_check(GTLE(num, 0, MAX_NODES)) == OKAY) )
            ptr_layer_spec->Y_dim = num;
      ENDELSE
      else BEGIN
         sprintf(IO_str, "\n*** WARNING: spurious ");
         IO_print(0);
         PS_print_command(temp);
         sprintf(IO_str, " command found in the middle of a NODES specification.");
         IO_print(0);
         sprintf(IO_str, "\nCommand ignored.");
         IO_print(0);
      ENDELSE
      temp = PS_get_command(ptr_file, &num);
   ENDWHILE

   /*----------------------------------------------------*/
   /* Set default values of X_dim and Y_dim if necessary */
   /*----------------------------------------------------*/
   if ((ptr_layer_spec->X_dim == -1) && (ptr_layer_spec->Y_dim == -1)) BEGIN
      ptr_layer_spec->X_dim = ptr_layer_spec->num_nodes;
      ptr_layer_spec->Y_dim = 1;
   ENDIF
   else BEGIN
      if (ptr_layer_spec->X_dim == -1) ptr_layer_spec->X_dim = 1;
      if (ptr_layer_spec->Y_dim == -1) ptr_layer_spec->Y_dim = 1;
   ENDELSE

   if ((ptr_layer_spec->X_dim * ptr_layer_spec->Y_dim)
       != ptr_layer_spec->num_nodes) BEGIN
      sprintf(IO_str, "\n*** ERROR: X,Y dimensions inconsistent with the number of nodes in layer %d",
             ptr_layer_spec->ID);
      IO_print(0);
      ptr_layer_spec->status = ERROR;
   ENDIF
   use_last_command = TRUEE;

END /* PS_get_node_dimensions */


void  PS_get_target(ptr_file, ptr_layer_spec)
FILE        *ptr_file;
Layer_spec  *ptr_layer_spec;
/*
----------------------------------------------------------------------
  This routine must search for any number of target specs (ie, one or
  more).

  layer-spec   :== LAYER  node-spec  {target-spec}
  node-spec    :== NODES  {X-DIMENSION}  {Y-DIMENSION}
  target-spec  :== TARGET  {pattern-spec} {target-spec}
  pattern-spec :== PATTERN-X-DIMENSION  PATTERN-Y-DIMENSION  {X-OVERLAP}
                   {Y-OVERLAP}
----------------------------------------------------------------------
*/
BEGIN
   void   PS_get_pattern(), PS_print_command();
   float  num;
   int    temp, target_count, PS_get_command(), PS_int_check(), 
          PS_range_check();

   target_count = 0;
   temp = PS_get_command(ptr_file, &num);
   while ((temp != LAYER) && (ptr_layer_spec->status == 1)) BEGIN
      if (temp == EOF) BEGIN
         ptr_layer_spec->status = EOF;
         break;
      ENDIF
      else if (temp == TARGET) BEGIN
         if ( (PS_int_check(num) == OKAY)
              && (PS_range_check(GELE(num, 0, MAX_LAYERS)) == OKAY) ) BEGIN
            ptr_layer_spec->targets[target_count][0] = (int16)num;
            ptr_layer_spec->targets[target_count][1] = (int16)0;
            ptr_layer_spec->targets[target_count][2] = (int16)0;
            ptr_layer_spec->targets[target_count][3] = (int16)0;
            ptr_layer_spec->targets[target_count][4] = (int16)0;
            PS_get_pattern(ptr_file, ptr_layer_spec, target_count);
            target_count++;
         ENDIF
      ENDELSE
      else BEGIN
         sprintf(IO_str, "\n*** WARNING: spurious ");
         IO_print(0);
         PS_print_command(temp);
         sprintf(IO_str, " command found in the middle of a TARGET specification.");
         IO_print(0);
         sprintf(IO_str, "\nCommand ignored.");
         IO_print(0);
      ENDELSE
      temp = PS_get_command(ptr_file, &num);
   ENDWHILE
   ptr_layer_spec->num_targets = target_count;
   use_last_command = TRUEE;

END /* PS_get_target */


void  PS_get_pattern(ptr_file, ptr_layer_spec, target_index)
FILE        *ptr_file;
Layer_spec  *ptr_layer_spec;
int         target_index;
/*
----------------------------------------------------------------------
  The entire pattern spec is optional. However, if present it looks
  like:

  pattern-spec :== PATTERN-X-DIMENSION PATTERN-Y-DIMENSION 
                   {X-OVERLAP} {Y-OVERLAP}
  
  Reading this specification is achieved in a three step process. First,
  the PATTERN-X-DIMENSION command is looked for, if a LAYER or TARGET 
  command doesn't appear first. Once it is found, step two is to search
  for the PATTERN-Y-DIMENSION.  If it is not present, that is an error
  (you can't have a half-specified pattern). The third step consists 
  of a while loop to search for the optional OVERLAP dimensions which
  will default to 0 if they are not present. Finally, if a pattern is
  specified, then the end of its specification will occur on a TARGET
  or LAYER command, thus the "use_last_command" flag is set to TRUEE.
----------------------------------------------------------------------
*/
BEGIN
   int    temp, PS_get_command(), PS_int_check(), PS_range_check();
   float  num;
   void   PS_print_command();

   /*---------------------------------------------------------*/
   /* Step 1: find the PATTERN-X-DIMENSION command if present */
   /*---------------------------------------------------------*/
   temp = PS_get_command(ptr_file, &num);
   if (temp == EOF) 
      ptr_layer_spec->status = EOF;
   else if ((temp == LAYER) || (temp == TARGET))
      use_last_command = TRUEE;
   else if (temp != PATTERN_X_DIM) BEGIN
      sprintf(IO_str, "\n*** ERROR: found spurious ");
      IO_print(0);
      PS_print_command(temp);
      sprintf(IO_str, " command in the middle of a PATTERN specification.");
      IO_print(0);
      sprintf(IO_str, "\nExpected a PATTERN-X-DIMENSION command.");
      IO_print(0);
      ptr_layer_spec->status = ERROR;
   ENDELSE
   
   /*----------------------------------------------------------------------*/
   /* Step 2: if PATTERN-X-DIMENSION command found, search for Y dimension */
   /*----------------------------------------------------------------------*/
   else BEGIN
      if ( (PS_int_check(num) == OKAY)
           && PS_range_check(GTLE(num, 0, MAX_NODES) == OKAY) )
         ptr_layer_spec->targets[target_index][1] = (int16)num;  /* pattern_x_dim */
      temp = PS_get_command(ptr_file, &num);
      if (temp != PATTERN_Y_DIM) BEGIN                       /* error if no y dim */
         sprintf(IO_str, "\n*** ERROR: Incomplete pattern dimension specification.");
         IO_print(0);
         sprintf(IO_str, "\nExpected a PATTERN-Y-DIMENSION command.");
         IO_print(0);
         ptr_layer_spec->status = ERROR;
      ENDIF
      else BEGIN
         if ( (PS_int_check(num) == OKAY)
              && PS_range_check(GTLE(num, 0, MAX_NODES) == OKAY) )
            ptr_layer_spec->targets[target_index][2] = (int16)num;
         temp = PS_get_command(ptr_file, &num);
         
         /*-------------------------------------------------------------*/
         /* Step 3: once both dimensions are found, search for OVERLAPs */
         /*-------------------------------------------------------------*/
         while ((temp != LAYER) && (temp != TARGET)) BEGIN
            if (temp == EOF) BEGIN
               ptr_layer_spec->status = EOF;
               break;
            ENDIF
            else if (temp == X_OVERLAP) BEGIN
               if ( (PS_int_check(num) == OKAY)
                    && PS_range_check(GTLE(num, 0, MAX_NODES) == OKAY) )
                  ptr_layer_spec->targets[target_index][3] = (int16)num;
            ENDELSE
            else if (temp == Y_OVERLAP) BEGIN
                if ( (PS_int_check(num) == OKAY)
                     && PS_range_check(GTLE(num, 0, MAX_NODES) == OKAY) )
                  ptr_layer_spec->targets[target_index][4] = (int16)num;
            ENDELSE
            else BEGIN
               sprintf(IO_str, "\n*** WARNING: spurious ");
               IO_print(0);
               PS_print_command(temp);
               sprintf(IO_str, " command found in the middle of a pattern OVERLAP specification.");
               IO_print(0);
               sprintf(IO_str, "\nCommand ignored.");
               IO_print(0);
            ENDELSE
            temp = PS_get_command(ptr_file, &num);
         ENDWHILE
         use_last_command = TRUEE;
      ENDELSE
   ENDELSE

END /* PS_get_pattern */


int  PS_get_command(ptr_file, ptr_data)
FILE   *ptr_file;
float  *ptr_data;
/*
----------------------------------------------------------------------
 Given a pointer to an input file, this guy will read through the
  file line by line, until a valid command is reached.  Since each
  line of the input is to have at most 1 command, only the first 
  command in a line will be read; any others will be ignored. The 
  set of valid commands is defined by the BNF for parsing a layer
  specification (see netio.h and comments under 'PS_get_layer').
  A return code is given which depends upon the command found. These
  return codes are defined as constants in the netio.h file. Since
  this routine has no idea of syntax or semantics, it cannot tell
  whether or not the last command it read was actually used, or
  simply served as a delimiter for the BNF. Consequently, this
  routine must be told whether or not to reuse the last command 
  which was found. This information is kept in a global variable
  (for convenience) called "use_last_command" and is set or reset
  by the 'PS_get_layer' and 'PS_reset_for_layer_parse' routines.
 Aside from the return value specified above, any data which accompanies
  the command is returned via the 'ptr_data' parameter which is passed
  in from the calling routine.
----------------------------------------------------------------------
 9-7-89 Note that I have made the "last_str" variable global. I did this
  to enable better error messages. By having access to the last command
  read by the system, I can print out a message which will be more 
  meaningful to the user. A global variable seems the best way to make
  the last command available to many routines.
----------------------------------------------------------------------
*/
BEGIN
   static char  temp_str[MAX_LINE_SIZE];

   if (use_last_command == FALSEE)
      if (fgets(last_str, MAX_LINE_SIZE, ptr_file) == NULL)
         return(EOF);
   use_last_command = FALSEE;     /* reset flag so reuse only ONCE */

   while (TRUEE) BEGIN
      if (sscanf(last_str, "%s : %f", temp_str, ptr_data) == 2) BEGIN
         if (strcmp(temp_str, "TARGET") == 0) 
            return(TARGET);
         if (strcmp(temp_str, "LAYER") == 0)
            return(LAYER);
         if (strcmp(temp_str, "NODES") == 0) 
            return(NODES);
         if (strcmp(temp_str, "X-DIMENSION") == 0) 
            return(X_DIMENSION);
         if (strcmp(temp_str, "Y-DIMENSION") == 0) 
            return(Y_DIMENSION);
         if (strcmp(temp_str, "LEARN-RATE") == 0) 
            return(LEARN_RATE);
         if (strcmp(temp_str, "GLOBAL-LEARN-RATE") == 0) 
            return(GLOBAL_LEARN_RATE);
         if (strcmp(temp_str, "SCALE-FACTOR") == 0) 
            return(SCALE_FACTOR);
         if (strcmp(temp_str, "GLOBAL-MOMENTUM") == 0) 
            return(GLOBAL_MOMENTUM);
         if (strcmp(temp_str, "MOMENTUM") == 0) 
            return(MOMENTUM);
         if (strcmp(temp_str, "PATTERN-X-DIMENSION") == 0) 
            return(PATTERN_X_DIM);
         if (strcmp(temp_str, "PATTERN-Y-DIMENSION") == 0) 
            return(PATTERN_Y_DIM);
         if (strcmp(temp_str, "X-OVERLAP") == 0) 
            return(X_OVERLAP);
         if (strcmp(temp_str, "Y-OVERLAP") == 0) 
            return(Y_OVERLAP);
      ENDIF

      if (fgets(last_str, MAX_LINE_SIZE, ptr_file) == NULL)
         return(EOF);
   ENDWHILE

END /* PS_get_command */


int  PS_range_check(comparison)
int   comparison;
/*
----------------------------------------------------------------------
  Checks the results given by comparison and determines whether or not
  to print out a message. If comparison == 1, then the range check was
  OKAY, otherwise, the command had a number out of range. 
  NOTE: this routine uses the "last_str" global variable which holds
  the last command read in to NETS. This saves me the trouble of passing
  the bloody thing around.
----------------------------------------------------------------------
*/
BEGIN
   if (comparison) 
      return(OKAY);
   sprintf(IO_str, "\n *** ERROR: the command:\n %s has a value which is out of range",
           last_str);
   IO_print(0);
   sprintf(IO_str, "\n Command ignored.");
   IO_print(0);
   return(ERROR);
   
END /* PS_range_check */


int  PS_int_check(num)
float  num;
/*
----------------------------------------------------------------------
  Some of the commands in NETS need integer arguments. This routine is
  called to check that the number associated with a command was indeed
  an integer. If not, a message is printed and ERROR is returned.
  NOTE: as with the PS_range_check routine, this guy make use of the
  "last_str" global variable which is updated by PS_get_command to print
  the last command to the user.
----------------------------------------------------------------------
*/
BEGIN
   if (num == ((float) ((int) num)) )
      return(OKAY);
   
   sprintf(IO_str, "\n *** ERROR: the command:\n %s should have an INTEGER value",
           last_str);
   IO_print(0);
   sprintf(IO_str, "\n Command ignored.");
   IO_print(0);
   return(ERROR);
   
END /* PS_int_check */


void  PS_print_command(command)
int  command;
/*
----------------------------------------------------------------------
  Just a common routine for all of the layer parsing routines which 
  can be used to print out parts of error messages.
----------------------------------------------------------------------
*/
BEGIN
   switch (command) BEGIN
      case LAYER : BEGIN
         sprintf(IO_str, "LAYER");
         IO_print(0);
         break;
      ENDCASE
      case NODES : BEGIN
         sprintf(IO_str, "NODES");
         IO_print(0);
         break;
      ENDCASE
      case X_DIMENSION : BEGIN
         sprintf(IO_str, "X-DIMENSION");
         IO_print(0);
         break;
      ENDCASE
      case Y_DIMENSION : BEGIN
         sprintf(IO_str, "Y-DIMENSION");
         IO_print(0);
         break;
      ENDCASE
      case LEARN_RATE : BEGIN
         sprintf(IO_str, "LEARN-RATE");
         IO_print(0);
         break;
      ENDCASE
      case SCALE_FACTOR : BEGIN
         sprintf(IO_str, "SCALE-FACTOR");
         IO_print(0);
         break;
      ENDCASE
      case MOMENTUM : BEGIN
         sprintf(IO_str, "MOMENTUM");
         IO_print(0);
         break;
      ENDCASE
      case TARGET : BEGIN
         sprintf(IO_str, "TARGET");
         IO_print(0);
         break;
      ENDCASE
      case PATTERN_X_DIM : BEGIN
         sprintf(IO_str, "PATTERN-X-DIMENSION");
         IO_print(0);
         break;
      ENDCASE
      case PATTERN_Y_DIM : BEGIN
         sprintf(IO_str, "PATTERN-Y-DIMENSION");
         IO_print(0);
         break;
      ENDCASE
      case X_OVERLAP : BEGIN
         sprintf(IO_str, "X-OVERLAP");
         IO_print(0);
         break;
      ENDCASE
      case Y_OVERLAP : BEGIN
         sprintf(IO_str, "Y-OVERLAP");
         IO_print(0);
         break;
      ENDCASE
      default : break;
   ENDSWITCH

END /* PS_print_command */


int  PS_parse_iopairs(file_name, sum)
char  file_name[];
int   sum;
/*
----------------------------------------------------------------------
 This guy is called from "PA_setup_iopairs" to do most of the work in  
  setting up the io pairs for teaching the system.  The basic idea    
  is to read through the file (represented by "filename" above) which 
  specifies the io-pairs and sift through any comments, creating a    
  streamlined version of the io-pairs (in Sint format) which will be  
  used for the actual training.  Now, you might be wondering why we   
  don't just read in the io-pairs and store them in memory since that 
  would be a much faster approach.  The problem is that the number of 
  io-pairs can get enormous, which would put too great a burden on    
  the RAM we were attempting to use.  A more practical approach is to 
  convert the io-pairs into the exact format we need (Sints) and then 
  store them into a temporary file in a convenient format.  Then,     
  when we do our training, we can mass-read a large number of the io- 
  pairs into a buffer, and train from the buffer.  The result is a    
  compromise between the ideal case of storing all of the io-pairs in 
  memory, and the worst case of having to read all the io-pairs from  
  disk one byte at a time.    
                                        
 Returns the number of io pairs successfully read in and translated.  
  ERROR is returned if there are any errors during processing.           

 Note that this routine depends upon "the_buffer", "line_count", and  
  "last_char", all of which are variables global to netio.c           
----------------------------------------------------------------------
*/
BEGIN
   int    PS_check_items(), PS_skip_tokens(), PS_get_token();
   FILE   *fp_input, *fp_output;
   int    list_count, item_count, STATUS, i, j;
   char   next_token[MAX_WORD_SIZE];
   float  fnum;
   extern int DELETE, DEL_TRAIN, Ninput, Noutput, numtrain;

   fp_input  = fopen(file_name, "rt");
   fp_output = PA_open_binary("workfile.net", WRITE_MODE); 

   STATUS = OKAY;
   list_count = 0;
   
   if(DELETE) list_count = DEL_TRAIN; 
   else list_count = numtrain; 

   for(i=0;i < list_count;i++) {
	for(j=0;j < Ninput;j++) {
		fscanf(fp_input,"%f",&fnum); 
		if(PA_put_to_workfile(fp_output,C_float_to_Sint(fnum)) == ERROR)
			return(ERROR);
	}
	for(j=0;j < Noutput;j++) {
		fscanf(fp_input,"%f",&fnum);
		if(PA_put_to_workfile(fp_output,C_float_to_Sint(fnum)) == ERROR)
			return(ERROR);
	}
   }
   STATUS = 1;

/*   while (PS_skip_tokens(fp_input, "(" ) == OKAY) BEGIN       more lists
      list_count++;                                   incr # of lists 
      item_count = 0;                                 reset num items 
      while (PS_get_token(fp_input, next_token) == OKAY) BEGIN
         if (strcmp(next_token, ")" ) == 0)       items++ til ")" 
            break;                              
         if (sscanf(next_token, "%f", &fnum) == OKAY) BEGIN
            if (PA_put_to_workfile(fp_output, C_float_to_Sint(fnum)) == ERROR)
               return(ERROR);
            item_count++;
         ENDIF
      ENDWHILE
      STATUS = PS_check_items(item_count, sum, list_count);
   ENDWHILE */

   fclose(fp_input);
   PA_flush(fp_output);
   fclose(fp_output);
   if ((STATUS == ERROR) || (list_count == 0))
      return(ERROR);
   else return(list_count);

END /* PS_parse_iopairs */


int  PS_check_items(num_items, max_items, list_count)
int  num_items, max_items, list_count;
/*
----------------------------------------------------------------------
 A routine to check the number of items counted in an io pair list to 
  see if that number is equal to the desired length of the list.  If  
  the 'num_items' is either greater than or less than 'max_items'     
  (which is the desired length of the io pair) then ERROR is returned  
  to indicate that an error was found.  Also, to help the user in     
  finding the error, the current line is also printed to indicate     
  where the current io pair list ended.  Note that there is a trick to
  printing out the current line.  The routine 'PS_get_token' counts the  
  number of newlines, but it does not indicate whether the list was   
  ended by a newline or by a comment.  If the list was ended by a     
  comment, then 'PS_get_token' will be waiting on the SAME line as the   
  end of the list for the next token.  On the other hand, if the io   
  pair list was ended with a ")" followed by a newline, then the      
  'PS_get_token' routine will be waiting on the NEXT line.  Thus the     
  first two lines of this code check that ending condition, and set   
  the 'end_list' variable accordingly.
                           
 Note that the 'list_count' variable is passed in here for printing   
  purposes only.                                                      
----------------------------------------------------------------------
*/
BEGIN
   int  end_list;

   end_list = line_count;                       /* see where list ended */
   if (last_char == NEWLINE) end_list--;        /* decr if ended w/ nl  */
   if (num_items > max_items) BEGIN 
      sprintf(IO_str, "\n*** list number %d is too long ***\n", list_count);
      IO_print(0);
      sprintf(IO_str, "    list ends on line %d\n", end_list);
      IO_print(0);
      return(ERROR);
   ENDIF
   else if (num_items < max_items) BEGIN
      sprintf(IO_str, "\n*** list number %d is too short ***\n", list_count);
      IO_print(0);
      sprintf(IO_str, "    list ends on line %d\n", end_list);
      IO_print(0);
      return(ERROR);
   ENDELSE
   return(OKAY);

END /* PS_check_items */


int  PS_skip_tokens(fp, token)
FILE  *fp;
char  *token;
/*
----------------------------------------------------------------------
 skips through the tokens in a file until it reaches the value given  
  by the input 'token'.  Returns ERROR if unsuccessful, otherwise it    
  returns a OKAY.
----------------------------------------------------------------------
*/
BEGIN
   char   temp[MAX_WORD_SIZE];
   int    PS_get_token();
   
   while (PS_get_token(fp, temp) == OKAY) BEGIN   /* while tokens left in file */
      if (strcmp(temp, token) == 0)
         return(OKAY);
   ENDWHILE
   return(ERROR);  

END /* PS_skip_tokens */


int  PS_get_token(fp, token)
FILE  *fp;
char  *token;
/*
----------------------------------------------------------------------
 Puts a token in the input parameter 'token' returns ERROR if EOF, 
  else returns OKAY. Note that a static integer is used to keep track of
  whether or not the last character read in should be reused.
----------------------------------------------------------------------
*/
BEGIN
   static int  use_last_char = FALSEE;
   int  i;

   i = 0;
   while (TRUEE) BEGIN
      /*------------------------------------*/
      /* get the next character; if newline */
      /* then increment the line count      */
      /*------------------------------------*/
      if (use_last_char == TRUEE)
         use_last_char = FALSEE;
      else if ((last_char = getc(fp)) == EOF) 
         return(ERROR);
      if (last_char == NEWLINE) line_count++;

      /*-------------------------------------*/
      /* if token partially full, then check */
      /* for end and return; else add char   */
      /*-------------------------------------*/
      if (i > 0) BEGIN                               /* inside a word */
         if ( (last_char == SPACE)
              || (last_char == TAB)
              || (last_char == NEWLINE)
	      || (last_char == CLOSE_PAREN)
	      || (last_char == OPEN_PAREN)
              || (i == MAX_WORD_SIZE - 1) ) BEGIN
            token[i] = ENDSTRING;
            /*--------------------------------------------*/
            /* any single character token checked here    */
            /* and above; flag set here for reuse of char */
            /*--------------------------------------------*/
            if ((last_char == CLOSE_PAREN) || (last_char == OPEN_PAREN))
               use_last_char = TRUEE;
            return(OKAY);
         ENDIF
         else
            token[i++] = last_char;
      ENDIF

      /*------------------------------------*/
      /* if at beginning of token, add char */
      /* if not whitespace; return if paren */
      /*------------------------------------*/
      else BEGIN
         if ( (last_char != SPACE) 
              && (last_char != TAB) 
              && (last_char != NEWLINE) ) 
         token[i++] = last_char;
	 /*-------------------------------*/
	 /* single character tokens found */
	 /* are returned automatically    */
	 /*-------------------------------*/
         if ( (last_char == OPEN_PAREN)
	      || (last_char == CLOSE_PAREN)) BEGIN
            token[i] = ENDSTRING;
            return(OKAY);
         ENDIF
      ENDELSE

   ENDWHILE

END /* PS_get_token */


int  PS_get_float_from_file(fp, ptr_float)
FILE   *fp;
float  *ptr_float;
/*
----------------------------------------------------------------------
  This routine is used for retrieving floating point numbers from some
  input file (specified by the file pointer "fp"). It is used only
  during a "query_from_file" request made by the user. In such a 
  request, inputs to the network are read from a file and mapped onto
  the input layer.  This routine will read through any file, picking
  out floating point numbers.  The routine 'N_query_net' makes
  sure that the right number of foats are read and mapped onto the
  input layer.

 Returns OKAY if a floating point number is successfully loaded into the 
  'ptr_float' variable, otherwise it returns ERROR. It makes use of the
  the 'PS_get_token' routine to do all of the dirty work is getting the  
  input from the file.  Note that he skips over all inputs that are   
  not floating point.
----------------------------------------------------------------------
*/
BEGIN
   static char  in_string[MAX_LINE_SIZE];
   int          PS_get_token();

   while (TRUEE) BEGIN
      if (PS_get_token(fp, in_string) == ERROR)
         return(ERROR);
      if (sscanf(in_string, "%f", ptr_float) == 1)
         return(OKAY);
   ENDWHILE

END /* PS_get_float_from_file */
