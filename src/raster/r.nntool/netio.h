
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
  NET I/O STRUCTURES MODULE  
----------------------------------------------------------------------
  Much of the IO accomplished in this program requires some parsing and
  some buffering (for speed).  Below, I've defined typical "token 
  delimiters" for the parsing of the iopairs file.  Also, I have defined
  a default buffer size for a buffer to hold io pairs (in Sint format) 
  during the teaching process.  Finally, two file modes are defined so
  that I may open files for both reading and writing.
----------------------------------------------------------------------
*/

#define SPACE           ' '     /* just some white space definitions */
#define TAB             '\t'    /* for reading the input file        */
#define NEWLINE         '\n'
#define OPEN_PAREN      '('
#define CLOSE_PAREN     ')'
#define ENDSTRING       '\0'    /* special in 'C'; marks end of str  */

#define BUFFER_SIZE    1024     /* size, in bytes, used to create    */
                                /* the buffer for heavy IO (ie, for  */
                                /* reading io pairs & weights) 16384 */

#define READ_MODE       0       /* these two are used for creating   */       
#define WRITE_MODE      1       /* and reading files (like "r", "w") */


/*
----------------------------------------------------------------------
  Now come some constants used while parsing through a layer spec. 
  Layer specs consist of several commands (see 'IO_get_layer' routine)
  and the idea here is to have a constant defined for each type of
  command. 
----------------------------------------------------------------------
*/
#define GLOBAL_LEARN_RATE  1
#define GLOBAL_MOMENTUM    2
#define LAYER              3
#define NODES              4
#define X_DIMENSION        5
#define Y_DIMENSION        6
#define LEARN_RATE         7
#define SCALE_FACTOR       8
#define MOMENTUM           9
#define TARGET            10
#define PATTERN_X_DIM     11
#define PATTERN_Y_DIM     12
#define X_OVERLAP         13
#define Y_OVERLAP         14

#define LEARN_MAX         10.0
#define SCALE_MAX          1.0
#define MOMENTUM_MAX       2.0


/*
-------------------------------------------------------------------
 Below is a structure which is convenient for manipulating the     
  creation of a layer.  The idea is to read from a file and place  
  the data into the above format.  Once this is done, other code   
  will take the above structure and generate the actual Net layer  
  corresponding to the data given.  This may seem like a waste of  
  time; after all, why not just translate the data from the file   
  directly into a Layer structure?  It turns out that it was much  
  easier to handle the intermediate representation because of the  
  recursive nature of the interdependence of Layers and Weights.   
 Additionally, note that if it is ever decided that the input file 
  should look drastically different, then all I need do is change  
  the routines that read the file INTO THIS FORMAT.  Every other   
  routine which depends upon this format will be fine.  Thus by    
  separating the file format from my internal format, I save lots  
  of future headaches which could occur due to changes in input    
  file format.                                                     
-------------------------------------------------------------------
*/
typedef struct Layer_spec_str {
   int    status;             /* indicates if EOF or ERROR occurred  */
   int    ID;                 /* identification tag for the layer    */
   int    num_nodes;          /* num nodes in the layer              */
   int    X_dim;              /* The X-dimension of the layer        */
   int    Y_dim;              /*  "  Y   "       "  "    "           */
   float  learn_base;         /* used to hold the base learnng rate. */
   float  learn_scale;        /* holds scaling factor, if present    */
   float  momentum;           /* holds momemtum, if present          */
   int    num_targets;        /* num of layers as target for wts     */
   int16  targets[MAX_LAYERS][5]; 
                              /* list of target layers and specs for */
                              /* connection. [n][0]=target layer,    */
                              /* [n][1] to [n][4] are pattern specs. */
                              /* Each target may be either CONNECT   */
                              /* ALL or PATTERNED. Iff patterened,   */
                              /* then the last 4 specs of each entry */
                              /* in this array represent the P,Q,R,  */
                              /* and S specs of the pattern (see the */
                              /* documentation under 'IO_get_layer') */
                              /* Else, these last 4 = 0.             */
} Layer_spec;


/*
-------------------------------------------------------------------
 What follows is a structure used for speeding up the io during    
  training of the net.  Since we have potentially large amounts of 
  input-output pairs, it is really not feasible to read them all   
  into memory at once.  Instead, we read in a large number of the  
  inputs into a buffer and then do our io from the buffer.  If the 
  buffer runs low, then our own customized "get" routines refill   
  its contents.                                                    
 A special piece of info is necessary to keep a check on the buffer
  namely, we need to know where we currently are in the  buffer.   
  We record this in the 'index' variable.                          
-------------------------------------------------------------------
 4-11-89  While upgrading NETS to handle both weights and bias values
  I ran into a problem. The routine IO_get_from_workfile has no idea
  how much of its buffer (below) actually contains good values. Here
  I have added another element to the buffer structure to keep track of
  the number of elements which are actually read in to the buffer.
  Then, this value can be consulted to see if IO_get_from_workfile 
  has exceed the last value somewhere in the middle of the buffer!
  NOTE THAT LAST_ELEM IS AN INDEX ie, it indicates where the last 
  valid entry is in the buffer.
-------------------------------------------------------------------
*/
typedef struct buffer_str {
   Sint  values[BUFFER_SIZE];
   int   index;
   int   last_elem;
} buffer;
