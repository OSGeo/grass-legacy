
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
  COMMON DATA MODULE  
----------------------------------------------------------------------
  This module has all of the types which are global to the neural  
   net system, but which are not part of any particular structure  
   within the net.  Global constants, flags, time variables, etc   
   are the kinds of things which get declared here.  I guess you   
   can think of this as the global area for the overall program.   
  Some notes on nomenclature:  Capitalized letters are used here   
   for types which are declared for this program.  If those type   
   definitions are not here, they will be in a file that goes with 
   this system (ie, either: 'net.h' 'layer.h' or 'weights.h')      
  Also, note that the other include.h files are included at the    
   bottom of this file.  This is done so that the '.c' files need  
   only include this file to get their hands on everything.        
-------------------------------------------------------------------
*/


/*
-------------------------------------------------------------------
  PORTABILITY SPECIFICATION
-------------------------------------------------------------------
  Set only ONE of the constants below to "1" depending upon which 
  machine/compiler you are using. Set the rest to "0" (zero).
-------------------------------------------------------------------
*/

#define VMS              0
#define IBM_MSC          0
#define IBM_TBC          0
#define LSPEED           0
#define UNIX_ATT         0
#define UNIX_BER         1
#define WIZARD           0


/*
-------------------------------------------------------------------
  DELIVERY SPECIFICATION
-------------------------------------------------------------------
  Set the "DELIVERY" constant to "0" (zero) when compiling NETS in 
  the normal mode. Set it to "1" when compiling delivery code.
-------------------------------------------------------------------
*/
#define DELIVERY         0


/*
-------------------------------------------------------------------
  SCALED INTEGER SPECIFICATION
-------------------------------------------------------------------
  If the Sint (Scaled Integer) format is desired, set the constant
  below to "1". Otherwise, setting the constant to "0" (zero) will
  cause NETS to use floating point format. The Sint format runs 
  much faster on most machines, but has less precision.
-------------------------------------------------------------------
*/
#define USE_SCALED_INTS  1



/*
-------------------------------------------------------------------
  INCLUDE FILES
-------------------------------------------------------------------
  Below are the include files needed by each of the various compilers.
-------------------------------------------------------------------
*/

#include  <stdio.h>      /* standard include files for C programs    */
#include  <math.h>

#if   VMS
#include <timeb>
#endif

#if   IBM_MSC
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#include <conio.h>
#include <malloc.h>
#include <sys\timeb.h>
#endif

#if   UNIX_ATT
#include <sys/types.h>
#include <sys/times.h>
#endif

#if   UNIX_BER
#include <sys/types.h>
#include <sys/timeb.h>
#endif

#if  IBM_TBC
#include <stdlib.h>
#include <time.h>
#include <dos.h>
#include <alloc.h>
#endif

#if  WIZARD
#include <stdlib.h>
#include <time.h>
#include <system.h>
#include <string.h>
pragma Off (Char_default_unsigned);
#endif

#if  LSPEED
#include <unix.h>
#include <storage.h>
#include <time.h>
#endif


/*
-------------------------------------------------------------------
  COMMON DEFINITIONS
-------------------------------------------------------------------
  This section contains the rest of the constant definitions needed
  in common across all the files in NETS.
-------------------------------------------------------------------
*/

      /*--------------------------------------------------*/
      /* first some redefinitions of "{" and "}" to help  */
      /* me keep track of my begins and ends              */
      /*--------------------------------------------------*/
      
#define BEGIN       {    /* English versions of the familiar "{" and */
#define END         }    /* "}" tokens of C.                         */
#define ENDIF       }
#define ENDFOR      }
#define ENDELSE     }
#define ENDCASE     }
#define ENDWHILE    }
#define ENDSWITCH   }

      /*--------------------------------------------------*/
      /* Next, SEEK_SET is defined for use by the funct.  */
      /* "fseek" (see pairs.c). Some compilers predefine. */
      /*--------------------------------------------------*/

#ifndef  SEEK_SET
#define  SEEK_SET   0
#endif

      /*--------------------------------------------------*/
      /* Definitions for the scaled integer format which  */
      /* I call "Sint."  Note that Sints are assumed to   */
      /* be 2 bytes on all machines, thus the definitions */
      /* for each machine are changed appropriately. Also */
      /* note that a Sint is an int with 10 bits assumed  */
      /* after the decimal. That's 2^10 = 1024 which is   */
      /* a constant needed during conversion operations.  */
      /* (also, see below for Sint type definitions).     */
      /*--------------------------------------------------*/

#define SINT_BYTES   2 
#define SINT_SCALE   1024 
#define SCALE_LOG2   10      /* log base 2 of the SINT_SCALE value */
#define MAX_SINT     32000
#define MIN_SINT    -32000
#if  USE_SCALED_INTS
#define WTS_SCALE    1000    /* Should be set to 1 * 10^precision, where */
                             /* precision = number of sig figs after the */
                             /* decimal point in a Sint.                 */
#else
#define WTS_SCALE    100000  /* more precision with floating points      */
#endif

      /*--------------------------------------------------*/
      /* There are several constants needed to define the */
      /* various network parameters such as layer size,   */
      /* type of connectivity between layers, and limits  */
      /* imposed during learning. Most notable is the     */
      /* MAX_NODES constant which is defined differently  */
      /* depending upon whether Sint or float format is   */
      /* used. Normally, 32K would be the upper limit due */
      /* the limit if 16-bit ints, but 10K turns out to   */
      /* be practical if Sints are used. Anything above   */
      /* 10K produces numbers past the precision of Sints */
      /*--------------------------------------------------*/

#define MAX_LAYERS   20
#define IN_LAYER     0
#define OUT_LAYER    1 

#if   USE_SCALED_INTS
#define MAX_NODES    10000 
#else
#define MAX_NODES    32000 
#endif

#define PATTERNED    11     /* For patterned connection schemes */
#define CONNECT_ALL  21     /* For fully-connected layers       */
#define MAX_CYCLES   10000  /* Most cycles allowed during learning */  
                         
      /*--------------------------------------------------*/
      /* Below are some constants for the two different   */
      /* file formats which NETS produces. The first two  */
      /* constants indicate which format to print. Then   */
      /* come two signature constants which are used to   */
      /* tell binary format from text format. That is, if */
      /* a binary format is specified but a text file is  */
      /* given as the argument, the first read will not   */
      /* reveal the signature thus an error will be cited */
      /* Note that different signatures are used for the  */
      /* Sint and float formats. The signature numbers    */
      /* don't mean anything, they're just low numbers of */
      /* the ascii table.                                 */
      /*--------------------------------------------------*/

#define PORTABLE_FORMAT  1   /* Flag for ASCII load/save of weights  */
#define FAST_FORMAT      0   /* Flag for BINARY load/save of weights */

#if  USE_SCALED_INTS
#define  BIN_SIG         0x1701    /* Star Trek Enterprize number */
#define  WRONG_SIG       0x1812    /* Famous battle date          */
#else
#define  BIN_SIG         0x1812    /* reverse if in float format  */
#define  WRONG_SIG       0x1701   
#endif
#define  SIG_SIZE        2         /* all signatures are 2 bytes  */
                               
      /*--------------------------------------------------*/
      /* Miscellaneous constant definitions.              */
      /*--------------------------------------------------*/

#define MAX_SUM       7000  /* values over this always produce an */
                            /* activation = 1 when using Sint     */
#define MAX_LINE_SIZE  256  /* for input buffers */
#define MAX_DISPLAY     19  /* Number of lines printed per screen */
#define MAX_WORD_SIZE   75  /* the longest unbroken chain of chars */
                            /* allowed in an input file, where     */
                            /* 'unbroken' is defined by the white  */
                            /* space characters above.             */ 

#define TRUEE          1
#define FALSEE         0

#define ERROR        -13
#define OKAY          1
#define UNDEFINED    -1     /* says global_learn_rate/momentum haven't */
                            /* been set to anything in net spec file   */

#ifndef  PI                            /* if not defined, define PI */
#define  PI  (3.14159265358979323846)  /* for use in lnrate.c file  */
#endif                                 /* when doing FLOAT not Sint */


/*
-------------------------------------------------------------------
  COMMON TYPEDEF'S
-------------------------------------------------------------------
  Below are the type definitions for NETS. Using typedefs allows 
  most of the code to remain the same regardless of which machine
  is used. Rather than depending upon the whim of the integer size
  of the machine, the code simply uses the type definitions made
  here. Thus, machine dependencies can be taken care of in one place.
-------------------------------------------------------------------
*/

      /*--------------------------------------------------*/
      /* First the scaled integer typedefs. It is assumed */
      /* that Sints are 2 bytes long, which means using   */
      /* the short int format. If floats are desired, the */
      /* "Sint" typedef is simply a renaming of "float."  */
      /* For multiplication reasons, double sized sints   */
      /* or D_Sints are needed (ie, you can't multiply 2  */
      /* short ints together and always end up with a     */
      /* a short, thus longs are used). Again, if floats  */
      /* are desired, D_Sint = float.                     */
      /*--------------------------------------------------*/

#if    USE_SCALED_INTS
typedef short int Sint;
#else           
typedef float  Sint;
#endif

#if  USE_SCALED_INTS && (IBM_MSC || IBM_TBC || LSPEED)
typedef long int D_Sint;
#endif
#if  USE_SCALED_INTS && (VMS || UNIX_ATT || UNIX_BER || WIZARD)    
typedef int D_Sint;
#endif
#if  !USE_SCALED_INTS
typedef float  D_Sint;
#endif

      /*--------------------------------------------------*/
      /* Finally, I also define 16-bit and 32-bit ints so */
      /* that I can guaranty specific sizes when needed.  */
      /* This allows NETS to save memory on big machines  */
      /*--------------------------------------------------*/

#if  IBM_MSC || IBM_TBC || LSPEED
typedef int int16;
typedef long int int32;
#endif 
#if  VMS || UNIX_ATT || UNIX_BER || WIZARD
typedef short int int16;
typedef int int32;
#endif


