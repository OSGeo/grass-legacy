#ifndef __STDTYPES_H
#define __STDTYPES_H


// @M
#define pure =0

#ifndef FALSE
#define FALSE 0
#define TRUE  1
#endif
#define HIDDEN static

#define EOL             -1
#define NOERR   0
// @END

// @T
typedef unsigned char UINT8;
#ifdef M_I386
typedef unsigned short UINT16;
#else
typedef unsigned int UINT16;
#endif
typedef unsigned long UINT32;

typedef char INT8;
#ifdef M_I386
typedef short INT16;
#else
typedef int INT16;
#endif
typedef long INT32;



// typedef unsigned char  bool;
typedef unsigned short word;
typedef unsigned long  dword;
typedef unsigned char  byte;
typedef unsigned short KEY;

#define UNDEFINED -32767

#undef _SVID

// @END


// @FPUB
// @END

#endif // __STDTYPES_H



