/* togl.h */

/*
 * Togl - a Tk OpenGL widget
 * Version:  1.0
 * Copyright (C) 1996  Brian Paul (brianp@ssec.wisc.edu) and
 * Ben Bederson (bederson@cs.unm.edu)  See the LICENSE file for details.
 */



#ifndef TOGL_H
#define TOGL_H


#include <tcl.h>
#include <tk.h>
#include <GL/glx.h>	   /* This includes the necessary X headers */
#include <GL/gl.h>


#ifndef NULL
#define NULL    0
#endif


#ifdef __cplusplus
extern "C" {
#endif


struct Togl;

typedef void (Togl_Callback) (struct Togl *togl);
typedef int  (Togl_CmdProc) (struct Togl *togl, int argc, char *argv[]);



extern int Togl_Init( Tcl_Interp *interp );


extern void Togl_CreateFunc( Togl_Callback *proc );

extern void Togl_DisplayFunc( Togl_Callback *proc );

extern void Togl_ReshapeFunc( Togl_Callback *proc );


extern void Togl_MakeCurrent(struct Togl *togl);

extern void Togl_CreateCommand( char *cmd_name, Togl_CmdProc *cmd_proc );

extern void Togl_PostRedisplay( struct Togl *togl );

extern void Togl_SwapBuffers( struct Togl *togl );


extern char *Togl_Ident( struct Togl *togl );

extern int Togl_Width( struct Togl *togl );

extern int Togl_Height( struct Togl *togl );

extern Tcl_Interp *Togl_Interp( struct Togl *togl );


extern unsigned long Togl_AllocColor( struct Togl *togl,
                                      float red, float green, float blue );

extern void Togl_FreeColor( struct Togl *togl, unsigned long index );


extern void Togl_SetColor( struct Togl *togl, unsigned long index,
                           float red, float green, float blue );


#ifdef __cplusplus
}
#endif


#endif
