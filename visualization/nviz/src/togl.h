/* togl.h */

/*
 * Togl - a Tk OpenGL widget
 * $Id$
 *
 * Version:  1.0
 * Copyright (C) 1996  Brian Paul (brianp@ssec.wisc.edu) and
 * Ben Bederson (bederson@cs.unm.edu)  See the LICENSE file for details.
 */



#ifndef TOGL_H
#define TOGL_H


#include <tcl.h>
#include <tk.h>
#if (TK_MINOR_VERSION) >= 3
#include "tkInt8.3.h"
#else
#include "tkInt8.0.h"
#endif
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

struct Togl
{
    Tk_Window  tkwin;		/* Tk window structure */
    Tcl_Interp *interp;		/* Tcl interpreter */
    GLXContext cx;		/* OpenGL graphics context */
    int width, height;		/* Dimensions of window */

    int rgba_flag;		/* configuration flags (true/false) */
    int double_flag;
    int depth_flag;
    int accum_flag;
    int alpha_flag;
    int stencil_flag;
    int privatecmap_flag;

    char *ident;		/* Widget type */
    GLboolean update_pending;	/* Flag true when render scheduled */
    Togl_Callback *create_proc;	/* Callback when widget is created */
    Togl_Callback *render_proc;	/* Callback when widget is rendered */
    Togl_Callback *resize_proc;	/* Callback when window size changes */
};



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

void create_cb(struct Togl *);
void reshape_cb(struct Togl *);
void display_cb(struct Togl *);
void swap_togl(void);

#ifdef __cplusplus
}
#endif


#endif
