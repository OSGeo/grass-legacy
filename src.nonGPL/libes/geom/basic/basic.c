/* basic/basic.c --- Basic C Library, core routines and version information. */

/*---------------------------------------------------------------------------*/

#ifndef lint
  static char version[] = "@(#) Basic C Library 0.99";
  static char author[] = "Ernst Mucke, et al";
# include "copyright.h"
#endif

/*---------------------------------------------------------------------------*/

#include "geom/basic.h"
#include <stdarg.h>   /* Hey, we're using stdarg! */

/*---------------------------------------------------------------------------*/

char basic_charbit_mask__[8] = {  128,  64,  32,  16,  8,  4,  2,  1 };
char basic_charbit_mask_n[8] = { ~128, ~64, ~32, ~16, ~8, ~4, ~2, ~1 };

/* NOTE: The masks must be global.  I don't see any reasonable way around it,
   except, giving up the macros. */

/* The headerfile basic.h defines the following macros:

   * macro charbit_on(I,C) returns TRUE iff bit I in char C is set to 1;
   * macro charbit_s1(I,C) sets bit I in char C to 1;
   * macro charbit_s0(I,C) sets bit I in char C to 0;
     usage eg.:  c = charbit_s1 (2,c);
   */

/*---------------------------------------------------------------------------*/

static void (* hook) () = NULL;

/* Global constants used in definitions in basic.h  --- don't tell anybody! */
char basic__assert_format[] = "Assertion failed in file \"%s\", at line %d.";
void (* basic__null_hook) () = NULL;
     
/*--------------------------------------------------------------------------*/

/*VARARGS0*/
/*ARGSUSED*/ 
void basic_relax ( char *a,...)
       /* Just a dummy function, with a variable number of arguments. */
{ 
}

/*--------------------------------------------------------------------------*/

/*VARARGS0*/
/*ARGSUSED*/ 
int basic_relaxf (char *a,...)
       /* Dummy function, with a variable number of arguments, returning 0. */
{
  return (0);
}

/*--------------------------------------------------------------------------*/

/*VARARGS0*/ 
void basic_error (char *fmt,...)
       /* Procedure basic_error (format [, arg] ...) has a variable number
          of arguments.  It produces an message just like fprint or vfprint.
          Then it either aborts execution or calls a given error hook. */
{ /* See also headerfile <varargs.h> and % man varargs. */
  va_list args;
  char *msg;
  va_start (args,fmt);
  basic_cb_push_buffer ();
  (void) basic_cb_vprintf (fmt, args);
  msg = STRDUP (basic_cb_str ());
  basic_cb_pop_buffer ();
  va_end (args);
  if (hook)
    { /* call hook */
      hook (msg);
    }
  else
    { /* print error message and abort execution (with coredump) */
      fprintf (stderr, "%s\n", msg);
#ifdef sgi
      (void) abort ();
#else
      abort ();
#endif      
    }
  FREE (msg);
}  

/*--------------------------------------------------------------------------*/

void 
basic_error_hook (void (*user_error)(void))
     /* To specify an error hook call basic_error_hook (my_error) assuming
        my_error is a pointer to a function:
        .
        .             void my_error (error_message)
        .                char error_message[];
        .            { ... }
        .
        With this, basic_error() will generate the error_message and pass it
        to my_error().  This function might then do some cleanup and/or cause
        segmentation fault for dbx.  Use basic_error_hook (NULL_HOOK) to get
        default behaviour back. */
{
  hook = user_error;
}

/*---------------------------------------------------------------------------*/

int 
basic_kbytes (int bytes)
     /* Converts bytes to kilobytes (going to next higher integer). */
{
  return (If ((bytes > 0), bytes / 1024 + 1, bytes));
}

/*--------------------------------------------------------------------------*/

double 
basic_mbytes (int bytes)
     /* Converts bytes to megabytes. */
{
  return (bytes / 1024.0 / 1024.0);
}

/*--------------------------------------------------------------------------*/

char *
basic_strip (char *s)
     /* Returns a pointer to the first character in s that is not
        a special character, or NULL if no such character exists.
        Cf, man strpbrk(). */
{
  return ((char*) strpbrk 
          (s,
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"));
}

/*--------------------------------------------------------------------------*/

/*VARARGS0*/ 
void basic_system (char *fmt,...)
{ /* See also headerfile <stdarg.h> and % man varargs. */
  va_list args;
  char *cmd;
  int result;
  va_start (args,fmt);
  basic_cb_push_buffer ();
  (void) basic_cb_vprintf (fmt, args);
  cmd = STRDUP (basic_cb_str ());
  basic_cb_pop_buffer ();
  va_end (args);
  result = system (cmd);
  if (result != 0)
    print ("WARNING: basic_system(\"%s\") status=%d\n", cmd, result);
  FREE (cmd);
}  
