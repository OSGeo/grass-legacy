
/* This "distance.h" contains the include to "curses.h"                      */
#include <curses.h>
/* NOTE:  When executing "Gmake" the variable "CURSES" should equal          */
/* "-lcurses $(TERMLIB)" when using this "distance.h" which contains the     */
/* the include to "curses.h".                                                */


/* Choose one of these "PRT_COMMAND"'s for your site:                        */
/* (This "PRT_COMMAND" is the command used within the program to send the    */
/* distance information to the printer.)                                     */
/* If you do not want to use the default "print" command "lp" then comment   */
/* out the default command and then uncomment another "PRT_COMMAND" to use,  */
/* or modify or create your own "PRT_COMMAND" to be used for printing.       */
/*                                                                           */
/* Default command for sending distance information to printer.              */
#define PRT_COMMAND "lp    "                                              
/* Example command for sending information to printer with options "-oh".    */
/* #define PRT_COMMAND "lp -oh"                                              */
/* Example command to be used when your system doesn't have a printer.       */
/* #define PRT_COMMAND "cat>/dev/null"                                       */
/* Example command for sending information to "nps" system printer that is   */
/* networked to this present system.                                         */
/* #define PRT_COMMAND "rsh nps lp -oh"                                      */
/* Example command for "lpr" printer.                                        */
/* #define PRT_COMMAND "lpr   "                                              */

/* These are the GRASS libraries used by this program:                       */
/* Include "/user/grass3.1/src/libes/gis.h" while being at                   */
/* location:  "/user/grass3.1/src.alpha/D/prog_inter/distance".              */
#include "gis.h"
/* Include "/user/grass3.1/src/mapdev/lib/dig_structs.h while being at       */
/* location:  "/user/grass3.1/src.alpha/D/prog_inter/distance".              */
#include "/home/archive/grass3/src/mapdev/lib/dig_structs.h"
/* Include "/user/grass3.1/src/mapdev/lib/dig_defines.h while being at       */
/* location:  "/user/grass3.1/src.alpha/D/prog_inter/distance".              */
#include "/home/archive/grass3/src/mapdev/lib/dig_defines.h"

#include <stdio.h>
#define abs(x)			(x>=0 ? x : -(x))

struct Cell_head window;
/* "pipe_ptr" is the pointer for printing.                                   */
FILE *pipe_ptr;


/****************************** ARC INFORMATION ******************************/
#define TOTAL_ARCS  100 
struct ARC_TABLE
 {
  long int arc;
 };
struct ARC_TABLE *ptr_arc, *ptr_arc_var;
struct ARC_T_INFO
 {
  long int total;
  long int count;
  struct ARC_TABLE *addr_first_arc, *addr_last_arc;
 } arc_t_info;
struct PRESENT
 {
  double n;
  double e;
  long int arc;
  long int segment;
  long int indicator;
  long int node;
 } present;
struct LAST_M_PT 
 {
  double n;
  double e;
  long int arc;
  long int segment;
  double arc_dist_i_lm;
  double dir_dist_i_lm;
  long int indicator;
  long int node;
 } last_m_pt;
struct INITIAL
 {
  double n;
  double e;
  long int arc;
  long int segment;
  long int indicator;
  long int node;
 } initial;
struct TERMINAL
 {
  double n;
  double e;
  long int arc;
  long int segment;
  double arc_dist_i_t;
  double dir_dist_i_t;
  long int indicator;
  long int node;
 } terminal;

/* "indicator" for PRESENT, INITIAL, TERMINAL, and LAST_M_PT structures are  */
/* defined as follows:                                                       */
/*                                                                           */
/* Indicator | Definition                                                    */
/*           |                                                               */
/*     1     | I & N (N1)                                                    */
/*     2     | I & N (N2)                                                    */
/*     3     | I & B                                                         */
/*     4     | I & E                                                         */
/*     5     | I & M                                                         */
/*           |                                                               */
/*     6     | T & N (N1)                                                    */
/*     7     | T & N (N2)                                                    */
/*     8     | T & B                                                         */
/*     9     | T & E                                                         */
/*     0     | T & M                                                         */
/*                                                                           */
/* NOTE: I is initial point (first point for measured line)                  */
/*       N is node point                                                     */
/*       B is beginning point of a segment                                   */
/*       E is ending point of a segment                                      */
/*       M is middle point of a segment and is not either of the end points  */
/*       T is terminal point (last point for measured line)                  */
/*       N1 is the beginning node number for an arc                          */
/*       N2 is the ending node number for an arc                             */

/* Definitions of colors used for measuring existing arcs */
#define IN_COLOR  "gray"   /* initial node color */
#define IA_COLOR1 "brown"  /* initial arc color 1 */
#define IA_COLOR2 "aqua"   /* initial arc color 2 */
#define IP_COLOR  "red"    /* initial point plus color */ 
#define TP_COLOR  "violet" /* terminal point plus color */ 
#define BS_COLOR  "orange" /* beginning segment plus color */
#define ES_COLOR  "green"  /* ending segment plus color */
#define PP_COLOR  "white"  /* present point plus color */
#define IT_COLOR  "yellow" /* initial or terminal segment color */
#define HL_COLOR1 "violet" /* highlighted color 1 for segment or arc */
#define HL_COLOR2 "green"  /* highlighted color 2 for segment or arc */
#define MA_COLOR  "red"    /* measured arc color (from initial to "lm" pt) */
#define BG_COLOR  "black"  /* back ground color for entire screen */
#define LM_COLOR  "violet" /* last measured plus from initial pt to "lm" pt */
#define DD_COLOR  "yellow" /* direct distance line color (from I-pt to T-pt) */
#define WI_COLOR  "white"  /* color of WIND boundary */


/***************************** NODE INFORMATION ******************************/
#define TOTAL_NODES 100
struct NODE_TABLE
 {
  long int node_number;
 };
struct NODE_TABLE *ptr_node, *ptr_node_var;
struct NODE_T_INFO
 {
  long int total;
  long int count;
  double cum_dist;
  double dir_dist;
 } node_t_info;

/* Definitions of colors used for measuring direct distances */
#define N1_COLOR  "green"   /* selected node color */
#define N2_COLOR  "red"     /* first node color */
#define N3_COLOR  "violet"  /* last node color */


/***************************** EXTRACTION INFORMATION ************************/
struct HEADER
 {
  char organization[30] ;
  char date[20] ;
  char your_name[20] ;
  char map_name[41] ;
  char source_date[11] ;
  long  orig_scale ;
  char line_3[73] ;
  int plani_zone ;
  double W, E, S, N ;
  double digit_thresh ;
  double map_thresh ;
 } dig_header;
/* "dig_asc" is the pointer to the digit ascii file that this program        */
/* will create in directory "dig_ascii".                                     */
FILE *dig_asc;
/* "dig_bin" is the pointer to the digit binary file that this program       */
/* will create in directory "dig".                                           */
FILE *dig_bin;
/* "total_points" is the total number of points of the measured line to be   */
/* extracted.                                                                */
long int total_points;
/* "*x_array" is a pointer to the array of X values of the measured line to  */
/* be extracted to the binary digit file.                                    */
double *x_array;
/* "x_counter" counts the number of X values in "x_array".                   */
long int x_counter;
/* "*y_array" is a pointer to the array of Y values of the measured line to  */
/* be extracted to the binary digit file.                                    */
double *y_array;
/* "y_counter" counts the number of Y values in "y_array".                   */
long int y_counter;
/*****************************************************************************/


/* You can turn "curses" off by uncommenting the following line:             */
/* #define NO_CURSES                                                         */
/* NOTE:  If you are compiling on a Sun then make sure that for your         */
/* environmental variable "PATH" that you have directory "/usr/bin" before   */
/* directory "usr/5bin" due to the fact you are using the "curses" library.  */


/* You can create an "ascii" digit file rather than the default which is the */
/* creation of a "binary" digit file by uncommenting the following "define"  */
/* statement:                                                                */
/* #define ASCII                                                             */


/******************************** DEBUG **************************************/
/* You can turn your debugging on for this program by uncommenting the       */
/* following debug variables:                                                */ 
/* NOTE:  You must uncomment "NO_CURSES" as well if you turn on the "debug"  */
/* variables.                                                                */
/*
#define DEBUG  
#define DEBUG_H
#define DEBUG_J
#define DEBUG_P
#define DEBUG_N
#define DEBUG_M
#define DEBUG_M
#define DEBUG_O
#define DEBUG_vp
#define DEBUG_e4o
#define DEBUG_dmpe
#define DEBUG_o1a
#define DEBUG_pos
#define DEBUG_cs
*/
