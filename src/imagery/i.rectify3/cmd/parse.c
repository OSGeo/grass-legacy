/*======================================================================
Filename:   parse.c
Module:	    i.rectify3 (cmd)
Author:	    Mike Baba


      Parse the command line, fill in *rect_data's fields.  Return NULL if
      successful, else return string containing error message.


Modifications:
30 Oct 93    - mbaba       - original 
======================================================================*/

#include "global.h"
#include "parse.h"
#include <stdlib.h>

char *Parse (int argc, char *argv[], tRect_Data *rect_data)
{
   struct Option  *group;
   struct Option  *source;
   struct Option  *target;
   struct Option  *trans;
   struct Option  *north, *south, *east, *west, *res, *nsres, *ewres;
   struct Flag *oflag, *cflag, *mflag, *qflag, *vflag;
   char msg[80];

   /* group name */
   group = G_define_option();
   group->key         = "group";
   group->type        = TYPE_STRING;
   group->required    = YES;
   group->description = "Imagery group to be rectified";


   /* the rast=from,to options. */
   source = G_define_option();
   source->key           = "source";
   source->type          = TYPE_STRING;
   source->required      = YES;
   source->gisprompt  = "old,cell,raster";
   source->description   = "Source raster file to be rectified";

   /* the rast=from,to options. */
   target = G_define_option();
   target->key           = "target";
   target->type          = TYPE_STRING;
   target->required      = YES;
   target->gisprompt  = "any,cell,raster";
   target->description   = "Target name of rectified image";

   /* transformation type */
   trans = G_define_option();
   trans->key         = "trans";
   trans->type        = TYPE_STRING;
   trans->required    = NO;
   trans->description = "Transformation type [poly1|poly2|poly3|photo|landtm]";

   /* region specification */
   /** NOTE: format_string not yet correct because need format of target location **/
   /** see g.region/cmd/main.c about llinfo **/

   /* region options */
   north = G_define_option();
   north->key         = "n";
   north->key_desc    = "value";
   north->required    = NO;
   north->multiple    = NO;
   north->type        = TYPE_STRING;
   north->description = "Value for the northern edge";

   south = G_define_option();
   south->key         = "s";
   south->key_desc    = "value";
   south->required    = NO;
   south->multiple    = NO;
   south->type        = TYPE_STRING;
   south->description = "Value for the southern edge";

   east = G_define_option();
   east->key         = "e";
   east->key_desc    = "value";
   east->required    = NO;
   east->multiple    = NO;
   east->type        = TYPE_STRING;
   east->description = "Value for the eastern edge ";

   west = G_define_option();
   west->key         = "w";
   west->key_desc    = "value";
   west->required    = NO;
   west->multiple    = NO;
   west->type        = TYPE_STRING;
   west->description = "Value for the western edge ";

   res = G_define_option();
   res->key         = "res";
   res->key_desc    = "value";
   res->required    = NO;
   res->multiple    = NO;
   res->type        = TYPE_STRING;
   res->description = "Grid resolution (both north-south and east-west)";

   nsres = G_define_option();
   nsres->key         = "nsres";
   nsres->key_desc    = "value";
   nsres->required    = NO;
   nsres->multiple    = NO;
   nsres->type        = TYPE_STRING;
   nsres->description = "North-south grid resolution";

   ewres = G_define_option();
   ewres->key         = "ewres";
   ewres->key_desc    = "value";
   ewres->required    = NO;
   ewres->multiple    = NO;
   ewres->type        = TYPE_STRING;
   ewres->description = "East-west grid resolution  ";




   /* overwrite target imagery flag  */
   oflag = G_define_flag();
   oflag->key = 'o';
   oflag->description = "Overwrite target imagery files";

   /* use current target region */
   cflag = G_define_flag();
   cflag->key = 'c';
   cflag->description = "Use current target region";

   /* calc and use minimal window */
   mflag = G_define_flag();
   mflag->key = 'm';
   mflag->description = "Use minimal target region";

   /* quite flag */
   qflag = G_define_flag();
   qflag->key = 'q';
   qflag->description = "Run quietly";

   /* verbose flag */
   vflag = G_define_flag();
   vflag->key = 'v';
   vflag->description = "Verbose output";


   /* Call the parser */
   G_disable_interactive();
   if(G_parser(argc, argv))
      exit(1);


   /* Check the group returned */
   if(group->answer == NULL)
     return("You must specify a group");
   rect_data->group = group->answer;


   /* Check the source name returned */
   if(source->answer == NULL)
     return("You must specify a source file");
   rect_data->source_name = source->answer;
      
   /* Check the target name returned */
   if(target->answer == NULL)
     return("You must specify a target file");
   rect_data->target_name = target->answer;


   /** TODO - check trans **/


   /* save all info enterd about the target region */
   rect_data->north = north->answer;
   rect_data->south = south->answer;
   rect_data->east  = east->answer;
   rect_data->west  = west->answer;
   rect_data->res   = res->answer;
   rect_data->nsres = nsres->answer;
   rect_data->ewres = ewres->answer;


   /* the flags */
   rect_data->overwrite = oflag->answer;
   rect_data->current   = cflag->answer;
   rect_data->minimal   = mflag->answer;
   rect_data->quiet     = qflag->answer;
   rect_data->verbose   = vflag->answer;


   /* do some prelimeinary checks on the region */
   /* more extensive checks in check_window() later */

   /* if cflag of mflags are set then return */
   if (rect_data->current || rect_data->minimal)
     return (NULL);

   /* do we have all of the n,s,e,w parameters */
   else if (! (rect_data->north && rect_data->south &&
	       rect_data->east  && rect_data->west)) {
     sprintf (msg, "Target region parameters [n,s,e,w] not set\n");
     G_fatal_error (msg);
   }

   /* is a resolution set */
   else if (( ! rect_data->res) &&
	    ( ! (rect_data->nsres  && rect_data->ewres))) {
     sprintf (msg, "Target region parameters [res or nwres,ewres] not set\n");
     G_fatal_error (msg);
   }

   /* should get here */
   return (NULL);
}



