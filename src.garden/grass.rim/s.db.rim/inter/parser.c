#define PARSER

#include "gis.h"
#include "globals.h"
#include "parse.h"
#include "opcode.h"



int parser(instruct, inp_line)
int instruct;
char *inp_line;
{
   static int parse_state = P_CMD_STATE,
              read_only_mode = FALSE;

   char buffer[INP_SIZ], localbuf[100];
   int cmd_num, opcode;

#ifdef DEBUGP
   fprintf(stderr,"\nNow entering parser with instruct=%d and inp_line=%s\n",
            instruct, inp_line);
   fprintf(stderr,"\nOn entry parse_state=%d and read_only_mode=%d\n",
            parse_state, read_only_mode);
#endif

   if (instruct==P_INST_DB_NA) { /* database not open and must be made!! */
      parse_state = P_NOTMADE_STATE;
      return(NOP);
   }
   else if (instruct==P_INST_RONLY) {  /* database open in the read only mode */
      read_only_mode = TRUE;
      return(NOP);
   }
   else if (instruct==P_INST_RET_STATE) { /* return the parser state */
      return(parse_state);
   }
   else if (instruct==P_INST_RET_MODE) { /* return the read only mode flag */
      return(read_only_mode);
   }
   else if (instruct==P_INST_RESET) { /* reset parser state and modes */
      read_only_mode = FALSE;
      parse_state = P_CMD_STATE;
   }
   else {
      sprintf(localbuf, "Unrecognized instruction to parser ==> %d ",
              instruct);
      G_fatal_error(localbuf);
   }
}

