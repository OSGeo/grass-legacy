#define PARSER

#include "gis.h"
#include "globals.h"
#include "parse.h"
#include "opcode.h"


/* Here are the definitions for the dot commands. */
struct cmd_info {
  char name[10];
  int min_chars,
  opcode,
  p_state,
  read_only;
  char desc[67];                /* help text */
} commands[] = {{"add", 1, ADD_OP, P_ADD_STATE, FALSE,
                   "Add a new record (row) to data base."},
                  {"backup", 1, BACKUP_OP, P_CMD_STATE, TRUE,
                     "Create a text file dump of data base."},
                  {"change", 1, CHANGE_OP, P_CHANGE_STATE, FALSE,
                     "Change individual fields in a record."},
                  {"delete", 1, DELETE_OP, P_DELETE_FOUND_STATE, FALSE,
                     "Delete individual records or all records in the query list ."},
                  {"end", 1, END_OP, P_CMD_STATE, TRUE,
                     "Required terminator line for some commands."},
                  {"exit", 2, EXIT_OP, P_CMD_STATE, TRUE,
                     "Exit, Quit, Terminate cleanly."},
                  {"find", 1, FIND_INIT_OP, P_FIND_STATE, TRUE,
                     "Create 'query list' of sites closest to target location."} ,
                  {"help", 1, HELP_OP, P_CMD_STATE, TRUE,
                     "Print this help information."},
                  {"input", 1, INPUT_OP, P_CMD_STATE, TRUE,
                     "Specify file to process as program commands."},
                  {"list", 1, LIST_OP, P_CMD_STATE, TRUE,
                     "List available vector data bases by mapset."},
                  {"make", 3, MAKE_OP, P_MAKE_STATE, FALSE,
                     "Create a new vector data base."},
                  {"map", 3, MAP_OP, P_CMD_STATE, TRUE,
                     "List/Add/Delete entries in the reference map list."},
                  {"output", 1, OUTPUT_OP, P_CMD_STATE, TRUE,
                     "Specify file for output of text results."},
                  {"pack", 2, PACK_OP, P_CMD_STATE, FALSE,
                     "Unload data base to text file and reload to recover space.  "},
                  {"print", 1, PRINT_OP, P_CMD_STATE, TRUE,
                     "Print records in 'query list' after .find or .query."},
                  {"query", 1, QUERY_OP, P_QUERY_STATE, TRUE,
                     "Create 'query list' of records using 'SQL' request format.  "},
                  {"read_vect", 2, READ_VECT_OP, P_CMD_STATE, FALSE,
                     "Read a vector map into the data base as records."},
                  {"remove", 6, REMOVE_OP, P_CMD_STATE, FALSE,
                     "Remove (Delete, Trash) an entire data base.  Be careful!"},
                  {"show", 2, SHOW_OP, P_CMD_STATE, TRUE,
                     "Print the record format."},
                  {"site_list", 2, SITE_LIST_OP, P_CMD_STATE, TRUE,
                     "Write selected sites to a GRASS site list."},
                  {"tables", 1, TABLES_OP, P_CMD_STATE, TRUE,
                     "Print the table structure of the data base."},
                  {"vector_map", 1, VECT_MAP_OP, P_CMD_STATE, TRUE,
                     "Create a GRASS vector map in current mapset from 'query list'."},
                  /* The following row must always be the last in this array*/
                  {"", 0, SYNTAX_ERR, P_CMD_STATE, TRUE, ""}
                };

#define NUMBER_COMMANDS sizeof(commands) / sizeof(struct cmd_info)

help()
{
  int i;

  fprintf(Outfile,"\n\n     v.db.rim:  Command  Summary\n\n");
  for (i=0; i < NUMBER_COMMANDS-1; i++)
    fprintf(Outfile,".%-10s %s\n", commands[i].name, commands[i].desc);
  fprintf(Outfile,"!command    Execute a shell command line.");
  fprintf(Outfile,"\n");
}





int parser(instruct, inp_line)
int instruct;
char *inp_line;
{
  static int parse_state = P_CMD_STATE, read_only_mode = FALSE;

  char buffer[INP_SIZ], localbuf[100];
  int cmd_num, opcode;

#ifdef DEBUGP
  fprintf(stderr,"\nNow entering parser with instruct=%d and inp_line=%s\n",
          instruct, inp_line);
  fprintf(stderr,"\nOn entry parse_state=%d and read_only_mode=%d\n",
          parse_state, read_only_mode);
#endif

  /* copy the input line and change it to lower case */
  strcpy(buffer, inp_line);
  G_squeeze(buffer);
  G_tolcase(buffer);

  if (instruct==P_INST_PARSE) { /* parse the input line and return an opcode */
    cmd_num = match_cmd(buffer);
    opcode = commands[cmd_num].opcode;
    if (strlen(buffer)<1) opcode = NOP; /* an empty line is a nop */
    if (read_only_mode==TRUE && (parse_state!=P_CMD_STATE &&
                                 parse_state!=P_QUERY_STATE &&
                                 parse_state!=P_FIND_STATE)) {
      sprintf(localbuf, "Illegal parser state '%d' while in read only mode.",
              parse_state);
      G_fatal_error(localbuf);
    }
    switch (parse_state) {
    case P_NOTMADE_STATE:       /* Database needs to be made, only accept .make */
      if (opcode == SYNTAX_ERR)
        if (buffer[0]==SHELL_ESCAPE_CHAR) {
          G_system(&inp_line[strspn(inp_line," \t")+1]);
          opcode = NOP;
        }
      if ((opcode==SYNTAX_ERR)||(opcode==LIST_OP)||(opcode==EXIT_OP)
          ||(opcode==INPUT_OP)||(opcode==OUTPUT_OP)
          ||(opcode==HELP_OP))
        break;
      else if (opcode==MAKE_OP)
        parse_state = commands[cmd_num].p_state;
      else {
        fprintf(Outfile,"Warning: You must make a database before you can continue.\n");
        opcode = NOP;
      }
      break;

    case P_CMD_STATE:           /* Expecting a new dot command */
      if (opcode == SYNTAX_ERR)
        if (buffer[0]==SHELL_ESCAPE_CHAR) {
          G_system(&inp_line[strspn(inp_line," \t")+1]);
          opcode = NOP;
        }
      if (read_only_mode==TRUE && commands[cmd_num].read_only==FALSE) {
        fprintf(Outfile,"Warning: Command not available with a read-only database.\n");
        opcode = NOP;
      }
      else {
        if (opcode==MAKE_OP) {
          fprintf(Outfile,"Warning: Database already exists. Make command ignored.\n");
          opcode = NOP;
          parse_state = P_CMD_STATE;
        }
        else
          parse_state = commands[cmd_num].p_state;
      }
      break;

    case P_MAKE_STATE:          /* In the midst of a .make */
      if (opcode==END_OP) {
        opcode = MAKE_DONE_OP;
        parse_state = P_CMD_STATE;
      }
      else
        opcode = MAKE_ADD_OP;
      break;

    case P_ADD_STATE:           /* In the midst of a .add */
      if (opcode==END_OP){
        opcode = ADD_DONE_OP;
        parse_state = P_CMD_STATE;
      }
      else
        opcode = ADD_FIELD_OP;
      break;

    case P_DELETE_FOUND_STATE:  /*in the midst of a .delete from last query*/
      if (opcode==END_OP) {
        opcode = DELETE_FOUND_OP;
        parse_state = P_CMD_STATE;
      }
      else {
        opcode = DELETE_SITE_OP;
        parse_state = P_DELETE_LIST_STATE;
      }
      break;

    case P_DELETE_LIST_STATE:   /*in the midst of a .delete from and list*/
      if (opcode==END_OP){
        opcode = DELETE_DONE_OP;
        parse_state = P_CMD_STATE;
      }
      else
        opcode = DELETE_SITE_OP;
      break;

    case P_QUERY_STATE:         /* In the midst of a .query */
      if (opcode==END_OP){
        opcode = QUERY_DONE_OP;
        parse_state = P_CMD_STATE;
      }
      else
        opcode = QUERY_LINE_OP;
      break;

    case P_CHANGE_STATE:        /* In the midst of a .change */
      if (opcode==END_OP){
        opcode = CHANGE_DONE_OP;
        parse_state = P_CMD_STATE;
      }
      else
        opcode = CHANGE_FIELD_OP;
      break;

    case P_FIND_STATE:          /* In the midst of a .find */
      if (opcode==NOP) break;   /*ignore blank lines*/
      if (opcode!=SYNTAX_ERR) {
        opcode = SYNTAX_ERR;
        fprintf(Outfile,"Warning: No coordinates provided for .find command.\n") ;
      }
      else
        opcode = FIND_OP;
      parse_state = P_CMD_STATE;
      break;

    default:                    /* Unrecognized parse_state */
      sprintf(localbuf, "Unrecognized parse_state ==> %d ",
              parse_state);
      G_fatal_error(localbuf);
    }
    return(opcode);
  }
  else if (instruct==P_INST_DB_NA) { /* database not open and must be made!! */
    parse_state = P_NOTMADE_STATE;
    return(NOP);
  }
  else if (instruct==P_INST_RONLY) { /* database open in the read only mode */
    read_only_mode = TRUE;
    return(NOP);
  }
  else if (instruct==P_INST_RET_STATE) { /* return the parser state */
    return(parse_state);
  }
  else if (instruct==P_INST_RET_MODE) { /* return the read only mode flag */
    return(read_only_mode);
  }
  else {
    sprintf(localbuf, "Unrecognized instruction to parser ==> %d ",
            instruct);
    G_fatal_error(localbuf);
  }
}


/* The following function takes a null-ended input line and non-destructively
   matches it with the commands in the global array named "commands".
   This function returns the location in the array of the command that
   matched or a -1 (SYNTAX_ERR) if no command was found.
   */
int match_cmd(inp_line)
char *inp_line;
{
   int cur_pos,
       cur_cmd;

#ifdef DEBUGP
   fprintf(stderr, "\nNow entering match_cmd with inp_line='%s'\n", inp_line);
#endif

   cur_cmd = 0;
   cur_pos = strspn(inp_line, " \t"); /* span over spaces and tabs */
   if (inp_line[cur_pos]=='.') {            /* look for the dot */
      cur_pos++;
      while (cur_cmd<NUMBER_COMMANDS-1) {/* and match the command */
         if (match_word(commands[cur_cmd].name, inp_line+cur_pos,
                        commands[cur_cmd].min_chars))
            break;
         else
            cur_cmd++;
      }
   }
   else     /* the last row in the commands array is the syntax error value */
      cur_cmd = NUMBER_COMMANDS-1;

#ifdef DEBUGP
   fprintf(stderr, "\nLeaving match_cmd with cur_cmd=%d\n", cur_cmd);
#endif
   return(cur_cmd);
}



int match_word(word, string, min_chars)
char *word, *string;
int min_chars;
{
   int pos;
   int str_wrd_end;

#ifdef DEBUGP
   fprintf(stderr, "\nNow entering match_word with word='%s'", word);
   fprintf(stderr, "\nstring='%s'", string);
   fprintf(stderr, "\nmin_chars=%d\n", min_chars);
#endif

   pos = strncmp(word, string, min_chars);
   if (pos==0) {   /* if the minimum number of chars matched continue */
      pos = min_chars;
      str_wrd_end = strcspn(string, "   ");
      while (pos<str_wrd_end) {
         if (word[pos]!=string[pos]) return(FALSE);
         pos++;
      }
      return(TRUE);
   }
   else
      return(FALSE);
}


