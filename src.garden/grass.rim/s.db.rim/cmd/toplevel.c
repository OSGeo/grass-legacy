#define TOPLEVEL

#include "gis.h"
#include "globals.h"
#include "parse.h"
#include "opcode.h"
#include "make.h"



/* The following declarations set up the array to convert opcodes
   returned by the parser into a function call.
   */
int nop(), syntax_err(), make_init(), make_add(), make_done(), show(),
    list(), backup(), pack(), remove(), delete_init(), delete_site(),
    delete_done(), delete_found_done(), add_init(), add_change_field(),
    add_done(), find(), find_init(), query_init(), query_line(), query_done(),
    change_init(), change_done(), tables(), misplaced_end(),
    print(), input_file(), output_file(), site_list(), help(), read_sites();

FILE *pop_input_stack();

struct funct_ptr_array {
   int pass_inp_buffer;
   int (*funct)();
} op_functions[] = {{FALSE, nop},        /* Order is very important in this */
                    {FALSE, make_init},  /* array!!!  If you add a new opcode*/
                    {TRUE, make_add},   /* assign it the next unused integer*/
                    {FALSE, make_done},  /* and put the function name at the */
                    {FALSE, show},       /* end of this array along with */
                    {FALSE, list},       /* FALSE if the function does not */
                    {TRUE, backup},     /* have any arguments or TRUE if the*/
                    {FALSE, pack},       /* function expects the inp_buffer */
                    {FALSE, remove},     /* to be passed to it. */
                    {FALSE, delete_init},
                    {TRUE, delete_site},
                    {FALSE, delete_done},
                    {FALSE, add_init},
                    {TRUE, add_change_field},
                    {FALSE, add_done},
                    {TRUE, find},
                    {TRUE, query_init},
                    {TRUE, query_line},
                    {FALSE, query_done},
                    {TRUE, change_init},
                    {TRUE, add_change_field},
                    {FALSE, change_done},
                    {FALSE, tables},
                    {FALSE, delete_found_done},
                    {TRUE, syntax_err},
                    {FALSE, misplaced_end},
                    {TRUE, input_file},
                    {TRUE, output_file},
                    {TRUE, print},
                    {TRUE, site_list},
                    {TRUE, find_init},
                    {FALSE, help},
                    {TRUE, read_sites}
                    };

#define NUMBER_OP_FUNCTS sizeof(op_functions) / sizeof(struct funct_ptr_array)



/* Here is the top-level C routine that accepts input from stdin and
   calls the parser for the opcode and then executes the returned opcode.
   */
toplevel_(numarg,quiet,for_pname,for_fname, pname_len,fname_len)
char *for_fname,*for_pname;
int *numarg,fname_len,pname_len,*quiet; /* quiet is an unused flag */
{
  int opcode, temp_int, i;
  char inp_buffer[INP_SIZ];
  char *progname;

  /* clean up the FORTRAN strings (allocate local space and null terminate) */
  if (*numarg > 0) {
  for (i=0; for_fname[i]!=' ' && i<fname_len; i++);
  fname_len = i;
  if (fname_len>7)
    G_fatal_error("Data base name too long (should be 7 characters or less).");
  strncpy(File_name, for_fname, fname_len);
  File_name[fname_len] = '\0';
  }
  for (i=0; for_pname[i]!=' ' && i<pname_len; i++);
  pname_len = i;
  if ((progname = G_malloc(pname_len+3))==NULL)
    G_fatal_error("Unable to allocate memory in top_level.");
  strncpy(progname, for_pname, pname_len);
  progname[pname_len] = '\0';

  /* initialize GRASS libs */
  G_gisinit(progname);
  G_sleep_on_error(0);
  Projection = G_projection();
  G_begin_distance_calculations();

  /* initialize the input and output files */
  Outfile = stdout;
  Infile = stdin;
  Input_files[Input_nest_depth] = Infile;

  /* initialize the field values to NULL */
  Field_num = 0;
  init_field_val();

  /* initialize the prompt */
  strcpy(Prompt, PROMPT);

  /* get a temp file name for dumping unwanted output to. */
  Tempdumpf = G_tempfile();

  fprintf(Outfile,"\ns.db.rim version 1.4 6/11/91.\n");

  /* Open the data base files through RIM */
  if (open_db(File_name,FALSE)==FALSE) {
    fprintf(Outfile,"\nThe database %s does not currently exist.",File_name);
    fprintf(Outfile,"\nYou must make (.make) it before executing any other commands.\n");
    parser(P_INST_DB_NA,"");    /* alert the parser to this condition */
  }
  else {
    get_field_info();
    init_field_val();
  }


  do {
    /* Output initial prompt if this a terminal (tty) input */
    if (FP_ISATTY(Infile)!=0)
      printf("\n%s>", Prompt);

    /* read the input one line at a time and execute the function */
    while (fgets(inp_buffer, INP_SIZ, Infile)!=NULL) {
      /* if there is a new line char at end get rid of it */
      if (inp_buffer[(temp_int = strlen(inp_buffer)-1)]=='\n')
        inp_buffer[temp_int]='\0';

      /* if input or output is not a tty then echo the input to the output */
      if (FP_ISATTY(Infile)==0 || FP_ISATTY(Outfile)==0)
        fprintf(Outfile,"%s\n", inp_buffer);

      /* get and execute the opcode from this input line */
      opcode = parser(P_INST_PARSE, inp_buffer);
      if (opcode==EXIT_OP) break;
      if ((opcode<NUMBER_OP_FUNCTS)&&(opcode>=NOP)) {
        if (op_functions[opcode].pass_inp_buffer == TRUE)
          (op_functions[opcode].funct)(inp_buffer);
        else
          (op_functions[opcode].funct)();
      }
      else {
        fprintf(Outfile,"\nWarning: The parser function issued an unrecognized opcode");
        fprintf(Outfile,"\n    Opcode ==> %d\n", opcode);
      }

      /* output prompt for next line if input is from a tty */
      if (FP_ISATTY(Infile)!=0)
        printf("%s>", Prompt);
    }                           /* end of while (gets...) */

  } while ((Infile = pop_input_stack())!=NULL);

  if (parser(P_INST_RET_STATE,"")!=P_NOTMADE_STATE)
    close_db(File_name);

  /* close any output pipe or file */
  output_file(".output");

  /* get rid of the dump temp file */
  unlink(Tempdumpf);
  printf("");

  return(0);
}

