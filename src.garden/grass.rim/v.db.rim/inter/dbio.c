#define DBIO
#include <stdio.h>
#include "globals.h"
#include "parse.h"
#include "rim.h"
#include "gis.h"

char *strchr();

/* This file contains a number of functions that handle the interaction
   with the RIM database.  These include open_db and close_db which
   open and close a database for use through this program.
   */


int open_db(name,quiet)
char *name;
int quiet;
{
   int status;
   char cmdbuf[BUF_SIZ];

#ifdef DEBUGIO
   fprintf(stderr, "Now entering open_db with name='%s'.\n", name);
#endif


#ifdef DEBUGIO
   fprintf(stderr, "In open_db with status='%d'.\n", status);
#endif
   if ((status=open_rim_db(name))!=0) {
       if (status==13) {
          if (quiet==FALSE) {
              fprintf(Outfile,"\nWarning: The database was opened in read only mode.");
              fprintf(Outfile,"\nYou will be unable to change or add data.\n");
          }
          status = 0;
          parser(P_INST_RONLY,""); /*alert parser that database is read only*/
       }
       else if (status==999) /* unable to open the files */
           return(FALSE);
       else
           rim_error(status);
   }

   Number_of_records = db_size(name);
        if (Record_list == NULL)
          Record_list = (struct query_record *)
                  G_malloc((Number_of_records+EXTRA_SITES) * SIZEOF_QUERY_RECORD);
        else
           Record_list = (struct query_record *) G_realloc(Record_list,
                        (Number_of_records+EXTRA_SITES) * SIZEOF_QUERY_RECORD);
   if (quiet==FALSE)
      fprintf(Outfile, "The database %s has %d records.\n", name,
               Number_of_records);


#ifdef DEBUGIO
   fprintf(stderr,"\nNow leaving open_db with status=%d.\n", status);
#endif

   return(TRUE);
}



close_db(name)
   char *name;
{
   int status;

#ifdef DEBUGIO
   fprintf(stderr, "Now entering close_db.\n");
#endif

   /* close the data base and unlink the temporary tables file. */
   crim_w_err(FIELD_TABLE,"close");
   if ((status = open_rim_db(name))!=0) rim_error(status);
   crim_w_err(FIELD_TABLE,"close");
   unlink(Tableinfof);
   return(1);
}





/* This routine searches for a substring in a string and returns the
   position of the beginning of the substring in the string or a -1
   if the substring was not found. */
char *find_string(substr, string)
   char *substr, *string;
{
   char *pos;
   int sublength;

#ifdef DEBUGIO
   fprintf(stderr, "Entering find_string() with substr='%s', and string='%s'\n",
            substr, string);
#endif

   pos = string;
   sublength = strlen(substr);

   while (pos!=NULL) {
      if ((pos = strchr(pos, substr[0]))==NULL)
         return(NULL);

      if (strncmp(substr, pos, sublength)==0)
         return(pos);
      pos++;
   }
}




/* This routine popens a RIM process to send commands for table information
   A temporary file that holds all the table information is created and can
   be used by the tables() command. */
get_tableinfo(db_name)
   char *db_name;
{
  char buffer[BUF_SIZ];
  FILE *rim_proc;

        /* get tempfile only once */
        if (Tableinfof != NULL)
                return;

        Tableinfof = G_tempfile();

   sprintf(buffer, "%s >%s", RIM_COMMAND_STR, Tempdumpf);
   if ((rim_proc = popen(buffer, "w"))==NULL)
      G_fatal_error("Unable to 'popen' the RIM process in get_tableinfo().");

   fprintf(rim_proc, "open '%s/%s'\n", RIM_db_path, db_name);
   fprintf(rim_proc, "output '%s'\n", Tableinfof);
   fprintf(rim_proc, "list *\n");
   fprintf(rim_proc, "output\n");
   fprintf(rim_proc, "exit\n");

   pclose(rim_proc);

}



/* This routine returns the number of sites in the database. */
/* Do a select to force TUPLER common block to be updated */
int db_size(db_name)
   char *db_name;
{
int number;
        crim_w_err(DATA_TABLE,"select from data");
        number = *(&tupler_ +  ((3*4)+4-1)  );
        return(number);
}




/* this routine passes 'command' onto RIM and returns the error status */
int crim(table, command)
int table;
char *command;
{

#ifdef DEBUGIO
   fprintf(stderr, "Now entering crim with table=%d and command='%s'.\n",
            table, command);
#endif

   rim_(&table, command, strlen(command));
   return(rimcom_);
}

/* this routine calls crim and if there is an error reports it using rim_error*/
crim_w_err(table, command)
   int table;
   char *command;
{
   int err_num;

   if ((err_num = crim(table, command))!=0)
      rim_error(err_num);
}


/* this routine passes the data movement 'command' onto RIM causing 'row'
   to be sent or recieved and returns the error status */
int crimdm(table, command, row)
int table;
char *command;
int *row;
{

#ifdef DEBUGIO
   fprintf(stderr, "Now entering crimdm with table=%d and command='%s'.\n",
            table, command);
#endif

   rimdm_(&table, command, row, strlen(command));
   return(rimcom_);
}

/* this routine calls crimdm, if there is an error reports it using rim_error*/
crimdm_w_err(table, command, row)
   int table;
   char *command;
   int *row;
{
   int err_num;

   if ((err_num = crimdm(table, command, row))!=0)
      rim_error(err_num);
}



/* Here is an array of all the possible errors that RIM can return */
struct err_array {
   int error_num;
   char message[60];
} error_msgs[] = {-1, "No more data available for retrieval.",
                  0, "OK - operation successful.",
                  4, "Unimplemented function in programmer interface.",
                  10, "Files do not contain a RIM database.",
                  12, "Files incompetely updated.",
                  13, "Database is attached in read only mode.",
                  14, "Database is being updated.",
                  16, "Database has not been opened.",
                  20, "Undefined table.",
                  30, "Undefined column.",
                  40, "Where clause too complex.",
                  42, "Unrecognized comparison operator.",
                  43, "'like' only available for text columns.",
                  45, "Unrecognized logical operator.",
                  46, "Compared columns must be same length/type.",
                  47, "Lists are valid only for eq and ne.",
                  50, "'select' not called.",
                  60, "'get' not called.",
                  70, "Multiple table index out of range.",
                  80, "Variable length columns cannot be sorted.",
                  81, "The number of sorted columns is too large.",
                  89, "Sort system error.",
                  90, "Unauthorized table access.",
                  91, "Table already exists.",
                  92, "Bad column type.",
                  93, "Bad column length.",
                  94, "Too many or too few columns.",
                  95, "Row too big to define.",
                  100, "Illegal variable length row definition (load/put).",
                  1001, "Buffer size problem -- BLKCHG.BLKDEF",
                  1002, "Undefined block -- BLKLOC",
                  1003, "Cannot find a larger b-tree value -- BTADD.PUTDAT",
                  1004, "Cannot fing b-tree block -- BTPUT",
                  3000, "Sort error -- no buffer available"
                  };
#define NUMBER_ERROR_MSGS sizeof(error_msgs) / sizeof(struct err_array)


rim_error(status)
int status;
{
   char temp_buf[100];
   int pos;

#ifdef DEBUGIO
   fprintf(stderr, "Now entering rim_error with status=%d.\n",
            status);
#endif

   if (status>=2100 && status<2200)
      sprintf(temp_buf, "RIM ERROR #%d: Random file error %d on file1.",
               status, status-2100);
   else if (status>=2200 && status<2300)
      sprintf(temp_buf, "RIM ERROR #%d: Random file error %d on file2.",
               status, status-2200);
   else if (status>=2300 && status<2400)
      sprintf(temp_buf, "RIM ERROR #%d: Random file error %d on file3.",
               status, status-2300);
   else if (status>=3100 && status<3200)
      sprintf(temp_buf, "RIM ERROR #%d: Sort error -- %d on file open.",
               status, status-3100);
   else {
      pos = 0;
      while (pos<NUMBER_ERROR_MSGS) {
         if (status == error_msgs[pos].error_num) {
            sprintf(temp_buf, "RIM ERROR #%d: %s", status,
                     error_msgs[pos].message);
            break;
         }
         pos++;
      }
      if (pos>=NUMBER_ERROR_MSGS)
         sprintf(temp_buf, "RIM ERROR #%d: Unlisted RIM error code.", status);
   }

   G_fatal_error(temp_buf);

}



