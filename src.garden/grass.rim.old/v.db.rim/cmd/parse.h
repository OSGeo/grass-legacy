
/* constants for determining the parse_state */
#define P_NOTMADE_STATE -1
#define P_CMD_STATE 0
#define P_MAKE_STATE 1
#define P_ADD_STATE 2
#define P_DELETE_FOUND_STATE 3
#define P_QUERY_STATE 4
#define P_CHANGE_STATE 5
#define P_FIND_STATE 6
#define P_DELETE_LIST_STATE 7

/* constants that reprsent the instructions to the parser */
#define P_INST_PARSE 0
#define P_INST_DB_NA 1
#define P_INST_RONLY 2
#define P_INST_RET_STATE 3
#define P_INST_RET_MODE 4
#define P_INST_RESET 5

#define SHELL_ESCAPE_CHAR '!'
