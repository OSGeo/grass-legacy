#include "dbmi.h"

db__send_token (token)
    dbToken *token;
{
    return db__send_int (*token);
}

db__recv_token (token)
    dbToken *token;
{
    return db__recv_int (token);
}
