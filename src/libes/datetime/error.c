#include <string.h>
static int err_code = 0;
static char err_msg[1024];

int datetime_error (int code, char *msg)
{
    err_code = code;
    *err_msg = 0;
    if (code != 0 && msg)
	strcpy (err_msg, msg); /* hope err_msg is big enough */
    
    return code;
}

int datetime_error_code (void)
{
    return err_code;
}

char *datetime_error_msg (void)
{
    return err_msg;
}

void datetime_clear_error (void)
{
    err_code = 0;
    *err_msg = 0;
}
