#define EXTERN extern

#include "inter.h"

transfer_data(temp_name,tape_name)  char *temp_name, *tape_name;
{

    int temp_fd, tape_fd;
    int num;
    char buf[1024];

    temp_fd = open(temp_name,4);
    if (temp_fd < 0)
    {
        fprintf(stderr,"Error in transfering from temp file <%s>\n",temp_name);
        fprintf(stderr,"Unable to open for reading\n");
        exit(2);
    }

    tape_fd = open(tape_name,2);
    if (tape_fd < 0)
    {
        fprintf(stderr,"Error in transfering to tape file <%s>\n",tape_name);
        fprintf(stderr,"Unable to open for writing\n");
        exit(2);
    }

    num = 0;
    do
    {
        num = read(temp_fd,buf,1024);
        write(tape_fd,buf,num);
    } while (num == 1024);

    close(temp_fd);
    close(tape_fd);

    return;
}
