#include "tape.h"

unmount_tape()
{
    close (tape.fd);
    tape.fd = -1;
}
