#include "rowio.h"

int rowio_fileno(ROWIO *R)
{
    return R->fd;
}
