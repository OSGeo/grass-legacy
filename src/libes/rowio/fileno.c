#include "rowio.h"

rowio_fileno(R)
    ROWIO *R;
{
    return R->fd;
}
