#include <unistd.h>
#include "lock.h"


/*!
 * \brief remove a lock
 *
 * This routine releases the
 * lock by unlinking <b>file.</b> This routine does NOT check to see that the
 * process unlocking the file is the one which created the lock. The file is
 * simply unlinked. Programs should of course unlock the lock if they created it.
 * (Note, however, that the mechanism correctly handles abandoned locks.)
 * Return codes:
 * 1 ok. lock file was removed
 * 0 ok. lock file was never there
 * -1 error. lock file remained after attempt to remove it.

 *
 *  \param file
 *  \return int
 */

int unlock_file ( char *file)
{
    if (access (file,0) != 0)
	return 0;
    remove ( file );
    if (access (file,0) != 0)
	return 1;
    return -1;
}
