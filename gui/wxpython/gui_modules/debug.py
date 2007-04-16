"""
MODULE: debug

CLASSES:
 * Debug

PURPOSE: GRASS debugging

AUTHORS: The GRASS Development Team
         Martin Landa

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import grassenv

class Debug:
    """
    GRASS Debugging

    Usage:
         import cmd

         cmd.Command (cmd="g.gisenv set=DEBUG=3")

         import grassenv # or reload (grassenv)

         debug = Debug()
         debug.msg (3, "message level=%d" % 3)
    """
    def __init__(self):
        self.debuglevel = 0
        self._update_level()

    def _update_level(self):
        if grassenv.env.has_key ("DEBUG"):
            level = int (grassenv.env["DEBUG"])
            if self.debuglevel != level:
                self.debuglevel = level

    def msg (self, level, message):
        self._update_level()
        if self.debuglevel > 0 and level > 0 and level <= self.debuglevel:
            print "GUI D%d/%d: %s" % (level, level, message)

# Debug instance
Debug = Debug()

# testing
if __name__ == "__main__":
    import cmd
    cmd.Command (cmd="g.gisenv set=DEBUG=3")
    reload (grassenv) # reload GRASS environments !
                
    for level in range (4):
        Debug.msg (level, "message level=%d" % level)
