"""
MODULE: utils.py

CLASSES:
    * GetTempfile

PURPOSE: Misc utilities for GRASS wxPython GUI

AUTHORS: The GRASS Development Team

COPYRIGHT: (C) 2007 by the GRASS Development Team
         This program is free software under the GNU General Public
         License (>=v2). Read the file COPYING that comes with GRASS
         for details.
"""
import os

def GetTempfile( pref=None):
    """
    Creates GRASS temporary file using defined prefix.

    Returns:
        Path to file name (string) or None
    """

    tempfile = os.popen("g.tempfile pid=%d" %
            os.getpid()).readlines()[0].strip()

    if not tempfile:
        return None
    else:
        path,file = os.path.split(tempfile)
        if pref:
            file = "%s%s" % (pref,file)
        return os.path.join(path,file)
