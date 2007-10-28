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

import gcmd

def GetTempfile(pref=None):
    """
    Creates GRASS temporary file using defined prefix.

    Parameters:
        pref: prefer the given path
    Returns:
        Path to file name (string) or None
    """

    tempfileCmd = gcmd.Command(["g.tempfile",
                                "pid=%d" %
                                os.getpid()])

    tempfile = tempfileCmd.ReadStdOutput()[0].strip()

    try:
        path, file = os.path.split(tempfile)
        if pref:
            file = "%s%s" % (pref, file)
        return os.path.join(path, file)
    except:
        return Node

def GetGRASSVariable(var):
    """Return GRASS environment variable"""

    gisEnv = gcmd.Command(['g.gisenv'])

    for item in gisEnv.ReadStdOutput():
        if var in item:
            return item.split('=')[1].replace("'",'').replace(';','').strip()

    return ''
