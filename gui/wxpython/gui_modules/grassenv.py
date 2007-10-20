"""
MODULE:     grassenv

PURPOSE:    GRASS environment variable management

AUTHORS:    The GRASS Development Team
            Jachym Cepicky (Mendel University of Agriculture)
            Martin Landa <landa.martin gmail.com>

COPYRIGHT:  (C) 2006-2007 by the GRASS Development Team
            This program is free software under the GNU General Public
            License (>=v2). Read the file COPYING that comes with GRASS
            for details.

"""

import os
import sys

try:
    import subprocess
except:
    CompatPath = os.path.join(os.getenv("GISBASE"), "etc", "wx", "compat")
    sys.path.append(CompatPath)
    import subprocess
    
# gmpath = os.path.join(os.getenv("GISBASE"), "etc", "wx", "gui_modules")
# sys.path.append(gmpath)
# import gcmd

def GetGRASSVariable(var):
    """Return GRASS variable or '' if variable is not defined"""
    # gisEnv = gcmd.Command(['g.gisenv'])

    gisEnv = subprocess.Popen(['g.gisenv'],
                              stdin=None,
                              stdout=subprocess.PIPE,
                              stderr=None,
                              close_fds=True)

    for item in gisEnv.stdout.readlines():
        if var in item:
            return item.split('=')[1].replace("'",'').replace(';','').strip()

    return None
