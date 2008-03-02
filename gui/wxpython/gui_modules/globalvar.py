"""
MODULE:    global.py

PURPOSE:   Global variables

           This module provide the space for global variables
           used in the code.

AUTHOR(S): GRASS Development Team
           Martin Landa <landa.martin gmail.com>

COPYRIGHT: (C) 2007 by the GRASS Development Team

           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import os

### recursive import problem 
# import utils
# utils.CheckForWx()
import wx
import wx.lib.flatnotebook as FN

try:
    import subprocess
except:
    compatPath = os.path.join(globalvar.ETCWXDIR, "compat")
    sys.path.append(compatPath)
    import subprocess

"""
Query layer (generated for example by selecting item in the Attribute Table Manager)
Deleted automatically on re-render action
"""
# temporal query layer (removed on re-render action)
QUERYLAYER = 'qlayer'

# path to python scripts
ETCDIR = os.path.join(os.getenv("GISBASE"), "etc")
ETCWXDIR = os.path.join(ETCDIR, "wxpython")

"""Style definition for FlatNotebook pages"""
FNPageStyle = FN.FNB_VC8 | \
    FN.FNB_BACKGROUND_GRADIENT | \
    FN.FNB_NODRAG | \
    FN.FNB_TABS_BORDER_SIMPLE 
FNPageColor = wx.Colour(125,200,175)

"""@brief File name extension binaries/scripts"""
if subprocess.mswindows:
    EXT_BIN = '.exe'
    EXT_SCT = '.bat'
else:
    EXT_BIN = ''
    EXT_SCT = ''

def __getGRASSCmds(bin=True, scripts=True):
    """
    Create list of all available GRASS commands to use when
    parsing string from the command line
    """
    gcmdlst = []
    gisbase = os.environ['GISBASE']
    if bin is True:
        gcmdlst = os.listdir(os.path.join(gisbase, 'bin'))
    if scripts is True:
        gcmdlst = gcmdlst + os.listdir(os.path.join(gisbase, 'scripts'))
    # self.gcmdlst = self.gcmdlst + os.listdir(os.path.join(gisbase,'etc','gm','script'))

    return gcmdlst

"""@brief Collected GRASS-relared binaries/scripts"""
grassCmd = {}
grassCmd['all'] = __getGRASSCmds()
grassCmd['script'] = __getGRASSCmds(bin=False)

"""@Toolbar icon size"""
toolbarSize = (24, 24)