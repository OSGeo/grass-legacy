"""
@package nviz.py

@brief Nviz extension for wxGUI

This module enables to visualize data in 2.5/3D space.

Map Display supports standard 2D mode ('mapdisp' module) and 2.5/3D
mode ('nviz_mapdisp' module).

(C) 2008 by the GRASS Development Team

This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

@author Martin Landa <landa.martin gmail.com> (Google SoC 2008)
"""

errorMsg = ''

import os
import sys

import wx
import globalvar
try:
    from wx import glcanvas
    import nviz_mapdisp
    import nviz_tools
    sys.path.append(os.path.join(globalvar.ETCWXDIR, "nviz"))
    import grass6_wxnviz as wxnviz
    haveNviz = True
except ImportError, e:
    haveNviz = False
    errorMsg = _("3D view mode is not available.\n"
                 "Reason: %s\n"
                 "Note that the 3D view mode is currently not working under\nMS Windows "
                 "(hopefully this will be fixed soon). "
                 "Please keep\nan eye out for updated versions of GRASS." % e)

if haveNviz:
    GLWindow = nviz_mapdisp.GLWindow
    NvizToolWindow = nviz_tools.NvizToolWindow
else:
    GLWindow = None
    NvizToolWindow = None
