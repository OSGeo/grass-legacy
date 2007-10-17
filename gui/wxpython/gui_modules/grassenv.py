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

gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","gui_modules" )
sys.path.append(gmpath)
import gcmd

env={}

class NotInGRASSSession(Exception):
    def __str__(self):
        return "You must be in running GRASS session"

class CouldNotStartMonitor(Exception):
    def __init__(self,monitor):
        self.monitor = monitor

    def __str__(self):
        return "Could not start GRASS monitor <%s>" % self.monitor

class CouldNotStopMonitor(Exception):
    def __init__(self,monitor):
        self.monitor = monitor

    def __str__(self):
        return "Could not start GRASS monitor <%s>" % self.monitor

class CouldNotExecute(Exception):
    def __init__(self,command):
        self.command = command
    def __str__(self):
        return "Could not execute GRASS command: %s" % self.command


if not os.getenv("GISBASE"):
    raise NotInGRASSSession()
else:
    env["GISBASE"] = os.getenv("GISBASE")
    env["GIS_LOCK"] = os.getenv("GIS_LOCK")
    env["GISRC"] = os.getenv("GISRC")

for key in os.environ.keys():
    if key.find("GRASS") > -1:
        env[key] = os.getenv(key)

for line in os.popen("g.gisenv").readlines():
    key,val = line.strip().split("=")
    val = val.replace("'","")
    val = val.replace(";","")
    env[key] = val

def GetGRASSVariable(var):
    """Return GRASS variable"""
    gisEnv = gcmd.Command(['g.gisenv'])

    for item in gisEnv.ReadStdOutput():
        if var in item:
            return item.split('=')[1].replace("'",'').replace(';','').strip()

    return ''
