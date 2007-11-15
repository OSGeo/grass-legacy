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

def GetLayerNameFromCmd(dcmd):
    """Get layer name from GRASS command"""
    mapname = ''
    for item in dcmd:
        if 'map=' in item:
            mapname = item.split('=')[1]
        elif 'red=' in item:
            mapname = item.split('=')[1]
        elif 'h_map=' in item:
            mapname = item.split('=')[1]
        elif 'reliefmap' in item:
            mapname = item.split('=')[1]
        elif 'd.grid' in item:
            mapname = 'grid'
        elif 'd.geodesic' in item:
            mapname = 'geodesic'
        elif 'd.rhumbline' in item:
            mapname = 'rhumb'
        elif 'labels=' in item:
            mapname = item.split('=')[1]+' labels'
            
        if mapname != '':
            break
        
    return mapname

def ListOfCatsToRange(cats):
    """Convert list of category number to range(s)

    Used for example for d.vect cats=[range]
    """

    catstr = ''

    try:
        cats = map(int, cats)
    except:
        return catstr

    i = 0
    while i < len(cats):
        next = 0
        j = i + 1
        while j < len(cats):
            if cats[i + next] == cats[j] - 1:
                next += 1
            else:
                break
            j += 1

        if next > 1:
            catstr += '%d-%d,' % (cats[i], cats[i + next])
            i += next + 1
        else:
            catstr += '%d,' % (cats[i])
            i += 1
        
    if catstr[-1] == ",":
        catstr = catstr[:-1]

    return catstr
