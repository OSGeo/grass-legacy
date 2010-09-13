"""
@package utils.py

@brief Misc utilities for GRASS wxPython GUI

(C) 2007-2008 by the GRASS Development Team

This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

@author Martin Landa, Jachym Cepicky

@date 2007-2008
"""

import os
import sys
import platform
import locale

import globalvar
grassPath = os.path.join(globalvar.ETCDIR, "python")
sys.path.append(grassPath)
from grass.script import core as grass

import gcmd
import grassenv
try:
    import subprocess
except:
    compatPath = os.path.join(globalvar.ETCWXDIR, "compat")
    sys.path.append(compatPath)
    import subprocess
from debug import Debug

def GetTempfile(pref=None):
    """
    Creates GRASS temporary file using defined prefix.

    @todo Fix path on MS Windows/MSYS

    @param pref prefer the given path

    @return Path to file name (string) or None
    """
    import gcmd

    tempfileCmd = gcmd.Command(["g.tempfile",
                                "pid=%d" % os.getpid()])

    tempfile = tempfileCmd.ReadStdOutput()[0].strip()

    # FIXME
    # ugly hack for MSYS (MS Windows)
    if platform.system() == 'Windows':
	tempfile = tempfile.replace("/", "\\")
    try:
        path, file = os.path.split(tempfile)
        if pref:
            return os.path.join(pref, file)
	else:
	    return tempfile
    except:
        return None

def GetLayerNameFromCmd(dcmd, fullyQualified=False, param=None,
                        layerType=None):
    """Get map name from GRASS command

    @param dcmd GRASS command (given as list)
    @param fullyQualified change map name to be fully qualified
    @param force parameter otherwise 'input'/'map'
    @param update change map name in command
    @param layerType check also layer type ('raster', 'vector', '3d-raster', ...)
    
    @return map name
    @return '' if no map name found in command
    """
    mapname = ''
    
    if len(dcmd) < 1:
        return mapname
    
    if 'd.grid' == dcmd[0]:
        mapname = 'grid'
    elif 'd.geodesic' in dcmd[0]:
        mapname = 'geodesic'
    elif 'd.rhumbline' in dcmd[0]:
        mapname = 'rhumb'
    elif 'labels=' in dcmd[0]:
        mapname = dcmd[idx].split('=')[1]+' labels'
    else:
        for idx in range(len(dcmd)):
            if param and param in dcmd[idx]:
                break
            elif not param:
                if 'map=' in dcmd[idx] or \
                        'input=' in dcmd[idx] or \
                        'red=' in dcmd[idx] or \
                        'h_map=' in dcmd[idx] or \
                        'reliefmap' in dcmd[idx]:
                    break
        
        if idx < len(dcmd):
            try:
                mapname = dcmd[idx].split('=')[1]
            except IndexError:
                if idx == 1:
                    mapname = dcmd[idx]
                else:
                    return ''
            
            if fullyQualified and '@' not in mapname:
                if layerType in ('raster', 'vector', '3d-raster'):
                    try:
                        if layerType == 'raster':
                            findType = 'cell'
                        else:
                            findType = layerType
                        result = grass.find_file(mapname, element=findType)
                    except AttributeError, e: # not found
                        return ''
                    if result:
                        mapname = result['fullname']
                    else:
                        mapname += '@' + grassenv.GetGRASSVariable('MAPSET')
                else:
                    mapname += '@' + grassenv.GetGRASSVariable('MAPSET')
                if '=' in dcmd[idx]:
                    dcmd[idx] = dcmd[idx].split('=')[0] + '=' + mapname
                else:
                    dcmd[idx] = mapname
    
    return mapname

def GetValidLayerName(name):
    """Make layer name SQL compliant, based on G_str_to_sql()
    
    @todo: Better use directly GRASS Python SWIG...
    """
    retName = str(name).strip()
    
    # check if name is fully qualified
    if '@' in retName:
        retName, mapset = retName.split('@')
    else:
        mapset = None
    
    cIdx = 0
    retNameList = list(retName)
    for c in retNameList:
        if not (c >= 'A' and c <= 'Z') and \
               not (c >= 'a' and c <= 'z') and \
               not (c >= '0' and c <= '9'):
            retNameList[cIdx] = '_'
        cIdx += 1
    retName = ''.join(retNameList)
    
    if not (retName[0] >= 'A' and retName[0] <= 'Z') and \
           not (retName[0] >= 'a' and retName[0] <= 'z'):
        retName = 'x' + retName[1:]

    if mapset:
        retName = retName + '@' + mapset
        
    return retName

def ListOfCatsToRange(cats):
    """Convert list of category number to range(s)

    Used for example for d.vect cats=[range]

    @param cats category list

    @return category range string
    @return '' on error
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
        
    return catstr.strip(',')

def ListOfMapsets(get = 'ordered'):
    """!Get list of available/accessible mapsets

    @param get method ('all', 'accessible', 'ordered')
    
    @return list of mapsets
    @return None on error
    """
    mapsets = []
    
    if get == 'all' or get == 'ordered':
        ret = gcmd.RunCommand('g.mapsets',
                              read = True,
                              quiet = True,
                              flags = 'l',
                              fs = ';')
        
        if ret:
            mapsets = ret.replace('\n', '').strip().split(';')
            ListSortLower(mapsets)
        else:
            return None
        
    if get == 'accessible' or get == 'ordered':
        ret = gcmd.RunCommand('g.mapsets',
                              read = True,
                              quiet = True,
                              flags = 'p',
                              fs = ';')
        if ret:
            if get == 'accessible':
                mapsets = ret.replace('\n', '').strip().split(';')
            else:
                mapsets_accessible = ret.replace('\n', '').strip().split(';')
                for mapset in mapsets_accessible:
                    mapsets.remove(mapset)
                mapsets = mapsets_accessible + mapsets
        else:
            return None
    
    return mapsets

def ListSortLower(list):
    """Sort list items (not case-sensitive)"""
    list.sort(cmp=lambda x, y: cmp(x.lower(), y.lower()))

def GetVectorNumberOfLayers(vector):
    """Get list of vector layers"""
    layers = []
    if not vector:
        return layers
    
    ret = gcmd.RunCommand('v.db.connect',
                          flags = 'g',
                          read = True,
                          map = vector,
                          fs = ';')
        
    if not ret:
        return layers
    
    for line in ret.splitlines():
        try:
            layer = line.split(';')[0]
            if '/' in layer:
                layer = layer.split('/')[0]
            layers.append(layer)
        except IndexError:
            pass
    
    Debug.msg(3, "utils.GetVectorNumberOfLayers(): vector=%s -> %s" % \
                  (vector, ','.join(layers)))
    
    return layers

def Deg2DMS(lon, lat):
    """Convert deg value to dms string

    @param lat latitude
    @param lon longitude

    @return DMS string
    @return empty string on error
    """
    try:
        flat = float(lat)
        flon = float(lon)
    except ValueError:
        return ''

    # fix longitude
    while flon > 180.0:
        flon -= 360.0
    while flon < -180.0:
        flon += 360.0

    # hemisphere
    if flat < 0.0:
        flat = abs(flat)
        hlat = 'S'
    else:
        hlat = 'N'

    if flon < 0.0:
        hlon = 'W'
        flon = abs(flon)
    else:
        hlon = 'E'

    slat = __ll_parts(flat)
    slon = __ll_parts(flon)

    return slon + hlon + '; ' + slat + hlat

def __ll_parts(value):
    """Converts deg to d:m:s string"""
    if value == 0.0:
        return '00:00:00.0000'
    
    d = int(int(value))
    m = int((value - d) * 60)
    s = ((value - d) * 60 - m) * 60
    if m < 0:
        m = '00'
    elif m < 10:
        m = '0' + str(m)
    else:
        m = str(m)
    if s < 0:
        s = '00.0000'
    elif s < 10.0:
        s = '0%.4f' % s
    else:
        s = '%.4f' % s
    
    return str(d) + ':' + m + ':' + s

def PathJoin(*args):
    """Check path created by os.path.join"""
    path = os.path.join(*args)
    if platform.system() == 'Windows' and \
            '/' in path:
        return path[1].upper() + ':\\' + path[3:].replace('/', '\\')
    
    return path

def EncodeString(string):
    """!Return encoded string

    @param string string to be encoded

    @return encoded string
    """
    enc = locale.getdefaultlocale()[1]
    if enc:
        return string.encode(enc)
    
    return string

def UnicodeString(string):
    """!Return unicode string
    
    @param string string to be converted
    
    @return unicode string
    """
    if isinstance(string, unicode):
        return string
    
    enc = locale.getdefaultlocale()[1]
    if enc:
        return unicode(string, enc)
    
    return string
