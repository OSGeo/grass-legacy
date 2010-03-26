"""
@brief Support script for wxGUI - only for developers needs. Updates
menudata.xml file.

Parse all GRASS modules in the search path ('bin' & 'script') and
updates: - description (i.e. help)

Prints warning for missing modules.

(C) 2008-2010 by the GRASS Development Team
This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

Usage: python support/update_menudata.py [-d]

 -d - dry run (prints diff, file is not updated)

@author Martin Landa <landa.martin gmail.com>
"""

import os
import sys
import locale
import tempfile

import xml.sax
import xml.sax.handler
HandlerBase=xml.sax.handler.ContentHandler
from xml.sax import make_parser

from grass.script import core as grass

def ParseInterface(cmd):
    """!Parse interface
    
    @param cmd command to be parsed (given as list)
    """
    grass_task = menuform.grassTask()
    handler = menuform.processTask(grass_task)
    enc = locale.getdefaultlocale()[1]
    if enc and enc.lower() not in ("utf8", "utf-8"):
        xml.sax.parseString(menuform.getInterfaceDescription(cmd[0]).decode(enc).split('\n',1)[1].replace('', '<?xml version="1.0" encoding="utf-8"?>\n', 1).encode("utf-8"),
                            handler)
    else:
        xml.sax.parseString(menuform.getInterfaceDescription(cmd[0]),
                            handler)
        
    return grass_task
    
def parseModules():
    """!Parse modules' interface"""
    modules = dict()
    
    # list of modules to be ignored
    ignore =  [ 'mkftcap',
                'g.parser',
                'r.mapcalc',
                'r3.mapcalc',
                'vcolors' ]
    
    count = len(globalvar.grassCmd['all'])
    i = 0
    for module in globalvar.grassCmd['all']:
        i += 1
        if i % 10 == 0:
            grass.info('* %d/%d' % (i, count))
        if module in ignore:
            continue
        try:
            interface = ParseInterface(cmd = [module])
        except IOError, e:
            grass.error(e)
            continue
        modules[interface.name] = { 'label'   : interface.label,
                                    'desc'    : interface.description }
        
    return modules

def updateData(data, modules):
    """!Update menu data tree"""
    # list of modules to be ignored
    ignore =  [ 'v.type_wrapper.py',
                'vcolors' ]
    

    for node in data.tree.getiterator():
        if node.tag != 'menuitem':
            continue

        item = dict()
        for child in node.getchildren():
            item[child.tag] = child.text
        
        if not item.has_key('command'):
            continue

        if item['command'] in ignore:
            continue
        
        module = item['command'].split(' ')[0]
        if not modules.has_key(module):
            grass.warning("'%s' not found in modules" % item['command'])
            continue
        
        if modules[module]['label']:
            desc = modules[module]['label']
        else:
            desc = modules[module]['desc']
        node.find('help').text = desc
    
def writeData(data, file = None):
    """!Write updated menudata.xml"""
    if file is None:
        file = os.path.join('xml', 'menudata.xml')
    
    try:
        data.tree.write(file)
    except IOError:
        print >> sys.stderr, "'%s' not found. Please run the script from 'gui/wxpython'." % file
        return
    
    try:
        f = open(file, 'a')
        try:
            f.write('\n')
        finally:
            f.close()
    except IOError:
        print >> sys.stderr, "ERROR: Unable to write to menudata file."
    
def main(argv = None):
    if argv is None:
        argv = sys.argv
    
    if len(argv) > 1 and argv[1] == '-d':
        printDiff = True
    else:
        printDiff = False
    
    if len(argv) > 1 and argv[1] == '-h':
        print >> sys.stderr, __doc__
        return 1
    
    nuldev = file(os.devnull, 'w+')
    grass.info("Step 1: running make...")
    grass.call(['make'], stderr = nuldev)
    grass.info("Step 2: parsing modules...")
    modules = dict()
    modules = parseModules()
    grass.info("Step 3: reading menu data...")
    data = menudata.Data()
    grass.info("Step 4: updating menu data...")
    updateData(data, modules)
    
    if printDiff:
        tempFile = tempfile.NamedTemporaryFile()
        grass.info("Step 5: diff menu data...")
        writeData(data, tempFile.name)
        
        grass.call(['diff', '-u',
                    os.path.join('xml', 'menudata.xml'),
                    tempFile.name], stderr = nuldev)
    else:
        grass.info("Step 5: writing menu data (menudata.xml)...")
        writeData(data)
    
    return 0

if __name__ == '__main__':
    if os.getenv("GISBASE") is None:
        print >> sys.stderr, "You must be in GRASS GIS to run this program."
        sys.exit(1)
    
    sys.path.append(os.path.join(os.getenv("GISBASE"), 'etc', 'wxpython', 'gui_modules'))
    import menudata
    import menuform
    import globalvar
    
    sys.exit(main())
