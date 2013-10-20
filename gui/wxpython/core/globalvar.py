"""!
@package core.globalvar

@brief Global variables used by wxGUI

(C) 2007-2012 by the GRASS Development Team

This program is free software under the GNU General Public License
(>=v2). Read the file COPYING that comes with GRASS for details.

@author Martin Landa <landa.martin gmail.com>
"""

import os
import sys
import locale

if not os.getenv("GISBASE"):
    sys.exit("GRASS is not running. Exiting...")

# path to python scripts
ETCDIR = os.path.join(os.getenv("GISBASE"), "etc")
ETCICONDIR = os.path.join(os.getenv("GISBASE"), "etc", "gui", "icons")
ETCWXDIR = os.path.join(ETCDIR, "wxpython")
ETCIMGDIR = os.path.join(ETCDIR, "gui", "images")
ETCSYMBOLDIR = os.path.join(ETCDIR, "gui", "images", "symbols")

from core.debug import Debug

sys.path.append(os.path.join(ETCDIR, "python"))
import grass.script as grass

def CheckWxVersion(version = [2, 8, 11, 0]):
    """!Check wx version"""
    ver = wx.version().split(' ')[0]
    if map(int, ver.split('.')) < version:
        return False
    
    return True

def CheckForWx():
    """!Try to import wx module and check its version"""
    if 'wx' in sys.modules.keys():
        return
    
    minVersion = [2, 8, 1, 1]

    try:
        try:
            import wxversion
        except ImportError, e:
            raise ImportError(e)
        # wxversion.select(str(minVersion[0]) + '.' + str(minVersion[1]))
        wxversion.ensureMinimal(str(minVersion[0]) + '.' + str(minVersion[1]))
        import wx
        version = wx.version().split(' ')[0]
        
        if map(int, version.split('.')) < minVersion:
            raise ValueError('Your wxPython version is %s.%s.%s.%s' % tuple(version.split('.')))

    except ImportError, e:
        print >> sys.stderr, 'ERROR: wxGUI requires wxPython. %s' % str(e)
        sys.exit(1)
    except (ValueError, wxversion.VersionError), e:
        print >> sys.stderr, 'ERROR: wxGUI requires wxPython >= %d.%d.%d.%d. ' % tuple(minVersion) + \
            '%s.' % (str(e))
        sys.exit(1)
    except locale.Error, e:
        print >> sys.stderr, "Unable to set locale:", e
        os.environ['LC_ALL'] = ''
    
if not os.getenv("GRASS_WXBUNDLED"):
    CheckForWx()
import wx
import wx.lib.flatnotebook as FN

"""
Query layer (generated for example by selecting item in the Attribute Table Manager)
Deleted automatically on re-render action
"""
# temporal query layer (removed on re-render action)
QUERYLAYER = 'qlayer'

"""!Style definition for FlatNotebook pages"""
FNPageStyle = FN.FNB_VC8 | \
    FN.FNB_BACKGROUND_GRADIENT | \
    FN.FNB_NODRAG | \
    FN.FNB_TABS_BORDER_SIMPLE 

FNPageDStyle = FN.FNB_FANCY_TABS | \
    FN.FNB_BOTTOM | \
    FN.FNB_NO_NAV_BUTTONS | \
    FN.FNB_NO_X_BUTTON

FNPageColor = wx.Colour(125,200,175)

"""!Dialog widget dimension"""
DIALOG_SPIN_SIZE = (150, -1)
DIALOG_COMBOBOX_SIZE = (300, -1)
DIALOG_GSELECT_SIZE = (400, -1)
DIALOG_TEXTCTRL_SIZE = (400, -1)
DIALOG_LAYER_SIZE = (100, -1)
DIALOG_COLOR_SIZE = (30, 30)

MAP_WINDOW_SIZE = (800, 600)
GM_WINDOW_SIZE = (500, 600)

if sys.platform == 'win32':
    BIN_EXT = '.exe'
    SCT_EXT = '.bat'
else:
    BIN_EXT = SCT_EXT = ''

def GetGRASSCommands():
    """!Create list of available GRASS commands to use when parsing
    string from the command line

    @return list of commands (set) and directory of scripts (collected
    by extension - MS Windows only)
    """
    gisbase = os.environ['GISBASE']
    cmd = list()
    if sys.platform == 'win32':
        scripts = { SCT_EXT : list() }
    else:
        scripts = {}
    
    # scan bin/
    if os.path.exists(os.path.join(gisbase, 'bin')):
        for fname in os.listdir(os.path.join(gisbase, 'bin')):
            if scripts: # win32
                name, ext = os.path.splitext(fname)
                if ext != '.manifest':
                    cmd.append(name)
                if ext in scripts.keys():
                    scripts[ext].append(name)
            else:
                cmd.append(fname)
    
    # scan scripts/ (not on MS Windows)
    if not scripts and os.path.exists(os.path.join(gisbase, 'scripts')):
        for fname in os.listdir(os.path.join(gisbase, 'scripts')):
            cmd.append(fname)
    
    # scan gui/scripts/
    if os.path.exists(os.path.join(gisbase, 'etc', 'gui', 'scripts')):
        os.environ["PATH"] = os.getenv("PATH") + os.pathsep + os.path.join(gisbase, 'etc', 'gui', 'scripts')
        os.environ["PATH"] = os.getenv("PATH") + os.pathsep + os.path.join(gisbase, 'etc', 'wxpython', 'scripts')
        
        pattern = "_wrapper"
        for script in os.listdir(os.path.join(gisbase, 'etc', 'gui', 'scripts')):
            if script[-len(pattern):] != pattern: # ignore wrappers
                cmd.append(script)
    
    return set(cmd), scripts

def UpdateGRASSAddOnCommands(eList = None):
    """!Update list of available GRASS AddOns commands to use when
    parsing string from the command line

    @param eList list of AddOns commands to remove
    """
    global grassCmd, grassScripts
    
    # scan addons (path)
    if not os.getenv('GRASS_ADDON_PATH'):
        return
        
    # remove commands first
    if eList:
        for ext in eList:
            if ext in grassCmd:
                grassCmd.remove(ext)
        Debug.msg(1, "Number of removed AddOn commands: %d", len(eList))

    nCmd = 0
    for path in os.getenv('GRASS_ADDON_PATH').split(os.pathsep):
        if not os.path.exists(path) or not os.path.isdir(path):
            continue
        for fname in os.listdir(path):
            if fname in ['docs', 'modules.xml']:
                continue
            if grassScripts: # win32
                name, ext = os.path.splitext(fname)
                if ext not in [BIN_EXT, SCT_EXT]:
                    continue
                if name not in grassCmd:
                    grassCmd.add(name)
                    Debug.msg(3, "AddOn commands: %s", name)
                    nCmd += 1
                if ext == SCT_EXT and \
                        ext in grassScripts.keys() and \
                        name not in grassScripts[ext]:
                    grassScripts[ext].append(name)
            else:
                if fname not in grassCmd:
                    grassCmd.add(fname)
                    Debug.msg(3, "AddOn commands: %s", fname)
                    nCmd += 1
                    
    Debug.msg(1, "Number of new AddOn commands: %d", nCmd)

def SetLanguage():
    import locale
    
    language = os.getenv('LANG')
    if not language:
        return
    
    language = language.split('.')[0] # Split off ignored .encoding part if present
    orig_language = language
    try:
        locale.setlocale(locale.LC_ALL, language)
    except locale.Error, e:
        if sys.platform != 'win32': # Don't try on Windows, it will probably not work
            # sys.stderr.write("Failed to set LC_ALL to %s (%s)\n" % (language, e))
            try:
                # Locale lang.encoding might be missing. Let's try
                # UTF-8 encoding before giving up as on Linux systems
                # lang.UTF-8 locales are more common than legacy
                # ISO-8859 ones.
                language = locale.normalize('%s.UTF-8' % (language))
                locale.setlocale(locale.LC_ALL, language)
            except locale.Error, e:
                # If we got so far, provided locale is not supported
                # on this system
                sys.stderr.write("Failed to set LC_ALL to %s (%s)\n" % (language, e))
                ### locale.getdefaultlocale() is probably related to gettext?
                # try:
                #     default_locale = locale.getdefaultlocale()
                # except:
                #     default_locale = None
                # if default_locale and default_locale[0]:
                #     language = default_locale[0]
                # else:
                language = 'C'
    
    # Set up environment for subprocesses
    for lc in ('LC_CTYPE', 'LC_MESSAGES', 'LC_TIME', 'LC_COLLATE', 'LC_MONETARY', 'LC_PAPER',
               'LC_NAME', 'LC_ADDRESS', 'LC_TELEPHONE', 'LC_MEASUREMENT', 'LC_IDENTIFICATION'):
        os.environ[lc] = language
    
    Debug.msg(1, "Language setttings: (WX) %s / (GRASS) %s", language, orig_language)
    
    # Some code in GRASS might not like other decimal separators than .
    # Other potential sources for problems are: LC_TIME LC_CTYPE
    locale.setlocale(locale.LC_NUMERIC, 'C')
    os.environ['LC_NUMERIC'] = 'C'
    if os.getenv('LC_ALL'):
        del os.environ['LC_ALL'] # Remove LC_ALL to not override LC_NUMERIC
    
    # Even if setting locale has failed, let's set LANG in a hope,
    # that UI will use it GRASS texts will be in selected language,
    # system messages (i.e. OK, Cancel etc.) - in system default
    # language
    os.environ['LANGUAGE'] = orig_language
    os.environ['LANG'] = orig_language

"""@brief Collected GRASS-relared binaries/scripts"""
grassCmd, grassScripts = GetGRASSCommands()
Debug.msg(1, "Number of GRASS commands: %d", len(grassCmd))
UpdateGRASSAddOnCommands()

"""@Toolbar icon size"""
toolbarSize = (24, 24)

"""@Is g.mlist available?"""
if 'g.mlist' in grassCmd:
    have_mlist = True
else:
    have_mlist = False

"""@Check version of wxPython, use agwStyle for 2.8.11+"""
hasAgw = CheckWxVersion()

SetLanguage()
