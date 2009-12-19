"""!
@package prompt.py

@brief GRASS prompt

Classes:
 - GPrompt

@todo: reduce size of STC prompt to about 3 lines

(C) 2009 by the GRASS Development Team
This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

@author Martin Landa <landa.martin gmail.com>
@author Michael Barton <michael.barton@asu.edu>
"""

import os
import sys
import shlex

import wx
import wx.stc

from grass.script import core as grass

import globalvar
import menudata
import gcmd

class GPrompt(wx.stc.StyledTextCtrl):
    """!Styled GRASS prompt with autocomplete and calltips"""    
    def __init__(self, parent, id, size=wx.DefaultSize, margin=False, wrap=None):
        wx.stc.StyledTextCtrl.__init__(self, parent, id)
        self.parent = parent
        self.SetUndoCollection(True)        

        #
        # styles
        #                
        self.SetWrapMode(True)
        
        #
        # create command and map lists for autocompletion
        #
        self.AutoCompSetIgnoreCase(False) 
        
        self.rastlist = []
        self.vectlist = []
        self.imglist = []
        self.r3list = []
        self.dblist = []
        self.genlist = []
        self.displist = []
        
        #
        # Get available GRASS commands and parse into lists by command type for autocomplete
        #
        for item in globalvar.grassCmd['all']:
            if len(item.split('.')) > 1:
                start,end = item.split('.',1)
                if start == 'r': self.rastlist.append(end)
                elif start == 'v': self.vectlist.append(end)
                elif start == 'i': self.imglist.append(end)
                elif start == 'r3': self.r3list.append(end)
                elif start == 'db': self.dblist.append(end)
                elif start == 'g': self.genlist.append(end)
                elif start == 'd': self.displist.append(end)

        self.rastlist.sort()
        self.vectlist.sort()
        self.imglist.sort()
        self.r3list.sort()
        self.dblist.sort()
        self.genlist.sort()
        self.displist.sort()
                        
        #
        # Create lists of element types and possible arguments for autocomplete
        #
        self.datatypes = []
        self.maplists = {}
        self.maptype = ''
        self.datatypes = ['rast',
                        'rast3d',
                        'vect',
                        'oldvect',
                        'asciivect',
                        'labels',
                        'region',
                        'region3d',
                        'group',
                        '3dview']

        self.drastcmd = ['d.rast',
                        'd.rgb',
                        'd.his',
                        'd.rast.arrow',
                        'd.rast.num']
                    
        self.dvectcmd = ['d.vect',
                        'd.vect.chart'
                        'd.thematic.area',
                        'd.vect.thematic']
        
        self.rastargs = ['map',
                        'input',
                        'rast',
                        'raster',
                        'red',
                        'green',
                        'blue',
                        'h_map',
                        'i_map',
                        's_map',
                        'hue_input',
                        'intensity_input',
                        'saturation_input',
                        'red_input',
                        'green_input',
                        'blue_input']
                        
        self.__getfiles()

        #
        # command history buffer
        #
        self.cmdbuffer = []
        self.cmdindex = 0

        #
        # line margins
        #
        # TODO print number only from cmdlog
        self.SetMarginWidth(1, 0)
        self.SetMarginWidth(2, 0)
        if margin:
            self.SetMarginType(0, wx.stc.STC_MARGIN_NUMBER)
            self.SetMarginWidth(0, 30)
        else:
            self.SetMarginWidth(0, 0)

        #
        # miscellaneous
        #
        self.SetViewWhiteSpace(False)
#        self.SetTabWidth(4)
        self.SetUseTabs(False)
        self.UsePopUp(True)
        self.SetSelBackground(True, "#FFFF00")
        self.SetUseHorizontalScrollBar(True)

        #
        # bindings
        #
        self.Bind(wx.EVT_WINDOW_DESTROY, self.OnDestroy)
        self.Bind(wx.EVT_KEY_DOWN, self.OnKeyPressed)
 
    def __getfiles(self):   
        """!Get accessible files for autocomplete"""
        for item in self.datatypes:
            mlist = grass.read_command("g.mlist", "m", type=item).splitlines()
            mlist.sort()
            self.maplists[item] = mlist
            
    def OnKeyPressed(self, event):
        """!Key press capture for autocompletion, calltips, and command history"""
        
        #keycodes used: "." = 46, "=" = 61, "," = 44 
        line = ''
        entry = ''
        usage = ''
        cmdtype = ''
        cmdname = ''
        cmd = ''
                            
        # CAN CHANGE: event.ControlDown() for manual autocomplete
        
        if event.GetKeyCode() == 46 and not event.ShiftDown():
            #GRASS command autocomplete when "." is pressed after r,v,i,g,db, or d
            listcmds = []
            pos = self.GetCurrentPos()
            self.InsertText(pos,'.')
            self.CharRight()
            
            entry = self.GetTextLeft()
            if entry not in ['r.','v.','i.','g.','db.','d.']:
                return

            if entry == 'r.': listcmds = self.rastlist
            elif entry == 'v.': listcmds = self.vectlist
            elif entry == 'i.': listcmds = self.imglist
            elif entry == 'r3.': listcmds = self.r3list
            elif entry == 'db.': listcmds = self.dblist
            elif entry == 'g.': listcmds = self.genlist
            elif entry == 'd.': listcmds = self.displist

            if listcmds == []:
                return
            else:
                self.AutoCompShow(0, " ".join(listcmds))                    
            
        elif event.GetKeyCode() == wx.WXK_TAB:
            #GRASS command calltips
                        
            #Must be a command to the left somewhere
            pos = self.GetCurrentPos()
            entry = self.GetTextLeft()
            cmd = entry.split()[0].strip()
            if cmd not in globalvar.grassCmd['all']:
                return
            
            usage, description = self.GetCommandUsage(cmd)
                                        
            self.CallTipSetBackground("PALE GREEN")
            self.CallTipSetForeground("BLACK")
            self.CallTipShow(pos, usage+'\n\n'+description)
            
        elif (event.GetKeyCode() == wx.WXK_SPACE and event.ControlDown()) or \
            event.GetKeyCode() == 61 or event.GetKeyCode() == 44:
            #Autocompletion for map/data file name entry after '=', ',', or manually
            
            pos = self.GetCurrentPos()
            entry = self.GetTextLeft()
            if event.GetKeyCode() != 44:
                self.maptype = ''
            arg = ''
            cmdtype = ''
            cmdname = ''
            cmd = ''

            if entry.strip()[0:2] in ['r.','v.','i.','g.','db.','d.']:
                cmdtype =  entry.strip()[0]
                cmd = entry.split()[0].strip()
                if cmd in globalvar.grassCmd['all']:
                    cmdname = cmd.split('.')[1]
                else:
                    #No complete GRASS command found
                    cmd = ''
                    cmdname = ''
            elif entry.strip()[0:4] == 'nviz':
                cmdtype = ''
                cmdname = cmd = 'nviz'
            else:
                #No partial or complete GRASS command found
                return

            cmdargs = entry.strip('=')
            try:
                arg = cmdargs.rsplit(' ',1)[1]
            except:
                arg = ''
                
            if event.GetKeyCode() == 61:
                # autocompletion after '='
                # insert the '=' and move to after the '=', ready for a map name
                self.InsertText(pos,'=')
                self.CharRight()

                maplist = []
                self.maptype = ''

                #what kind of map/data type is desired?
                if (((cmdtype in ['r', 'i'] or cmd in self.drastcmd) and arg in self.rastargs) or
                  ((cmd=='nviz' or cmdtype=='r3') and arg in ['elevation','color']) or
                  arg in ['rast', 'raster']):
                    self.maptype = 'rast'
                elif (((cmdtype=='v' or cmd in self.dvectcmd) and arg in ['map', 'input']) or
                  (cmdtype=='r3' and arg=='input') or
                  arg in ['vect', 'vector', 'points']):
                    self.maptype = 'vect'
                elif ((cmdtype=='r3' and arg in ['map', 'input']) or
                  (cmdtype=='nviz' and arg=='volume') or arg=='rast3d'):
                    self.maptype = 'rast3d'
                elif arg=='labels':
                    self.maptype ='labels'
                elif arg=='region':
                    self.maptype ='region'
                elif arg=='region3d':
                    self.maptype ='region3d'
                elif arg=='group':
                    self.maptype ='group'
                elif arg=='3dview':
                    self.maptype ='3dview'
                    
                print 'maptype at end of = ' + str(self.maptype)

            elif event.GetKeyCode() == 44:
                # autocompletion after ','
                # if comma is pressed, use the same maptype as previous for multiple map entries
                
                # insert the comma and move to after the comma ready for a map name
                self.InsertText(pos,',')
                self.CharRight()
                
                #must apply to an entry where '=[string]' has already been entered
                if '=' not in arg:
                    return

            elif event.GetKeyCode() == wx.WXK_SPACE and event.ControlDown():
                # manual autocompletion
                # map entries without arguments (as in r.info [mapname]) use ctrl-shift
                
                maplist = []
                if cmdtype=='r' or cmdtype=='i':
                    self.maptype = 'rast'
                elif cmdtype=='v':
                    self.maptype = 'vect'
                elif cmdtype=='r3':
                    self.maptype = 'rast3d'
                    
            if self.maptype == '': 
                return
            else:
                maplist = self.maplists[self.maptype]
                self.AutoCompShow(0, " ".join(maplist))
                        
        elif event.GetKeyCode() in [wx.WXK_UP,wx.WXK_DOWN] and event.ControlDown():
            # Command history using ctrl-up and ctrl-down   
            
            if self.cmdbuffer == []: return
            txt = ''

            self.DocumentEnd()
            
            # move through command history list index values
            if event.GetKeyCode() == wx.WXK_UP:
                self.cmdindex = self.cmdindex - 1
            if event.GetKeyCode() == wx.WXK_DOWN:
                self.cmdindex = self.cmdindex + 1
            if self.cmdindex < 0:
                self.cmdindex = 0
            if self.cmdindex > len(self.cmdbuffer) - 1:
                self.cmdindex = len(self.cmdbuffer) - 1
            
            try:
                txt = self.cmdbuffer[self.cmdindex]
            except:
                pass
                
            # clear current line and insert command history    
            self.DelLineLeft()
            self.DelLineRight()
            pos = self.GetCurrentPos()            
            self.InsertText(pos,txt)
            self.LineEnd()
            
        elif event.GetKeyCode() == wx.WXK_RETURN and self.AutoCompActive() == False:
            # Run command on line when <return> is pressed    
            
            # find the command to run
            line = str(self.GetCurLine()[0]).strip()
            if len(line) == 0:
                return
            
            # parse command into list
            # TODO: shell commands should probably be passed as string           
            cmd = shlex.split(str(line))
            
            #send the command list to the processor 
            self.parent.RunCmd(cmd)
                            
            #add command to history    
            self.cmdbuffer.append(line)
            
            #keep command history to a managable size
            if len(self.cmdbuffer) > 200:
                del self.cmdbuffer[0]
            self.cmdindex = len(self.cmdbuffer)

        else:
            event.Skip()

    def GetTextLeft(self):
        """!Returns all text left of the caret"""
        entry = ''
        pos = self.GetCurrentPos()
        self.HomeExtend()
        entry = self.GetSelectedText().strip()
        self.SetCurrentPos(pos)
        
        return entry

    def GetCommandUsage(self, command):
        """!Returns command syntax by running command help"""
        usage = ''
        description = ''

        ret, out  = gcmd.RunCommand(command, 'help', getErrorMsg = True)
               
        if ret == 0:
            cmdhelp = out.splitlines()
            addline = False
            helplist = []
            description = ''
            for line in cmdhelp:
                if "Usage:" in line:
                    addline = True
                    continue
                elif "Flags:" in line:
                    addline = False
                    break
                elif addline == True:
                    line = line.strip()
                    helplist.append(line)

            for line in cmdhelp:
                if "Description:" in line:
                    addline = True
                    continue
                elif "Keywords:" in line:
                    addline = False
                    break
                elif addline == True:
                    description += (line + ' ')
                
            description = description.strip()

            for line in helplist:
                usage += line + '\n'

            return usage.strip(), description
        else:
            return ''   

    def OnDestroy(self, evt):
        """!The clipboard contents can be preserved after
        the app has exited"""
        
        wx.TheClipboard.Flush()
        evt.Skip()
    
    def OnCmdErase(self, event):
        """!Erase command prompt"""
        self.Home()
        self.DelLineRight()
        
    def OnRunCmd(self, event):
        """!Run command"""
        cmdString = event.GetString()
        
        if self.parent.GetName() != "LayerManager":
            return
        
        if cmdString[:2] == 'd.' and not self.parent.curr_page:
            self.parent.NewDisplay(show=True)
        
        cmd = shlex.split(str(cmdString))
        if len(cmd) > 1:
            self.parent.goutput.RunCmd(cmd, switchPage = True)
        else:
            self.parent.goutput.RunCmd(cmd, switchPage = False)
        
        self.OnUpdateStatusBar(None)
        
    def OnUpdateStatusBar(self, event):
        """!Update Layer Manager status bar"""
        if self.parent.GetName() != "LayerManager":
            return
        
        if event is None:
            self.parent.statusbar.SetStatusText("")
        else:
            self.parent.statusbar.SetStatusText(_("Type GRASS command and run by pressing ENTER"))
            event.Skip()