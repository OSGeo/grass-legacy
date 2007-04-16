test_config = """
<qgisgrass name="Test config">
<modules>
  <section label="First Section">
    <grass name="g.gisenv"/>
    <grass name="d.vect"/>
  </section>
  <section label="Second Section">
    <grass name="r.to.vect.line"/>
    <grass name="r.to.vect.area"/>
  </section>
  <grass name="v.buffer"/>
</modules>
</qgisgrass>
"""
test_modules = {"g.gisenv":"""
<qgisgrassmodule label="GRASS enviroment variables" module="g.gisenv">
</qgisgrassmodule>
""","r.to.vect.area":"""
<qgisgrassmodule label="Convert a raster to vector areas" module="r.to.vect">
	<option key="input" />
	<option key="output" />
	<option key="feature" answer="area" hidden="yes" />
	<flag key="s" answer="on" hidden="yes" />
</qgisgrassmodule>
""","r.to.vect.line":"""
<qgisgrassmodule label="Convert a raster to vector areas" module="r.to.vect">
	<option key="input" />
	<option key="output" />
	<option key="feature" answer="line" hidden="yes" />
	<flag key="s" answer="on" hidden="yes" />
</qgisgrassmodule>
"""}
import xml.dom.minidom
import menuform

class handleQgisGrass:
    
    def __init__(self, qgisgrass):
        modules = qgisgrass.childNodes[0].getElementsByTagName('modules')[0]
        for child in modules.childNodes:
            if child.localName == 'grass':
                self.handleGrass( child )
            elif child.localName == 'section':
                self.handleSection( child )

    def handleSection( self,section ):
        for child in section.getElementsByTagName('grass'):
            self.handleGrass( child )

    def handleGrass( self, grass ):
        raise NotImplementedError

class printQgisGrass( handleQgisGrass ):
    def __init__(self, qgisgrass):
        print "in qgisgrass"
        handleQgisGrass.__init__( self, qgisgrass )

    def handleSection( self,section ):
        print "Section:",section.getAttribute('label')
        handleQgisGrass.handleSection( self, section )

    def handleGrass( self, grass ):
        print "Command:",grass.getAttribute('name')


class wxQgisGrass( handleQgisGrass ):
    pass


class handleQgisGrassModule:

    def __init__( self, qgisgrassmodule):
        qgisgrassm = qgisgrassmodule.getElementsByTagName( "qgisgrassmodule" )[0]
        self.handleAttributes( qgisgrassm )
        for inner in qgisgrassm.childNodes:
            it = inner.localName
            if it == 'option':
                self.handleOption( inner )
            elif it == 'flag':
                self.handleFlag( inner )

    def handleAttributes( self, node ):
        for (l,a) in node.attributes.items():
            self.handleAttribute( l, a, node )

    def handleAttribute( self, label, value, parent ):
        raise NotImplementedError
        
    def handleOption( self, option ):
        self.handleAttributes( option )

    def handleFlag( self, flag ):
        self.handleAttributes( flag )

class printQgisGrassModule( handleQgisGrassModule ):

    def __init__( self, qgisgrassmodule):
        print "in qgisgrassmodule"
        handleQgisGrassModule.__init__( self, qgisgrassmodule )

    def handleOption( self, opt ):
        print "Option"
        handleQgisGrassModule.handleOption( self, opt )
        print

    def handleFlag( self, flag ):
        print "Flag"
        handleQgisGrassModule.handleFlag( self, flag )

    def handleAttribute( self, label, value, option ):
        print "%s:%s" % (label, value)

class wxQgisGrassModule( handleQgisGrassModule ):
    def __init__(self, qgisgrassmodule, label='' ):
        """qgisGrassModule is a string containing the .qgm xml file"""
        self.task = None
        self.label = label
        self.description = ''
        handleQgisGrassModule.__init__( self, qgisgrassmodule )
        self.task.description = self.description
        menuform.GrassGUIApp( self.task ).MainLoop()

    def handleOption( self, opt, getit = menuform.grassTask.get_param ):
        a = dict(opt.attributes.items())
        p = getit( self.task, a['key'] )
        p['hidden'] = 'no' # unhide params
        p['guisection'] = _( 'Main' ) # this should be the only tab present in the end
        if a.get('hidden','no') == 'yes': p['hidden'] = 'yes' #except when explicitly hidden
        if a.has_key( 'answer' ): p['value'] = a.get('answer')

    def handleFlag( self, flag ):
        self.handleOption( flag, getit = menuform.grassTask.get_flag )

    def handleAttribute( self, label, value, option ):
        if option.localName == 'qgisgrassmodule':
            if label=='module':
                self.task = menuform.grassTask( grassModule = value )
                for pf in self.task.params + self.task.flags:
                    pf['hidden'] = 'yes'
            if label=='label':
                self.description = value

from sys import argv

if __name__ ==  '__main__':
    if len( argv ) != 2:
        print "Usage: %s <toolbox command>" % sys.argv[0]
    else:
        the_module =  argv[1]
        if the_module != 'test':
            qgm = open( the_module ).read()
            x = wxQgisGrassModule( xml.dom.minidom.parseString( qgm ), label = the_module )
        else:
            # self test
            config = xml.dom.minidom.parseString( test_config )
            printQgisGrass( config )
            print
            for m in test_modules.keys():
                print m
                module = xml.dom.minidom.parseString( test_modules[m] )
                printQgisGrassModule( module )
            print "----------------"
            m = "r.to.vect.area"
            x = wxQgisGrassModule( xml.dom.minidom.parseString( test_modules[ m ] ), label = m )
        
