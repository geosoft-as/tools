#---------------------------------------------------------------------------
#  (C) 2024 GeoSoft
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#
#  GnuMake crash course:
#
#  target : depends
#    rule
#
#  target  - the parameter given to make. I.e. what to build
#  depends - file or other targets target depends on
#  rule    - how to create target (note that rule is preceeded by a TAB char)
#  $(VAR)  - environment variable or variable defined above
#  $@      - Current target
#  $*      - Current target without extension
#  $<      - Current dependency
#  $(subst from,to,text)
#  $(patsubst pattern,replacement,text)
#
#
#  Environment variables that must be set:
#
#    DEV_HOME  - Pointing to development root folder
#    JAVA_HOME - Pointing to JDK root folder
#    IS_UNIX   - Set to something on Linux. Not use on Windows.
#
#---------------------------------------------------------------------------

# Replace "/cygdrive/c" with "C:" in CURDIR
CurrentDir  = $(subst /cygdrive/c,C:,$(CURDIR))

DevHomeStripped = $(subst $(DEV_HOME)/,,$(CurrentDir))
ProjectName = $(firstword $(subst /, ,$(subst $(DEV_HOME)/,,$(CurrentDir))))

# Must do this to work with source one level down ...
#ProjectName = JSONWellLogFormat/Java

TimeNow     = `date +"%s%3N"`
ThisMonth   = `date --utc +"%B %Y"`

Empty       =
Space       = $(Empty) $(Empty)

ifdef IS_UNIX
  X = :
else
  X = \;
endif



#---------------------------------------------------------------------------
#
#  Directories
#
#---------------------------------------------------------------------------

ProjectDir       = $(DEV_HOME)/$(ProjectName)
ToolsDir         = $(DEV_HOME)/tools
SourceDir        = $(ProjectDir)/src
TargetDir        = $(ProjectDir)/obj
LibDir           = $(ProjectDir)/lib
BinDir           = $(ProjectDir)/bin
DocsDir          = $(ProjectDir)/docs
DeliveryRoot     = $(ProjectDir)/deliveries
WebRoot          = $(ProjectDir)/web
WebDir           = $(WebRoot)/WEB-INF
WwwDir           = $(ProjectDir)/../geosoft.no

# Make from package level will have Source set. Make from root, not.
ifdef Source
  Package          = $(subst $(SourceDir)/,,$(CurrentDir))
  PackageList      = $(Package)
  PackageSourceDir = $(SourceDir)/$(Package)
  PackageTargetDir = $(TargetDir)/$(Package)
  JavaMainClass    = $(subst /,.,$(Package)).$(Main)
else
  PackageList      = $(Packages) $(JavaPackages)
endif

PackageListLoop  = $(patsubst %,$(SourceDir)/%/.loop,$(PackageList))
TargetDirList    = $(patsubst %,$(TargetDir)/%,$(PackageList))

JRE              = $(JAVA_HOME)/jre/lib/rt.jar
JavaSource       = $(JAVA_HOME)/src
TempFolder       = $(TEMP)


#---------------------------------------------------------------------------
#
#  JUnit
#
#---------------------------------------------------------------------------

JUnitConsoleStandalone = junit-platform-console-standalone-1.7.1.jar
JUnitLibraries = \
	apiguardian-api-1.0.0.jar \
	$(JUnitConsoleStandalone) \
	cc.jar \

UnitTestDir    = $(ProjectDir)/test
TestTargetDir  = $(ProjectDir)/objtest
TestReportsDir = $(ProjectDir)/testreports

UnitTestFiles  = `find . -type f -name '*.java' | grep 'test/'`
JUnitJarsTmp   = $(patsubst %,$(ToolsDir)/JUnit5/%,$(JUnitLibraries))
JUnitJars      = $(subst $(Space),$(X),$(JUnitJarsTmp))



#---------------------------------------------------------------------------
#
#  Classification of files
#
#---------------------------------------------------------------------------

# Source
JavaFiles           = $(filter %.java, $(Source))
OtherSourceFiles    = $(filter-out $(JavaFiles), $(Source))
SourceFiles         = $(JavaFiles:%.java=  $(PackageSourceDir)/%.java)
ManifestFile        = META-INF/MANIFEST.MF
MetaFiles           = $(filter-out $(ManifestFile), $(subst $(ProjectDir)/,,$(wildcard $(ProjectDir)/META-INF/*)))

# Target
JavaClassFiles      = $(JavaFiles:%.java=  $(PackageTargetDir)/%.class)
JavaClassFilesRel   = $(JavaFiles:%.java=  $(Package)/%.class)
OtherTargetFiles    = $(OtherSourceFiles:%=$(PackageTargetDir)/%)

ThirdPartyJarsTmp   = $(patsubst %,$(LibDir)/%,$(JavaLibraries))
ThirdPartyJars      = $(subst $(Space),$(X),$(ThirdPartyJarsTmp))

JarFile             = $(LibDir)/$(ProjectName).jar
JarFileSigned       = $(LibDir)/s$(ProjectName).jar
WindowsExeFile      = $(BinDir)/$(ProjectName).exe
WarFile             = $(BinDir)/$(ProjectName).war

ifdef Source
  JavaPackageName   = $(subst /,.,$(Package))
endif

ClassPath           = $(TargetDir)$(X)$(ThirdPartyJars)
JavaPackageNames    = $(subst /,.,$(JavaPackages))
LocalLibs           = $(subst /,,$(Packages))

VersionFileName     = version.txt
VersionFile         = $(SourceDir)/$(subst Application,$(VersionFileName),$(subst .,/,$(JavaMainClass)))
BuildTimeFile       = $(TargetDir)/buildTime.txt
Launch4jConfigFile  = $(ProjectDir)/launch4j.xml
InnoSetupConfigFile = $(ProjectDir)/setup.iss

ifeq ($(strip $(PackageList)),)
  Platform = .Net
else
  Platform = Java
endif

#---------------------------------------------------------------------------
#
#  Tools & Options
#
#---------------------------------------------------------------------------

Print                    = @echo
Copy                     = cp
MakeDirectory            = mkdir -p
Delete                   = rm -fr
Rename                   = mv -f
JavaCompiler             = "$(JAVA_HOME)/bin/javac"
JavaArchiver             = "$(JAVA_HOME)/bin/jar"
JarSigner                = "$(JAVA_HOME)/bin/jarsigner"
ExeSigner                = "C:/Program Files/Microsoft SDKs/Windows/v7.1/Bin/signtool.exe"
JavadocGenerator         = "$(JAVA_HOME)/bin/javadoc"
JavaExecute              = "$(JAVA_HOME)/bin/java"
KeyTool                  = "$(JAVA_HOME)/bin/keytool"
DoxygenGenerator         = $(DEV_HOME)/tools/Doxygen/doxygen.exe
WordCount                = wc
List                     = cat
Zip                      = zip -rq
Launch4j                 = "$(DEV_HOME)/tools/Launch4j/launch4j.exe"
InnoSetup                = "$(DEV_HOME)/tools/InnoSetup5/ISCC.exe"
JavaPackager             = "$(JAVA_HOME)/bin/javapackager"
IzPack                   = "C:/Program Files/IzPack/bin/compile"


KeyStore                 = $(ProjectDir)/geosoft.jks
KeyStorePassword         = NextWell
KeyStoreName             = GeoSoft
KeyStoreOrganizationUnit = GeoSoft
KeyStoreOrganization     = GeoSoft
KeyStoreLocality         = Stavanger
KeyStoreState            = Norway
KeyStoreCountry          = NO
KeyAlias                 = GeoSoftTrust
KeyAliasPassword         = NextWell
CertificateValidity      = 10000

MakeOptions              = -k -s -j 1

JavaCompilerOptions    = -classpath $(ClassPath) \
			 -sourcepath $(SourceDir) \
			 -deprecation \
			 -Xlint:all,-serial,-path \
			 -Xdiags:verbose \
	                 -g

JavaRunOptions         = -classpath $(ClassPath) \
	                 -enableassertions \
		         -Xms640m \
		         -Xmx8g \
			 -Djava.util.Arrays.useLegacyMergeSort=true \
			 -Djava.net.useSystemProxies=true \

JavadocOptions         = -d $(DocsDir) \
			 -J-Xmx1g \
			 -sourcepath $(SourceDir)$(X)'$(JavaSource)' \
			 -classpath $(ClassPath) \
			 -author \
			 -public \
			 -use \
			 -splitIndex \
			 -version \
			 -linkoffline https://docs.oracle.com/javase/8/docs/api/ https://docs.oracle.com/javase/8/docs/api/ \
			 -windowtitle "$(ProjectDisplayName)" \
			 -doctitle "$(ProjectDisplayName) - $(ThisMonth)" \
			 -header "$(ProjectDisplayName)" \
			 -bottom '<font size="-1">Copyright &copy; 2024 GeoSoft - <a href="https://geosoft.no">https://geosoft.no<a></font>' \
			 -overview $(ProjectDir)/overview.html

JSDocOptions           = --destination $(DocsDir) \
	                 --recurse \

GoogleAnalytics = \
	<script type="text/javascript"> \
	  var _gaq = _gaq || []; \
	  _gaq.push(['_setAccount', 'UA-107827085-1']); \
	  _gaq.push(['_trackPageview']); \
\
	  (function() { \
	    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true; \
	    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js'; \
	    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s); \
	  })(); \
	</script>

BrandingJavadoc = \
	<div class="aboutLanguage"> \
          <a href="https://geosoft.no" target="_parent"> \
            <img vspace="12" src="https://geosoft.no/images/profile/GeoSoft_logo_white_104x16.png"/> \
          </a> \
        </div> \

BrandingDoxygen = \
        <table cellspacing="0" cellpadding="0" width="100%"> \
          <tr style="height:44px;background:linear-gradient(\#59637e, \#19233e);color:\#ffffff;font-family:Verdana;font-size:18px;font-weight:bold;"> \
            <td style="padding-left:20px"> \
              $(ProjectDisplayName) \
            <\/td> \
            <td> \
              <a href="http:\/\/geosoft.no"> \
                <img align="right" hspace="40" vspace="12" src="https:\/\/geosoft.no\/images\/profile\/GeoSoft_white_transparent_103x16.png"\/> \
              <\/a> \
           <\/td> \
          <\/tr> \
        <\/table>

# Count number of LOC
#CountLines = $(ToolsDir)/cloc/cloc-1.64.exe
CountLines = $(ToolsDir)/cloc/loc.exe

JavaMemoryProfiler = $(JAVA_HOME)/bin/jmap
JavaMemoryServer   = $(JAVA_HOME)/bin/jhat

# Web server
WebServerHome      = "C:/Program Files/Apache Software Foundation/Tomcat 9.0"

ProductId = `md5sum $(BuildTimeFile) | cut -d ' ' -f 1`

#---------------------------------------------------------------------------
#
# Rules
#
#---------------------------------------------------------------------------

# make
default : build

%.loop :
	@$(MAKE) $(MakeOptions) -C $(subst .loop,,$@) _$(MAKECMDGOALS)all

# Create target directory
$(PackageTargetDir) :
	$(MakeDirectory) $@

# .java -> .class
$(PackageTargetDir)/%.class : $(PackageSourceDir)/%.java
	$(Print) $@
	$(JavaCompiler) $(JavaCompilerOptions) -d $(TargetDir) $<

%.class : $(PackageSourceDir)/%.java
	$(MAKE) $(MakeOptions) $(PackageTargetDir)/$@

buildtime :
	$(Delete) $(BuildTimeFile)
	$(Print) $(TimeNow) > $(BuildTimeFile)

# Create lib directory
$(LibDir) :
	$(MakeDirectory) $@

# Create jar file
jar : buildtime $(LibDir)
	$(Print) $(JarFile)
	$(JavaArchiver) cmf $(ManifestFile) $(JarFile) -C $(TargetDir) .
	$(JavaArchiver) uf $(JarFile) -C $(ProjectDir) $(MetaFiles)

# Create war file
war :
	$(Print) $(WarFile)
	$(MakeDirectory) $(BinDir)
	$(MakeDirectory) $(WebDir)/classes
	$(MakeDirectory) $(WebDir)/lib
	$(Delete) $(WarFile)
	$(Delete) $(WebDir)/classes/*
	$(Delete) $(WebDir)/lib/*
	$(Copy) -fr $(TargetDir)/* $(WebDir)/classes/
	$(Copy) -fr $(LibDir)/* $(WebDir)/lib/
	cd $(WebRoot);$(JavaArchiver) cmf $(ProjectDir)/$(ManifestFile) $(WarFile) *
	$(JavaArchiver) uf $(WarFile) -C $(ProjectDir) $(MetaFiles)

# make doc
doc :
ifeq "$(Platform)" ".Net"
	$(DoxygenGenerator)
else
	$(JavadocGenerator) $(JavadocOptions) $(JavadocPackages)
endif
	$(Print) Done documentation.

jsdoc :
	jsdoc $(JSDocOptions) *.js

minify :
	rollup LogViewer.js --compact --file LogViewer.bundle.js --format es
	uglifyjs LogViewer.bundle.js -c -m --module -o LogViewer.min.js

# make brand
brand :
ifeq "$(Platform)" ".Net"
	find $(DocsDir) -name "*.html" | xargs sed -i '/<table cellspacing/,/table>/ { 35 { s|.*|$(BrandingDoxygen)|; b; }; d; };'
else
	sed -i "s/background-color:#4D7A97;/background:linear-gradient(#59637e, #19233e);/g" $(DocsDir)/stylesheet.css
	sed -i 's|<p>See:|<center><img vspace="20" src="$(JavadocImage)"></center><p>See:|g' $(DocsDir)/overview-summary.html
	find $(DocsDir) -name "*.html" | xargs sed -i 's#<div class="aboutLanguage">.*</div>#$(BrandingJavadoc)#g'
endif
	$(Print) Done branding.

# make analytics
analytics :
	find $(DocsDir) -name "*.html" | xargs sed -i "s#</head>#$(GoogleAnalytics) </head>#g"
	$(Print) Done analytics.

# Anything else is just copied from source to target
ifneq ($(PackageTargetDir),)
$(PackageTargetDir)/% : $(PackageSourceDir)/%
	$(Print) $@
	$(Copy) $< $@
endif

#
# Default target
#
build : $(PackageListLoop)
ifeq "$(Platform)" ".Net"
	dotnet build --configuration Release
endif
	$(Print) Done build.

_all _buildall : $(PackageTargetDir) $(JavaClassFiles) $(OtherTargetFiles)

# make clean
clean : $(PackageListLoop)
	@$(Delete) $(JarFile)
	@$(Delete) -fr $(TestTargetDir)
	$(Print) Done clean.

_cleanall :
	$(Delete) -fr $(PackageTargetDir)

signedjar : $(JarFile)
	$(JarSigner) \
	-keystore $(KeyStore) \
	-storepass $(KeyStorePassword)\
	-keypass $(KeyAliasPassword)\
	-signedjar $(JarFileSigned) \
	-verbose \
	$(JarFile) $(KeyAlias)



#-------------------------------------------------------------------------
#
# Windows .exe
#
#-------------------------------------------------------------------------

$(WindowsExeFile) : $(JarFile) $(Launch4jConfigFile)
	$(Launch4j) $(Launch4jConfigFile)

exe : $(WindowsExeFile)

signexe : $(WindowsExeFile)
	$(ExeSigner) sign /v /f $(KeyStore) /p $(KeyAliasPassword) $(SetupFile)

_runjava :
ifeq "$(Platform)" "Java"
	$(JavaExecute) $(JavaRunOptions) $(JavaMainClass) $(RunParameters)
else
	dotnet Test/bin/Release/netcoreapp2.0/Test.dll
endif

# make run
run : _runjava

# make runjar
runjar : $(JarFile)
	$(JavaExecute) -jar $(JarFile)

#-------------------------------------------------------------------------
#
# Cross-platform deployment (experimental)
#
#-------------------------------------------------------------------------

pack :
	$(IzPack) izpack.xml -b . -o install.jar -k standard


#-------------------------------------------------------------------------
#
# Delivery
#
#-------------------------------------------------------------------------

Version   = `$(List) $(VersionFile)`
SetupFile = $(DeliveryRoot)/$(ProjectName)-$(Version)-setup.exe

setup : $(JarFile) $(WindowsExeFile)
	$(InnoSetup) $(InnoSetupConfigFile)

install :
	$(SetupFile)


#-------------------------------------------------------------------------
#
# Keystore management
#
#-------------------------------------------------------------------------

keyalias :
	$(KeyTool) -genkeypair \
		-validity $(CertificateValidity) \
                -dname "CN=$(KeyStoreName), \
			OU=$(KeyStoreOrganizationUnit), \
			O=$(KeyStoreOrganization), \
			L=$(KeyStoreLocality), \
			S=$(KeyStoreState), \
			C=$(KeyStoreCountry)" \
		-keystore $(KeyStore) \
		-storepass $(KeyStorePassword)\
		-keypass $(KeyAliasPassword)\
		-alias $(KeyAlias) \
		-keyalg RSA \
		-keysize 2048 \
		-storetype pkcs12 \
		-v

keyaliasdelete :
	$(KeyTool) -delete \
		-storepass $(KeyStorePassword) \
		-alias $(KeyAlias) \
		-keystore $(KeyStore) \
		-v

keystorelist :
	$(KeyTool) -v -list \
		-keystore $(KeyStore) \
		-storepass $(KeyStorePassword) \
		-v

obfuscate:
	$(Rename) $(JarFile) $(JarFile).original.jar
	$(JavaExecute) -jar $(ToolsDir)/ProGuard/proguard.jar \
                -injars $(JarFile).original.jar \
	        -outjars $(JarFile) \
		-forceprocessing \
		-flattenpackagehierarchy \
		-repackageclasses \
		-optimizationpasses 10 \
		-libraryjars '$(ProGuardClassPath)' \
		-verbose \
		@$(ProjectName).pro
	$(Delete) $(JarFile).original.jar

loc:
	$(CountLines) $(SourceDir)

# Java version
javaversion :
	$(JavaExecute) -version

# Make version
version :
	$(MAKE) --version

#-------------------------------------------------------------------------
#
# JUnit
#
#-------------------------------------------------------------------------

# Create the test target directory (./objtest/)
$(TestTargetDir) :
	$(MakeDirectory) $@

# Create the test reports directory
$(TestReportsDir) :
	$(MakeDirectory) $@

cleantests :
	@$(Delete) -fr $(TestTargetDir)
	@$(Delete) -fr $(TestReportsDir)
	$(Print) Done clean.

# Compile all test classes
tests : $(TestTargetDir)
	$(JavaCompiler) -Xlint:unchecked $(UnitTestFiles) \
		-cp $(ClassPath)$(X)$(JUnitJars) \
		-d $(TestTargetDir)

# Run unit tests in console
runtests :
	$(JavaExecute) -enableassertions -jar $(ToolsDir)/JUnit5/$(JUnitConsoleStandalone) \
		--class-path $(ClassPath)$(X)$(JUnitJars) \
		--class-path $(TestTargetDir) \
		--scan-class-path \
		--reports-dir=$(TestReportsDir)

# If the first argument is "runtest" use the rest as arguments for JUnit
ifeq (runtest, $(firstword $(MAKECMDGOALS)))
  testarguments := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(testarguments):;@:)
endif

#
# make runtest <(part of) class name>
#
# But it doesn't work:
# TODO: GET THE --select-class (or even better: --include-classname) TO WORK!
# Think it works on Linux
#
runtest :
	$(JavaExecute) -enableassertions -jar $(ToolsDir)/JUnit5/$(JUnitConsoleStandalone) \
		--class-path $(ClassPath)$(X)$(JUnitJars) \
		--class-path $(TestTargetDir) \
		--scan-class-path \
		--include-classname '.*$(testarguments)*'

# If the first argument is "profile" the second argument should be the pid
ifeq (profile, $(firstword $(MAKECMDGOALS)))
  pid := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(pid):;@:)
endif

profile :
	$(JavaMemoryProfiler) -dump:format=b,file=$(TempFolder)/heap.bin $(pid)
	$(JavaMemoryServer) $(TempFolder)/heap.bin
	open http://localhost:7000

memory :
	$(JavaMemoryServer) $(TempFolder)/heap.bin
	open http://localhost:7000

license :
	cp -i $(WwwDir)/LogStudio/Licenses/Template.license $(WwwDir)/LogStudio/Licenses/$(ProductId).license
	$(Print) Created $(WwwDir)/LogStudio/Licenses/$(ProductId).license

productid :
	$(Print) $(ProductId)

xxx :
	$(Print) $(JarFile)
