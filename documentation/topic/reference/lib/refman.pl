#!/usr/local/bin/perl -w

use XML::Parser;
use strict;

no strict qw(refs);

my @stack;

my $library;
my $library_file;

my $module;
my $module_file;

my $entry;
my $entry_file;

my $modifiers;
my $superclasses;

my $year = (localtime time)[5] + 1900;

while(my $filename = shift) {
    my $parser
	= new XML::Parser(Handlers => { Start => \&do_start,
					End => \&do_end,
					Char => \&do_char });
    $parser->parsefile($filename);
}

exit 0;

########################################################################

sub do_start {
    my ($expat, $element, @attributes) = @_;

    eval {
	my $function = "do_start_$element";
	$function =~ s/-/_/g;
	&$function(@attributes);
    };

    push @stack, $element;
}

sub do_end {
    my ($expat, $element) = @_;

    die unless $element eq $stack[-1];
    pop @stack;

    eval {
	my $function = "do_end_$element";
	$function =~ s/-/_/g;
	&$function;
    };
}

sub do_char {
    my ($expat, $string) = @_;

    eval {
	my $function = "do_char_$stack[-1]";
	$function =~ s/-/_/g;
	&$function($string);
    };
}

sub do_char_name {
    my ($string) = @_;

    eval {
	my $function = "do_char_$stack[-2]_name";
	$function =~ s/-/_/g;
	&$function($string);
    };
}

########################################################################

sub do_char_library_name {
    my ($string) = @_;

    $library = $string;

    $library_file = "$library.xml";
    if(-f $library_file) {
	$library_file .= ".new";
    }

    open(LIBRARY, '>', $library_file) || die "Can't open $library_file: $!";
    
    print LIBRARY << "EOF";
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE dylanLibrary PUBLIC "-//Gwydion//DTD DITA Dylan API Library//EN" "../../dtd/dylanLibrary.dtd" []>
<dylanLibrary id="lib-$library">
  <apiName>$library</apiName>
  <shortdesc></shortdesc>

  <prolog>
    <author></author>
    <copyright>
      <copyryear year="2005"/>
      <copyrholder>Gwydion Dylan Maintainers</copyrholder>
    </copyright>
  </prolog>

  <dylanLibraryDetail>
    <apiDesc>
      <p></p>
    </apiDesc>
  </dylanLibraryDetail>

  <related-links>
EOF

    mkdir $library unless -d $library;
}

sub do_end_library {
    print LIBRARY << "EOF";
  </related-links>
</dylanLibrary>
EOF

    close(LIBRARY);
    if($library_file ne "$library.xml") {
	system("cmp $library.xml $library_file >/dev/null") || die;
	if ($? == 0) {
	    remove $library_file;
	}
    }

    undef $library;
    undef $library_file;
}

sub do_char_module_name {
    my ($name) = @_;

    $module = $name;

    $module_file = "$library/$module.xml";
    if(-f $module_file) {
	$module_file .= ".new";
    }

    print LIBRARY << "EOF";
    <link href="$library/$module.xml">
      <linktext><apipackage>$module</apipackage></linktext>
    </link>
EOF

    open(MODULE, '>', $module_file) || die "Can't open $module_file: $!";
    
    print MODULE << "EOF";
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE dylanModule PUBLIC "-//Gwydion//DTD DITA Dylan API Module//EN" "../../../dtd/dylanModule.dtd" []>
<dylanModule id="lib-$library-$module">
  <apiName>$module</apiName>
  <shortdesc></shortdesc>

  <prolog>
    <author></author>
    <copyright>
      <copyryear year="2005"/>
      <copyrholder>Gwydion Dylan Maintainers</copyrholder>
    </copyright>
  </prolog>

  <dylanModuleDetail>
    <apiDesc>
      <p></p>
    </apiDesc>
  </dylanModuleDetail>

  <related-links>
EOF

    mkdir "$library/$module" unless -d "$library/$module";
}

sub do_end_module {
    print MODULE << "EOF";
  </related-links>
</dylanModule>
EOF

    close(MODULE);
    if($module_file ne "$library/$module.xml") {
	system("cmp $library/module.xml $module_file >/dev/null") || die;
	if ($? == 0) {
	    remove $module_file;
	}
    }

    undef $module;
    undef $module_file;
}

sub do_char_entry_name {
    my ($name) = @_;

    $entry = $name;

    my $mangled = &mangle($entry);

    my $escaped = $entry;
    $escaped =~ s/&/&amp;/g;
    $escaped =~ s/</&lt;/g;
    $escaped =~ s/>/&gt;/g;
    
    print MODULE << "EOF";
    <link href="$module/$mangled.xml">
      <linktext><apiname>$escaped</apiname></linktext>
    </link>
EOF

    $entry_file = "$library/$module/$mangled.xml";
    if(-f $entry_file) {
	$entry_file .= ".new";
    }

    open(ENTRY, '>', $entry_file) || die "Can't open $entry_file: $!";
}

sub do_end_entry {
    close(ENTRY);
    my $mangled = &mangle($entry);
    
    if($entry_file ne "$library/$module/$mangled.xml") {
	system("cmp $entry_file $library/$module/$mangled.xml >/dev/null")
	    || die;
	if ($? == 0) {
	    remove $entry_file;
	}
    }

    undef $entry;
    undef $entry_file;
}

sub do_start_classdef {
    my $mangled = &mangle($entry);
    my $escaped = &escape($entry);
    
    print ENTRY << "EOF"
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE dylanClass PUBLIC "-//Gwydion//DTD DITA Dylan API Class//EN" "../../../../dtd/dylanClass.dtd" []>
<dylanClass id="lib-$library-$module-$mangled">
  <apiName>$escaped</apiName>
  <shortdesc>The class of .</shortdesc>

  <prolog>
    <author></author>
    <copyright>
      <copyryear year="$year"/>
      <copyrholder>Gwydion Dylan Maintainers</copyrholder>
    </copyright>
  </prolog>

  <dylanClassDetail>
    <dylanClassDef>
EOF
}

sub do_end_classdef {
    print ENTRY << "EOF"
    </dylanClassDef>

    <apiDesc>
      <p></p>
    </apiDesc>
  </dylanClassDetail>
</dylanClass>
EOF
}

sub do_start_modifiers {
    $modifiers = '';
}

sub do_char_modifiers {
    $modifiers .= $_[0];
}

sub do_end_modifiers {
    if($stack[-1] eq 'classdef') {
	if($modifiers =~ /Open/) {
	    print ENTRY << "EOF"
      <dylanOpenClass/>
EOF
	}
	if($modifiers =~ /Primary/) {
	    print ENTRY << "EOF"
      <dylanPrimaryClass/>
EOF
	}
	if($modifiers =~ /Abstract/) {
	    print ENTRY << "EOF"
      <dylanAbstractClass value="abstract-uninstantiable"/>
EOF
	}
    } elsif($stack[-1] eq 'genericdef') {
	if($modifiers =~ /Open/) {
	    print ENTRY << "EOF"
      <dylanGenericFunctionSealing value='open'/>
EOF
	}
    } else {
	die;
    }
}

sub do_start_superclasses {
    $superclasses = '';
}

sub do_char_superclasses {
    $superclasses .= $_[0];
}

sub do_end_superclasses {
    foreach my $class (split /\s+/, $superclasses) {
	next if $class eq '';

	my ($name, $href) = &locate($class);
	my $escaped = &escape($name);
	
	print ENTRY << "EOF";
      <dylanSuperClass href="$href">$escaped</dylanSuperClass>
EOF
    }
}

sub do_char_keyword_name {
    my ($name) = @_;

    $name = &escape($name);

    print ENTRY << "EOF";
      <dylanInitKeyword>
	<apiItemName>$name</apiItemName>
EOF
}

sub do_end_keyword {
    print ENTRY << "EOF";
	<apiDefNote></apiDefNote>
      </dylanInitKeyword>
EOF
}

sub do_char_type {
    my ($string) = @_;

    my ($name, $href) = &locate($string);
    my $escaped = &escape($name);

    my $tag;
    my $s = '  ';
    if($stack[-2] eq 'keyword') {
	$tag = 'apiOtherClassifier';
    } elsif($stack[-2] eq 'constantdef' || $stack[-2] eq 'variabledef') {
	$tag = 'apiValueClassifier';
	$s = '';
    } else {
	$tag = 'apiOperationClassifier';
    }

    if($string eq '{complex type}') {
	print ENTRY << "EOF";
$s      <apiType value="$string"/>
EOF
    } else {
	print ENTRY << "EOF";
$s      <$tag href="$href">$escaped</$tag>
EOF
    }
}

sub do_start_functiondef {
    my $mangled = &mangle($entry);
    my $escaped = &escape($entry);
    
    print ENTRY << "EOF"
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE dylanFunction PUBLIC "-//Gwydion//DTD DITA Dylan API Function//EN" "../../../../dtd/dylanFunction.dtd" []>
<dylanFunction id="lib-$library-$module-$mangled">
  <apiName>$escaped</apiName>
  <shortdesc>Returns .</shortdesc>

  <prolog>
    <author></author>
    <copyright>
      <copyryear year="2005"/>
      <copyrholder>Gwydion Dylan Maintainers</copyrholder>
    </copyright>
  </prolog>

  <dylanFunctionDetail>
    <dylanFunctionDef>
EOF
}

sub do_end_functiondef {
    print ENTRY << "EOF"
    </dylanFunctionDef>

    <apiDesc>
      <p></p>
    </apiDesc>
  </dylanFunctionDetail>
</dylanFunction>
EOF
}

sub do_start_genericdef {
    my $mangled = &mangle($entry);
    my $escaped = &escape($entry);
    
    print ENTRY << "EOF"
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE dylanGenericFunction PUBLIC "-//Gwydion//DTD DITA Dylan API Generic Function//EN" "../../../../dtd/dylanGenericFunction.dtd" []>
<dylanGenericFunction id="lib-$library-$module-$mangled">
  <apiName>$escaped</apiName>
  <shortdesc>Returns .</shortdesc>

  <prolog>
    <author></author>
    <copyright>
      <copyryear year="2005"/>
      <copyrholder>Gwydion Dylan Maintainers</copyrholder>
    </copyright>
  </prolog>

  <dylanGenericFunctionDetail>
    <dylanGenericFunctionDef>
EOF
}

sub do_end_genericdef {
    print ENTRY << "EOF"
    </dylanGenericFunctionDef>

    <apiDesc>
      <p></p>
    </apiDesc>
  </dylanGenericFunctionDetail>
</dylanGenericFunction>
EOF
}

sub do_char_in_name {
    my ($name) = @_;

    $name = &escape($name);

    print ENTRY << "EOF";
      <dylanFunctionParam>
	<apiItemName>$name</apiItemName>
EOF
}

sub do_end_in {
    print ENTRY << "EOF";
	<apiDefNote></apiDefNote>
      </dylanFunctionParam>
EOF
}

sub do_char_rest_in_name {
    my ($name) = @_;

    $name = &escape($name);

    print ENTRY << "EOF";
      <dylanFunctionRestParam>
	<apiItemName>$name</apiItemName>
EOF
}

sub do_end_rest_in {
    print ENTRY << "EOF";
	<apiDefNote></apiDefNote>
      </dylanFunctionRestParam>
EOF
}

sub do_char_keyword_in_name {
    my ($name) = @_;

    $name = &escape($name);

    print ENTRY << "EOF";
      <dylanFunctionKeywordParam>
	<apiItemName>$name</apiItemName>
EOF
}

sub do_end_keyword_in {
    print ENTRY << "EOF";
	<apiDefNote></apiDefNote>
      </dylanFunctionKeywordParam>
EOF
}

sub do_start_all_keys {
    print ENTRY << "EOF";
      <dylanFunctionAllKeywords/>
EOF
}

sub do_char_out_name {
    my ($name) = @_;

    $name = &escape($name);

    print ENTRY << "EOF";
      <dylanFunctionReturn>
	<apiItemName>$name</apiItemName>
EOF
}

sub do_end_out {
    print ENTRY << "EOF";
	<apiDefNote></apiDefNote>
      </dylanFunctionReturn>
EOF
}

sub do_char_rest_out_name {
    my ($name) = @_;

    $name = &escape($name);

    print ENTRY << "EOF";
      <dylanFunctionRestReturn>
	<apiItemName>$name</apiItemName>
EOF
}

sub do_end_rest_out {
    print ENTRY << "EOF";
	<apiDefNote></apiDefNote>
      </dylanFunctionRestReturn>
EOF
}

sub do_start_macrodef {
    my $mangled = &mangle($entry);
    my $escaped = &escape($entry);
    
    print ENTRY << "EOF"
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE dylanMacro PUBLIC "-//Gwydion//DTD DITA Dylan API Function//EN" "../../../../dtd/dylanMacro.dtd" []>
<dylanMacro id="lib-$library-$module-$mangled">
  <apiName>$escaped</apiName>
  <shortdesc></shortdesc>

  <prolog>
    <author></author>
    <copyright>
      <copyryear year="2005"/>
      <copyrholder>Gwydion Dylan Maintainers</copyrholder>
    </copyright>
  </prolog>

  <dylanMacroDetail>
    <apiSyntax>
      <apiSyntaxText></apiSyntaxText>
    </apiSyntax>

    <apiDesc>
      <p></p>
    </apiDesc>
  </dylanMacroDetail>
</dylanMacro>
EOF
}

sub do_start_variabledef {
    my $mangled = &mangle($entry);
    my $escaped = &escape($entry);
    
    print ENTRY << "EOF"
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE dylanVariable PUBLIC "-//Gwydion//DTD DITA Dylan API Variable//EN" "../../../../dtd/dylanVariable.dtd" []>
<dylanVariable id="lib-$library-$module-$mangled">
  <apiName>$escaped</apiName>
  <shortdesc></shortdesc>

  <prolog>
    <author></author>
    <copyright>
      <copyryear year="$year"/>
      <copyrholder>Gwydion Dylan Maintainers</copyrholder>
    </copyright>
  </prolog>

  <dylanVariableDetail>
    <dylanVariableDef>
EOF
}

sub do_end_variabledef {
    print ENTRY << "EOF"
    </dylanVariableDef>

    <apiDesc>
      <p></p>
    </apiDesc>
  </dylanVariableDetail>
</dylanVariable>
EOF
}

sub do_start_constantdef {
    my $mangled = &mangle($entry);
    my $escaped = &escape($entry);
    
    print ENTRY << "EOF"
<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE dylanConstant PUBLIC "-//Gwydion//DTD DITA Dylan API Constant//EN" "../../../../dtd/dylanConstant.dtd" []>
<dylanConstant id="lib-$library-$module-$mangled">
  <apiName>$escaped</apiName>
  <shortdesc></shortdesc>

  <prolog>
    <author></author>
    <copyright>
      <copyryear year="$year"/>
      <copyrholder>Gwydion Dylan Maintainers</copyrholder>
    </copyright>
  </prolog>

  <dylanConstantDetail>
    <dylanConstantDef>
EOF
}

sub do_end_constantdef {
    print ENTRY << "EOF"
    </dylanConstantDef>

    <apiDesc>
      <p></p>
    </apiDesc>
  </dylanConstantDetail>
</dylanConstant>
EOF
}

########################################################################

sub mangle {
    my ($name) = @_;

    $name = lc($name);
    $name =~ tr,-!$%*/<>?+&^_@=~:|,_XDPTSLGQABCUOENMV,;
    return $name;
}

sub escape {
    my ($escaped) = @_;
    $escaped =~ s/&/&amp;/g;
    $escaped =~ s/</&lt;/g;
    $escaped =~ s/>/&gt;/g;
    return $escaped;
}

sub locate {
    my ($name) = @_;

    if($name =~ /^(.*):(.*):(.*)$/) {
	return ($1, "../../$3/$2/" . &mangle($1) . ".xml");
    } elsif($name =~ /^(.*):(.*)$/) {
	return ($1, "../$2/" . &mangle($1) . ".xml");
    } else {
	return ($name, &mangle($name) . ".xml");
    }
}
