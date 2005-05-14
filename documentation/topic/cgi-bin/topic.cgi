#!/usr/local/bin/perl -wt

use strict;
use utf8;
use CGI;
use CGI::Carp;

my $www = "/usr/local/www";
my $wwwdata = "$www/data";
my $wwwtopic = "$www/topic";
my $ditaot = "$www/topic/dita";
my $uri = "http://www.gwydiondylan.org/cgi-bin/topic.cgi";

my $q = new CGI;
my $view = $q->param('view') || 'default';
my $path = $q->path_info;

if($view eq 'download') {
    if(&path_ok($path)
       && $path =~ m|\.xml$|
       && -f "$wwwtopic$path"
       && open(DOWNLOAD, '<', "$wwwtopic$path")) {
	print $q->header(-type => 'text/xml',
			 -charset => 'utf-8',
			 -Content_length => (stat DOWNLOAD)[7]);
	print <DOWNLOAD>;
	close(DOWNLOAD);
	exit 0;
    } else {
	&not_found;
	exit 0;
    }
} elsif($view eq 'default') {
    require XML::LibXML;
    require XML::LibXSLT;

    my $parser = new XML::LibXML;
    my $xslt = new XML::LibXSLT;

    $parser->load_catalog("$wwwtopic/catalog.xml");
    $parser->load_catalog("$ditaot/catalog-dita.xml");
    
    my $stylesheet
	= $xslt->parse_stylesheet_file("$wwwtopic/xsl/dylan-dita2xhtml.xsl");

    $parser->expand_entities(1);
    $parser->complete_attributes(1);
    $parser->load_ext_dtd(1);
    $parser->validation(1);

    if(&path_ok($path)
       && $path =~ m|\.xml$|
       && -f "$wwwtopic$path") {
	print $q->header(-charset => 'utf-8');
	&dump_head;

	my $doc = $parser->parse_file("$wwwtopic$path");
	my $result = $stylesheet->transform($doc);

	print "<title>Gwydion Dylan: ";
	&printHTML($result->findnodes("/html/head/title/text()"));
	print "</title>";
	print "<body>";
	&dump_menu;

	&printHTML($result->findnodes("/html/body/node()"));
	&dump_foot;
	exit 0;
    } elsif(-d "$wwwtopic$path"
	    && !($path =~ m|/\.svn/|)) {
	print $q->header(-charset => 'utf-8');
	&dump_head;
	print "<title>Gwydion Dylan: Topic $path</title>";
	print "<body>";
	&dump_menu;
	print $q->h1("Directory $path");
	&dump_foot;
	exit 0;
    } else {
	&not_found;
	exit 0;
    }
	    
} else {
    exit 1;
}

exit 0;

########################################################################

sub not_found {
    print
	$q->header('text/html', '404 Not Found'),
	$q->start_html('404 Not Found'),
	$q->h1('404 Not Found'),
	$q->end_html;
}

sub path_ok {
    my ($path) = @_;
    return
	($path =~ m|^/reference/|
	 || $path =~ m|^/map/|
	 || $path =~ m|^/concept/|
	 || $path =~ m|^/task/|);
}

sub dump {
    my ($name) = @_;
    open(FILE, '<', $name) || die "Couldn't open $name: $!";
    print <FILE>;
    close(FILE);
}

sub dump_head {
    &dump("$wwwdata/header.html");
}

sub dump_menu {
    &dump("$wwwdata/menu.html");
}

sub dump_foot {
    &dump("$wwwdata/footer.html");
}

sub printHTML {
    while(my $node = shift) {
	my $type = ref $node;
	if($type eq 'XML::LibXML::Text') {
	    my $data = $node->data;
	    $data =~ s/&/&amp;/g;
	    $data =~ s/</&lt;/g;
	    $data =~ s/>/&gt;/g;
	    print $data;
	}
	elsif($type eq 'XML::LibXML::Element') {
	    print '<', $node->nodeName;
	    foreach my $attr ($node->attributes) {
		print ' ', $attr->nodeName, '="';
		my $value = $attr->nodeValue;
		$value =~ s/&/&amp;/g;
		$value =~ s/</&lt;/g;
		$value =~ s/>/&gt;/g;
		$value =~ s/\"/&quot/g;
		print $value, '\"';
	    }
	    my $child = $node->firstChild;
	    if(defined $child) {
		print ">";
		while($child) {
		    &printHTML($child);
		    $child = $child->nextSibling;
		}
		print '</', $node->nodeName, '>';
	    } else {
		print ' />'
	    }
	}
	elsif($type eq 'XML::LibXML::Comment') {
	    # do nothing
	}
	else {
	    die $type;
	}
    }
}
