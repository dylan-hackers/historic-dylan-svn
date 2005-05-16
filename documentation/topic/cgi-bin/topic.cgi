#!/usr/local/bin/perl -w

use strict;
use utf8;
use CGI;
use CGI::Carp;

my $www = "/usr/local/www";
my $wwwdata = "$www/data";
my $wwwtopic = "$www/topic";
my $cache = "$wwwtopic/Cache";
my $ditaot = "$www/topic/dita";
my $uri = "http://www.gwydiondylan.org/cgi-bin/topic.cgi";

my $q = new CGI;
my $view = $q->param('view') || 'default';
my $path = $q->path_info;

if($view eq 'download') {
    if(&path_ok($path)
       && $path =~ /\.(xml|dita|ditamap)$/
       && -f "$wwwtopic$path"
       && open(DOWNLOAD, '<', "$wwwtopic$path")) {
	print $q->header(-type => 'text/xml',
			 -charset => 'utf-8',
			 -encoding => 'utf-8',
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
    require IO::Tee;

    my $parser = new XML::LibXML;
    my $xslt = new XML::LibXSLT;

    $parser->load_catalog("$wwwtopic/catalog.xml");
    $parser->load_catalog("$ditaot/catalog-dita.xml");
    
    my $stylesheet
	= $xslt->parse_stylesheet_file("$wwwtopic/xsl/dylan-dita2cms.xsl");

    $parser->expand_entities(1);
    $parser->complete_attributes(1);
    $parser->load_ext_dtd(1);
    $parser->validation(1);

    if(&path_ok($path)
       && $path =~ /\.(xml|dita|ditamap)$/
       && -f "$wwwtopic$path") {
	print $q->header(-charset => 'utf-8', -encoding => 'utf-8');
	binmode STDOUT, ":utf8";
	&dump_head;

	my $cachepath = "$cache$path.cache";
	if(-f $cachepath
	   && (-M $cachepath) < (-M "$wwwtopic$path")
	   && (-M $cachepath) < (-M "$wwwdata/menu.html")
	   && open(CACHE, '<:utf8', $cachepath)) {
	    print <CACHE>;
	    close(CACHE);
	    &dump_foot;
	    exit 0;
	}

	my $fh = \*STDOUT;
	if(&ensure_cache_dirs($path) && open(CACHE, '>:utf8', $cachepath)) {
	    $fh = new IO::Tee(\*STDOUT, \*CACHE);
	} else {
	    warn "Couldn't open cache file $cachepath: $!";
	}

	my $doc = $parser->parse_file("$wwwtopic$path");
	my $result = $stylesheet->transform($doc);

	print $fh "<title>Gwydion Dylan: ";
	&printHTML($result->findnodes("/html/head/title/text()"));
	print $fh "</title>";
	print $fh "</HEAD>";
	print $fh "<body>";
	&dump_menu($fh);

	&printHTML($fh, $result->findnodes("/html/body/node()"));
	&dump_foot;
	exit 0;
    } elsif(-d "$wwwtopic$path"
	    && !($path =~ m|/\.svn/|)) {
	print $q->header(-charset => 'utf-8', -encoding => 'utf-8');
	binmode STDOUT, ":utf8";
	&dump_head;
	print "<title>Gwydion Dylan: Topic $path</title>";
	print "<body>";
	&dump_menu;

	unless($path =~ m|/$|) {
	    $path .= '/';
	}
	
	print $q->h1("Topic directory $path");

	my @dirs;
	my @files;
	opendir(DIR, "$wwwtopic$path");
	while(my $entry = readdir DIR) {
	    next if($entry =~ /^\./);
	    if(-d "$wwwtopic$path$entry") {
		push @dirs, "$entry/";
	    } else {
		push @files, $entry;
	    }
	}
	closedir(DIR);

        print "<table>";
	print "<thead>";
	print "<tr><th>Title</th><th>Summary</th></tr>";
	print "</thead>";
	print "<tbody>";
	foreach my $dir (sort @dirs) {
	    if(&path_ok("$path$dir")) {
		print "<tr>";
		print "<td><a href=\"$uri$path$dir\">$dir</a></td>";
		print "<td><i>Directory</i></td>";
		print "</tr>";
	    }
	}
	foreach my $file (sort @files) {
	    if($file =~ /\.(xml|dita|ditamap)$/) {
		eval {
		    my $doc = $parser->parse_file("$wwwtopic$path$file");
		    my $result = $stylesheet->transform($doc);
		    print "<tr><td><a href=\"$uri$path$file\">";
		    printHTML($result->findnodes("/html/head/title/text()"));
		    print "</a></td>";
		    print '<td class="summary">', "Summary", '</td>';
		    print '</tr>';
		}
	    }
	}
	print "</tbody>";
	print "</table>";
	
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

sub ensure_cache_dirs {
    my ($path) = @_;
    my @components = split m|/|, $path;
    shift(@components) && die;
    pop(@components) =~ /\.(xml|dita|ditamap)$/ || die;

    my $dir = $cache;
    while(my $component = shift @components) {
	$dir = "$dir/$component";
	unless(-d $dir) {
	    unless(mkdir $dir) {
		warn "Unable to create $dir: $!";
		return undef;
	    }
	}
    }
    return 1;    
}

sub dump {
    my ($fh, $name) = @_;
    open(FILE, '<', $name) || die "Couldn't open $name: $!";
    print $fh <FILE>;
    close(FILE);
}

sub dump_head {
    my $fh = shift || \*STDOUT;
    &dump($fh, "$wwwdata/header.html");
}

sub dump_menu {
    my $fh = shift || \*STDOUT;
    &dump($fh, "$wwwdata/menu.html");
}

sub dump_foot {
    my $fh = shift || \*STDOUT;
    &dump($fh, "$wwwdata/footer.html");
}

sub printHTML {
    my $fh = shift;
    while(my $node = shift) {
	my $type = ref $node;
	if($type eq 'XML::LibXML::Text') {
	    my $data = $node->data;
	    $data =~ s/&/&amp;/g;
	    $data =~ s/</&lt;/g;
	    $data =~ s/>/&gt;/g;
	    print $fh $data;
	}
	elsif($type eq 'XML::LibXML::Element') {
	    print $fh '<', $node->nodeName;
	    foreach my $attr ($node->attributes) {
		print $fh ' ', $attr->nodeName, '="';
		my $value = $attr->nodeValue;
		$value =~ s/&/&amp;/g;
		$value =~ s/</&lt;/g;
		$value =~ s/>/&gt;/g;
		$value =~ s/\"/&quot/g;
		print $fh $value, '"';
	    }
	    my $child = $node->firstChild;
	    if(defined $child) {
		print $fh ">";
		while($child) {
		    &printHTML($fh, $child);
		    $child = $child->nextSibling;
		}
		print $fh '</', $node->nodeName, '>';
	    } else {
		print $fh ' />'
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
