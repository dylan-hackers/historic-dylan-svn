#!/usr/bin/perl -w

use strict;
use utf8;
use CGI;
use CGI::Carp;

my $www = "/var/www";
my $wwwdata = "$www/website";
my $wwwtopic = "$www/topic";
my $wwwcms = "$wwwtopic/cms";
my $cache = "$wwwtopic/Cache";
my $ditaot = "$www/topic/dita";
my $urisite = "http://www.opendylan.org";
my $uri = "$urisite/cgi-bin/topic.cgi";
my $uricms = "$urisite/cms";
my $uricss = "$uricms/css";
my $uriimages = "$uricms/images";
my $urijs = "$uricms/js";

my $q = new CGI;
my $view = $q->param('view') || 'default';
my $path = $q->path_info;
my $map  = $q->param('map');

my $parser;
my $xslt;

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
	&not_found($path);
	exit 0;
    }
} elsif($view eq 'default') {
    require XML::LibXML;
    require XML::LibXSLT;

    $parser = new XML::LibXML;
    $xslt = new XML::LibXSLT;

    $parser->load_catalog("$wwwtopic/catalog.xml");
    $parser->load_catalog("$ditaot/catalog-dita.xml");
    
    $parser->expand_entities(1);
    $parser->complete_attributes(1);
    $parser->load_ext_dtd(1);
    $parser->validation(1);

    if(&path_ok($path)
       && $path =~ /\.(xml|dita|ditamap)$/
       && -f "$wwwtopic$path") {
	if($path =~ /\.ditamap$/) {
	    my $mapdoc = $parser->parse_file("$wwwtopic$path");
	    my $toc = &toc($mapdoc);
	    my $tpath = &firsttopic($toc);
	    print $q->redirect("$uri$tpath?map=$path");
	    exit 0;
	}

	print $q->header(-charset => 'utf-8', -encoding => 'utf-8');
	binmode STDOUT, ":utf8";
	&start_html;
	&start_head;
	my $stylesheet
	    = $xslt->parse_stylesheet_file("$wwwcms/xsl/dylan-dita2cms.xsl");

	my $doc = $parser->parse_file("$wwwtopic$path");
	my $result = $stylesheet->transform($doc);

	my $tocdoc = $doc;
	if(defined $map) {
	    $tocdoc = $parser->parse_file("$wwwtopic/$map");
	}
	my $toc;
	if(defined $tocdoc) {
	    $toc = &toc($tocdoc);
	}

	my $tocmap = {};
	my $pathtoc = &tocnav($toc, $path, $tocmap, $map);

	&print_links(\*STDOUT, $path, $toc);

	print "<title>Dylan: ";
	&print_title(\*STDOUT, $path, $stylesheet, $doc, $result);
	print "</title>";
	&end_head;
	&start_body;

	&print_header;
	&print_sidebar(\*STDOUT, $path, $toc);
	&start_main;

	&print_breadcrumbs(\*STDOUT, $path);

	&print_body(\*STDOUT, $path, $stylesheet, $doc, $result);

	&end_main;
	&dump_foot;
	&end_body;
	&end_html;
	exit 0;
    } elsif(-d "$wwwtopic$path"
	    && !($path =~ m|/\.svn/|)) {
	print $q->header(-charset => 'utf-8', -encoding => 'utf-8');
	binmode STDOUT, ":utf8";

	unless($path =~ m|/$|) {
	    $path .= '/';
	}
	
	&start_html;
	&start_head;
	
	&print_links(\*STDOUT, $path);
	
	print "<title>Dylan: Topic $path</title>";
	&end_head;
	&start_body;

	&print_header;
	&print_sidebar(\*STDOUT);
	&start_main;

	&print_breadcrumbs(\*STDOUT, $path);
	
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

        print '<table>';
	print "<thead>";
	print "<tr bgcolor='#6666FF'><th>Title</th><th>Summary</th></tr>";
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

	if(@files != 0) {
	    my $stylesheet
		= $xslt->parse_stylesheet_file("$wwwcms/xsl/dylan-dita2cmsdir.xsl");

	    foreach my $file (sort @files) {
		if($file =~ /\.(xml|dita)$/) {
		    eval {
			print "<tr><td><a href=\"$uri$path$file\">";
			my ($doc, $result) =
			    &print_title(\*STDOUT, "$path$file", $stylesheet);
			print "</a></td>";
			print '<td class="summary">';
			&print_shortdesc(\*STDOUT, "$path$file", $stylesheet,
					 $doc, $result);
			print '</td>';
			print '</tr>';
		    };
		}
		elsif($file =~ /\.ditamap$/) {
		    print "<tr><td><a href=\"$uri$path$file\">";
		    print "<tt>$file</tt>";
		    print "</a></td>";
		    print '<td class="summary">Map</td>';
		    print '</tr>';
		}
	    }
	}
	print "</tbody>";
	print "</table>";
	
	&end_main;
	&dump_foot;
	&end_body;
	&end_html;
	exit 0;
    } elsif (&path_ok($path)
	     && $path =~ /\.svg$/
	     && -f "$wwwtopic$path") {
	&ensure_cache_dirs($path);
	my $png = $path;
	$png =~ s/\.svg$/.png/;
	if(!-f "$cache$png") {
	    system "rsvg '$wwwtopic$path' '$cache$png'";
	}
	if(open(DOWNLOAD, '<:raw', "$cache$png")) {
	    print $q->header(-type => 'image/png',
			     -Content_length => (stat DOWNLOAD)[7]);
	    binmode STDOUT, ':raw';
	    print <DOWNLOAD>;
	    close(DOWNLOAD);
	} else {
	    &not_found($path);
	}
	exit 0;
    } else {
	&not_found($path);
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
	$q->p(shift),
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
    pop(@components) =~ /\.(xml|dita|ditamap|svg|png|gif)$/ || die;

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

########################################################################

sub start_html {
    my $fh = shift || \*STDOUT;
    print $fh <<EOF;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en-US">
EOF
}

sub end_html {
    my $fh = shift || \*STDOUT;
    print $fh <<EOF;
</html>
EOF
}

sub start_head {
    my $fh = shift || \*STDOUT;
    print $fh <<EOF;
  <head>
    <meta http-equiv="content-type"
          content="application/xhtml+xml; charset=UTF-8" />

    <link rel="stylesheet" type="text/css" href="$uricss/tree.css" />
	  
    <link rel="stylesheet" type="text/css"
          href="$uricss/sinorca-screen.css"
          media="screen" title="Sinorca (screen)" />
    <link rel="stylesheet alternative" type="text/css"
          href="$uricss/sinorca-screen-alt.css" 
          media="screen" title="Sinorca (alternative)" />
    <link rel="stylesheet" type="text/css"
          href="$uricss/sinorca-print.css"
          media="print" />
EOF
}

sub end_head {
    my $fh = shift || \*STDOUT;
    print $fh "</head>";
}

sub start_body {
    my $fh = shift || \*STDOUT;
    print $fh "<body>";
}

sub end_body {
    my $fh = shift || \*STDOUT;
    print $fh "</body>";
}

sub print_header {
    my $fh = shift || \*STDOUT;

    print $fh <<EOF;
    <div id="header">
      <div class="superHeader">
        <div class="left">
          <a href="$uri">Home</a>
        </div>
        <div class="right">
          <span class="doNotDisplay">More related sites:</span>
          <a href="./index.html">Link 3</a> |
          <a href="./index.html">Link 4</a> |
          <a href="./index.html">Link 5</a> |
          <a href="./index.html">Link 6</a> |
          <a href="./index.html">Link 7</a>
        </div>
      </div>

      <div class="midHeader">
        <a href="$uri"><img src="$uriimages/dylan-8caae6.png" /></a>
      </div>

      <div class="subHeader">
        <span class="doNotDisplay">Navigation:</span>
        <a href="$uri" class="highlight">Language and Community</a> |
        <a href="$uri">Gwydion Dylan</a> |
        <a href="$uri">Open Dylan</a> |
        <a href="$uri">Support</a>
      </div>
    </div>
EOF
}

sub print_sidebar {
    my ($fh, $path, $toc) = @_;
    $fh = $fh || \*STDOUT;

    my $program = '';
    if($toc) {
	my $count = 0;
	$program = &tocprogram($toc, 'root', \$count);
    }

    print $fh <<EOF;
    <!-- ##### Side Bar ##### -->

    <div id="side-bar">
      <div>
        <p class="sideBarTitle">Contents</p>

	<div id="contents-tree" class="sideBarTree"></div>
      </div>

      <div>
        <p class="sideBarTitle">Related Links</p>

      </div>
    </div>
	
    <script type="text/javascript" src="$urijs/yahoo.js"></script>
    <script type="text/javascript" src="$urijs/event.js"></script>
    <script type="text/javascript" src="$urijs/treeview.js"></script>

<script type="text/javascript">
//<![CDATA[
var tree;
function treeInit() {
  tree = new YAHOO.widget.TreeView("contents-tree");
  var root = tree.getRoot();
  $program
  tree.draw();
}
YAHOO.util.Event.addListener(window, "load", treeInit);
//]]>
</script>
EOF
}

sub start_main {
    my $fh = shift || \*STDOUT;
    print $fh <<EOF;
    <!-- ##### Main Copy ##### -->

    <div id="main-copy">
EOF
}

sub end_main {
    my $fh = shift || \*STDOUT;
    print $fh <<EOF;
    </div>
EOF
}

sub dump_foot {
    my $fh = shift || \*STDOUT;
    print $fh <<EOF;
    <!-- ###### Footer ###### -->

    <!--UdmComment-->
    <div id="footer">
      <div>
        <span class="left">
          [<a href="mailto:gd-hackers\@gwydiondylan.org">Feedback</a>]
        </span>
      </div>

      <br class="doNotDisplay doNotPrint" />

      <div class="right">
Copyright &copy;1998&ndash;2006 Gwydion Dylan Maintainers. All rights
reserved.
      </div>

      <div class="right">  Created using <A
HREF="http://www.apache.org/">Apache</A> on a machine running <A
HREF="http://www.debian.org/">Debian</A>. Graphics by the <A
HREF="http://www.gimp.org/">GIMP</A>. Version control by <A HREF="http://subversion.tigris.org/">Subversion</A>.
      </div>
    </div>
    <!--/UdmComment-->
EOF
}

########################################################################

sub print_links {
    my ($fh, $path, $toc) = @_;

    print $fh '<link rel="top" href="', $uri, '" title="Top" />';

    my @components = split m|/|, $path;
    shift(@components) && die;
    pop(@components) || return;

    $path = join('/', @components);

    print $fh '<link rel="up" href="', $uri, '/', $path, '" title="Up" />';
}

sub print_breadcrumbs {
    my ($fh, $path) = @_;

    my @components = split m|/|, $path;
    shift(@components) && die;
    pop(@components);

    my $href = $uri;
    print '<div class="breadcrumbs"><a href="', $uri, '">Topic</a> &gt; ';
    while(my $component = shift @components) {
	$href = "$href/$component";
	print '<a href="', $href, '">', $component, '</a> &gt; ';
    }
    print "</div>\n";
}

sub print_title {
    my ($fh, $path, $stylesheet, $doc, $result) = @_;

    my $titlecache = "$cache$path.title";
    if(-f $titlecache
       && (-M $titlecache) < (-M "$wwwtopic$path")
       && open(CACHE, '<:utf8', $titlecache)) {
	print $fh <CACHE>;
	close(CACHE);
	return;
    }

    unless(defined $doc) {
	$doc = $parser->parse_file("$wwwtopic$path");
    }
    unless(defined $result) {
	$result = $stylesheet->transform($doc);
    }

    my $title = $result->findnodes('/html/head/title/node()');
    &printHTML(\*STDOUT, $title);

    if(&ensure_cache_dirs($path) && open(CACHE, '>:utf8', $titlecache)) {
	&printHTML(\*CACHE, $title);
	close(CACHE);
    }

    return ($doc, $result);
}

sub print_shortdesc {
    my ($fh, $path, $stylesheet, $doc, $result) = @_;

    my $shortdesccache = "$cache$path.shortdesc";
    if(-f $shortdesccache
       && (-M $shortdesccache) < (-M "$wwwtopic$path")
       && open(CACHE, '<:utf8', $shortdesccache)) {
	print $fh <CACHE>;
	close(CACHE);
	return;
    }

    unless(defined $doc) {
	$doc = $parser->parse_file("$wwwtopic$path");
    }
    unless(defined $result) {
	$result = $stylesheet->transform($doc);
    }

    my $shortdesc
	= $result->findnodes('/html/body/p[@class="shortdesc"]/node()');
    &printHTML(\*STDOUT, $shortdesc);

    if(&ensure_cache_dirs($path) && open(CACHE, '>:utf8', $shortdesccache)) {
	&printHTML(\*CACHE, $shortdesc);
	close(CACHE);
    }

    return ($doc, $result);
}

sub print_body {
    require IO::Tee;
    my ($fh, $path, $stylesheet, $doc, $result) = @_;

    my $bodycache = "$cache$path.body";
    if(-f $bodycache
       && (-M $bodycache) < (-M "$wwwtopic$path")
       && open(CACHE, '<:utf8', $bodycache)) {
	print $fh <CACHE>;
	close(CACHE);
	return;
    }

    unless(defined $doc) {
	$doc = $parser->parse_file("$wwwtopic$path");
    }
    unless(defined $result) {
	$result = $stylesheet->transform($doc);
    }

    my $body = $result->findnodes('/html/body/node()');

    if(&ensure_cache_dirs($path) && open(CACHE, '>:utf8', $bodycache)) {
	my $tee = new IO::Tee(\*STDOUT, \*CACHE);
	&printHTML($tee, $body);
	close(CACHE);
    } else {
	&printHTML(\*STDOUT, $body);
    }

    return ($doc, $result);
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
	elsif($type eq 'XML::LibXML::NodeList') {
	    &printHTML($fh, $node->get_nodelist);
	}
	elsif($type eq 'XML::LibXML::Comment') {
	    # do nothing
	}
	else {
	    die $type;
	}
    }
}

########################################################################

sub toc {
    my ($doc) = @_;
    my ($root) = $doc->findnodes('/*');
    my $class = $root->getAttribute('class');

    if($class =~ m| map/map |) {
	return &toc_map($root);
    }
    elsif($class =~ m| topic/topic |) {
	return &toc_topic($root);
    } else {
	die $root->nodeName . " (class=$class)";
    }
}

sub toc_map {
    my ($node) = @_;

    my $p = $node->baseURI;
    $p =~ s|^$wwwtopic||;

    my $self = {
	uri => $p,
	title => $node->getAttribute('title'),
    };

    my @children = $node->findnodes('./*');
    if (scalar @children) {
	my @results;
	foreach my $element (@children) {
	    my $class = $element->getAttribute('class');
	    if($class =~ m| map/topicref |) {
		push @results, &toc_topicref($element)
	    }
	    else {
		die $element->nodeName . " (class=$class) in map";
	    }
	}
	$self->{'children'} = \@results;
    }

    return $self;
}

sub toc_topicref {
    my ($node) = @_;

    my $self;
    if($node->getAttribute('href')) {
	my ($ref) = $node->findnodes('document(@href)');
	$self = &toc($ref);
    }
    else {
	$self = {};
    }

    my $title = $node->getAttribute('navtitle');
    if ($title) {
	$self->{'title'} = $title;
    }

    my @children = $node->findnodes('./*');
    if (scalar @children) {
	my @results;
	foreach my $element (@children) {
	    my $class = $element->getAttribute('class');
	    if($class =~ m| map/topicref |) {
		push @results, &toc_topicref($element)
	    }
	    else {
		die $element->nodeName . " (class=$class) in topicref";
	    }
	}
	if (exists $self->{'children'}) {
	    push @{$self->{'children'}}, @results;
	} else {
	    $self->{'children'} = \@results;
	}
    }

    return $self;
}

sub toc_topic {
    my ($node) = @_;

    my $p = $node->baseURI;
    $p =~ s|^$wwwtopic||;

    my (@title)
	= $node->findnodes('/*/*[contains(@class," topic/title ")]/text()');
    my $title = '';
    foreach my $n (@title) { $title .= $n->data };

    my $self = {
	title => $title,
	uri => $p,
    };

    my @children;
    my (@subtopics)
	= $node->findnodes('/*/*[contains(@class," topic/topic ")]');
    foreach my $topic (@subtopics) {
	my $id = $topic->getAttribute('id');

	my (@subtitle)
	    = $topic->findnodes('./*[contains(@class," topic/title ")]/text()');
	my $subtitle = '';
	foreach my $n (@subtitle) { $subtitle .= $n->data };
	
	push @children, { uri => "$p#$id", title => $subtitle };
    }

    if(scalar @children) {
	$self->{'children'} = \@children;
    }

    return $self;
}

########################################################################

sub tocnav {
    my ($toc, $path, $tocmap, $map) = @_;

    $tocmap->{$toc->{'uri'}} = $toc;
    $toc->{'navuri'} = $toc->{'uri'};

    if(defined $map) {
	$toc->{'navuri'} =~ s/^([^\#]+)/$1?map=$map/;
    }

    my $pathtoc;
    if($toc->{'children'}) {
	my $prev = $toc;
	foreach my $child (@{$toc->{'children'}}) {
	    my $childpathtoc = &tocnav($child, $path, $tocmap, $map);
	    if(defined $childpathtoc) {
		$pathtoc = $childpathtoc;
	    }
	}
    }

    if($path eq $toc->{'uri'}) {
	$pathtoc = $toc;
    }

    if(defined $pathtoc) {
	$toc->{'expand'} = 1;
    }

    return $pathtoc;
}

# Constructs an ECMAScript program fragment to build YUI Treeview nodes
# from a TOC.
#
sub tocprogram {
    my ($toc, $parent, $rcount) = @_;

    my $name = "node$$rcount";
    ++$$rcount;

    my $expand = $toc->{'expand'} ? 'true' : 'false';

    my $title = $toc->{'title'};
    $title =~ s/&/&amp;/g;
    $title =~ s/</&lt;/g;
    $title =~ s/>/&gt;/g;
    $title =~ s/'/&quot;/g;

    my $program;
    if(defined $toc->{'uri'}) {
	$program = "var $name = new YAHOO.widget.TextNode({ label:'$title', href:'$uri$toc->{'navuri'}' }, $parent, $expand);\n";
    } else {
	$program = "var $name = new YAHOO.widget.TextNode('$title', $parent, $expand);\n";
    }

    if($toc->{'children'}) {
	foreach my $child (@{$toc->{'children'}}) {
	    $program .= &tocprogram($child, $name, $rcount);
	}
    }
    
    return $program;
}

# Returns the path of the first topic in a TOC.
#
sub firsttopic {
    my ($toc) = @_;
    my $tpath = $toc->{'uri'};
    if($tpath =~ /\.(xml|dita)$/) {
	return $tpath;
    }

    if($toc->{'children'}) {
	foreach my $child (@{$toc->{'children'}}) {
	    $tpath = &firsttopic($child);
	    return $tpath if defined $tpath;
	}
    }
    return undef;
}
