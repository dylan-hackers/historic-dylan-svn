#!/usr/local/bin/perl -w

use CGI;
use CGI::Carp;
use strict;

my $www = "/usr/local/www";
my $wwwdata = "$www/data";
my $wwwtopic = "$www/topic";
my $uri = "http://www.gwydiondylan.org/cgi-bin/topic.cgi";

open(HEAD, '<', "$wwwdata/header.html") || exit 1;
my @head = <HEAD>;
close(HEAD);

open(MENU, '<', "$wwwdata/menu.html") || exit 1;
my @menu = <MENU>;
close(MENU);

open(FOOT, '<', "$wwwdata/footer.html") || exit 1;
my @foot = <FOOT>;
close(FOOT);

my $q = new CGI;
my $view = $q->param('view') || 'default';
my $path = $q->path_info;

if($view eq 'download') {
    if(($path =~ m|^/reference/|
	|| $path =~ m|^/map/|
	|| $path =~ m|^/concept/|
	|| $path =~ m|^/task/|)
       && $path =~ m|\.xml$|
       && -f "$wwwtopic$path"
       && open(DOWNLOAD, '<', "$wwwtopic$path")) {
	print $q->header('text/xml'), <DOWNLOAD>;
	close(DOWNLOAD);
	exit 0;
    } else {
	print $q->header('text/html', '404 Not Found'),
	      $q->start_html('404 Not Found'),
	      $q->h1('404 Not Found'),
	      $q->end_html;
	exit 0;
    }
} elsif($view eq 'default') {
    print $q->header, @head, @menu;

    print $q->h1("Hello topic"),
    $q->p($q->path_info);

    print @foot;
} else {
    exit 1;
}

exit 0;

