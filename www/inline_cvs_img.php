<?php
include("/usr/lib/phplot/phplot.php");
include("/usr/lib/phplot/rgb.inc.php");
include("/tmp/plot.data.php");
// Test data
//$cvs_data = array(
//	array( "", "2000/03/15", 750),
//	array("", "2010/04/18", 1700),
//	array("", "2015/05/19", 2000),
//	array("", "2030/06/20", 400)
//	);

$graph = new PHPlot(238,220);
$graph->SetIsInline("1");
$graph->SetDataType("text-data");
$graph->SetPlotType("bars");
$graph->SetDataValues($cvs_data);

// Specify plotting area details
$graph->SetImageArea(238,220);
$graph->SetPlotAreaPixels(55,20,208,190);
$graph->SetPrecisionY("0");
$graph->SetVertTickIncrement("100");
$graph->SetTickLength("5");

$graph->SetTitleFontSize("2");
$graph->SetPlotBgColor(204,204,204);
$graph->SetPlotBorderType("left");
$graph->SetBackgroundColor($ColorArray["gray80"]);

// Define the X axis
$graph->SetXLabel("Date");

// Define the Y axis

$graph->SetDataColors( array("blue","red"), array("black"));
$graph->SetFileFormat("png");
$graph->DrawGraph();

?>
