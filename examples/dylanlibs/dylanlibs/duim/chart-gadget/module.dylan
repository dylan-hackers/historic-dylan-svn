Module:    dylan-user
Synopsis:  Graphing panes for DUIM
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define module chart-gadget
  use functional-dylan;
  use duim;
  use format;

  // Add binding exports here.
  export <data-series>,
    data-series-name,
    data-series-name-setter,
    data-series-sequence,
    data-series-sequence-setter,
    make-data-series,
    <chart>,
    draw,
    <line-chart>,
    chart-data-series,
    chart-data-series-setter,
    chart-tick-values,
    chart-tick-values-setter,
    add-data,
    <chart-pane>,
    chart-pane-chart,
    chart-pane-chart-setter,
    draw-chart;
    
end module chart-gadget;
