Module:    chart-gadget
Synopsis:  Graphing panes for DUIM
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define class <data-series> (<object>)
  slot data-series-name :: <string>, init-keyword: name:;
  slot data-series-sequence :: <sequence>, init-keyword: sequence:;
end class;

define method make-data-series(name :: <string>, data :: <sequence>) => (r :: <data-series>)
  make(<data-series>, name: name, sequence: data);
end;

define class <chart> (<object>)
end class;

define function chart-rgb(r, g, b) 
  let r = as(<double-float>, r);
  let g = as(<double-float>, g);
  let b = as(<double-float>, b);
  make(<color>, red: r / 255, green: g / 255, blue: b / 255)
end;

define constant $brush-colors = 
    vector(chart-rgb(184, 134, 11),
           chart-rgb(0, 100, 0),
           chart-rgb(189, 183, 107),
           chart-rgb(85, 107, 47),
           chart-rgb(255, 140, 0),
           chart-rgb(153, 50, 204),
           chart-rgb(233, 150, 122),
           chart-rgb(143, 188, 143),
           chart-rgb(72, 61, 139),
           chart-rgb(47, 79, 79),
           chart-rgb(0, 206, 209),
           chart-rgb(148, 0, 211),
           chart-rgb(255, 20, 147),
           chart-rgb(0, 191, 255),
           chart-rgb(30, 144, 255),
           chart-rgb(178, 34, 34),
           chart-rgb(100, 149, 237),
           chart-rgb(0, 0, 0),
           chart-rgb(0, 255, 255),
           chart-rgb(184, 134, 11),
           chart-rgb(0, 100, 0),
           chart-rgb(189, 183, 107),
           chart-rgb(85, 107, 47),
           chart-rgb(255, 140, 0));
           
vector($blue, $red, $green, $magenta, $cyan, $yellow);

define method draw(chart :: <chart>, pane :: <chart-pane>, medium :: <medium>, region :: <region>) => ()
  let _port = port(pane);
  let text-height = font-height(get-default-text-style(_port, pane), _port);
  let (width, height) = sheet-size(pane);
  let gap-top = 10;
  let gap-bottom = 10;
  let gap-left = 10;
  let gap-right = 50;
  let ds = chart.chart-data-series;
  let min-value = 0;
  let max-value = 0;
  let series-number = 0;
  for(series in ds)
    min-value := apply(min, min-value, series.data-series-sequence);
    max-value := apply(max, max-value, series.data-series-sequence);
    series-number := max(series-number, series.data-series-sequence.size);
  end;
  let series-tick-gap = truncate/(width - (gap-left + gap-right), series-number);

  local method translate-x(index :: <integer>)
	index * series-tick-gap + gap-left;
  end;

  local method translate-y(dv :: <integer>)
    let top = gap-top;
    let bottom = height - gap-bottom;
    let percent = as(<double-float>, dv - min-value) /  (max-value - min-value);
    truncate(bottom - ((bottom - top) * percent));
  end;

  local method draw-axis()    
    let brush = $blue;
    let pen = make(<pen>, width: 1);
    with-drawing-options(medium, brush: brush, pen: pen)
      draw-line(medium, gap-left, gap-top, gap-left, height - gap-bottom);
      draw-line(medium, gap-left, height - gap-bottom, width - gap-right, height - gap-bottom);
      draw-text(medium, format-to-string("%d", max-value), 0, gap-top);
      draw-text(medium, format-to-string("%d", min-value), 0, height - gap-bottom);
      let tick = 0;
      for(count from 0 below series-number)
        draw-point(medium, gap-left + tick, height - gap-bottom + 1);
        tick := tick + series-tick-gap;
      end;

      for(tv in chart.chart-tick-values)
        unless(tv >= max-value)
          draw-point(medium, gap-left - 1, translate-y(tv));
          draw-text(medium, format-to-string("%d", tv), 0, translate-y(tv));
        end;
      end;
    end;
  end;
  local method draw-points()
    for(series keyed-by ds-index in ds)
      let brush = $brush-colors[min(ds-index, $brush-colors.size - 1)];
      let pen = make(<pen>, width: 1);
      with-drawing-options(medium, brush: brush, pen: pen)
        for(dv keyed-by index in series.data-series-sequence)
          unless(index = 0)
            draw-line(medium, 
              translate-x(index - 1), 
              translate-y(series.data-series-sequence[index - 1]),
              translate-x(index),
              translate-y(dv))
          end;
        end for;
      end;
      let brush = $brush-colors[min(ds-index, $brush-colors.size - 1)];
      with-drawing-options(medium, brush: brush, pen: pen)
        draw-text(medium, 
                  series.data-series-name, 
                  translate-x(series.data-series-sequence.size - 1) + 2, 
                  translate-y(series.data-series-sequence.last));
      end;
    end;	    
  end;
  local method draw-legend()
    let start-x = width - gap-right + 5;
	let start-y = gap-top;

    for(series keyed-by ds-index in ds)
		let brush = $brush-colors[min(ds-index, $brush-colors.size - 1)];
		let pen = make(<pen>, width: 1);
		with-drawing-options(medium, brush: brush, pen: pen)
			draw-text(medium, series.data-series-name, start-x, start-y);
	    end;
   
	    start-y := start-y + text-height + 2;
	end;
	      
  end;

  clear-box*(medium, $everywhere);
  draw-axis();
  draw-legend();
  draw-points();
  
end;

define class <line-chart> (<chart>)
  slot chart-data-series :: <sequence> = make(<stretchy-vector>);
  slot chart-tick-values :: <sequence>;
end class;

define method add-data(chart :: <line-chart>, ds :: <data-series>) => ()
  chart.chart-data-series := add!(chart.chart-data-series, ds);
end;

define class <chart-pane> (<drawing-pane>)
  slot chart-pane-chart = #f;
end;


define method draw-chart
    (pane :: <chart-pane>, medium :: <medium>, region :: <region>) => ()
  if(pane.chart-pane-chart)
    draw(pane.chart-pane-chart, pane, medium, region);	
  end;
end;



