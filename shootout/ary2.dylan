module: ary2

define constant <int-vector> = limited(<vector>, of: <integer>);

define function main(name, arguments)
	let n :: <integer> = string-to-integer(arguments[0]);

	let x = make(<int-vector>, size: n, fill: 0);
	let y = make(<int-vector>, size: n, fill: 0);

	let i :: <integer> = 0;

	while ( i < n )
		x[i] := i; i := i + 1;
		x[i] := i; i := i + 1;
		x[i] := i; i := i + 1;
		x[i] := i; i := i + 1;
		x[i] := i; i := i + 1;

		x[i] := i; i := i + 1;
		x[i] := i; i := i + 1;
		x[i] := i; i := i + 1;
		x[i] := i; i := i + 1;
		x[i] := i; i := i + 1;
	end while;

	i := n - 1;

	while ( i >= 0 )
		y[i] := x[i]; i := i - 1;
		y[i] := x[i]; i := i - 1;
		y[i] := x[i]; i := i - 1;
		y[i] := x[i]; i := i - 1;
		y[i] := x[i]; i := i - 1;

		y[i] := x[i]; i := i - 1;
		y[i] := x[i]; i := i - 1;
		y[i] := x[i]; i := i - 1;
		y[i] := x[i]; i := i - 1;
		y[i] := x[i]; i := i - 1;
	end while;

	format-out("%d\n", y[y.size - 1]);

end function main;

main(application-name(), application-arguments());
