module: ary2

define function main(name, arguments)
	let n :: <integer> = string-to-integer(arguments[0]);

	let x :: <stretchy-vector> = make(<stretchy-vector>, size: n, fill: 0);
	let y :: <stretchy-vector> = make(<stretchy-vector>, size: n, fill: 0);

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

	format-out("%d", y[y.size - 1]);

end function main;

main(application-name(), application-arguments());
