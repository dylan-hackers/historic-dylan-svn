module: ary

define function main(name, arguments)
	let n :: <integer> = string-to-integer(arguments[0]);

	let x :: <stretchy-vector> = make(<stretchy-vector>, size: n, fill: 0);
	let y :: <stretchy-vector> = make(<stretchy-vector>, size: n, fill: 0);

	for ( i from 0 below x.size)
		x[i] := i;
	end for;
	
	for ( i from n - 1 above 0 by -1 )
		y[i] := y[i] + x[i];
	end for;

	format-out("%d", y[y.size - 1]);

end function main;

main(application-name(), application-arguments());
