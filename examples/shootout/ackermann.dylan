module: ackermann

define function ackermann(M :: <integer>, N :: <integer>) => result :: <integer>;
	if (M = 0)
		N + 1;
	elseif (N = 0)
		ackermann( (M - 1), 1 );
	else
		ackermann ( (M - 1), ackermann (M, (N - 1)));
	end if;
end function ackermann;

define function main(name, argument)
	let arg :: <integer> = string-to-integer(argument[0]);

	format-out("Ack(3,%d): %d", arg,  ackermann(3, arg));
end function main;

main(application-name(), application-arguments());
