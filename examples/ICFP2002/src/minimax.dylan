module: minimax
author: Ingo Albrecht <prom@berlin.ccc.de>

define method max-value( state, alpha, beta )
  if (cutoff-test(state))
    evaluate(state);
  else
    for (s in successors(state))
      alpha := max(alpha, min-value( s, alpha, beta ));
      if ( alpha >= beta )
        beta;
      else
        alpha;
      end;
    end;
  end;
end method;

define method min-value( state, alpha, beta )
  if (cutoff-test(state))
    evaluate(state);
  else
    for (s in successors(state))
      beta := min( beta, max-value( s, alpha, beta ));
      if ( beta <= alpha )
        alpha;
      else
        beta;
      end;
    end;
  end;
end method;
