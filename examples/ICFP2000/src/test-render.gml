{ /v /u /face         % ignore the args
  0.0 0.0 1.0 point   % color
  1.0		      % Full diffusion coef
  0.0		      % No specular
  1.0		      % No phong, either
} /blue

{ /v /u /face         % ignore the args
  1.0 0.0 0.0 point   % color
  1.0		      % Full diffusion coef
  0.0		      % specular
  1.0		      % phong exp
} /red

{ /v /u /face
  1.0 1.0 1.0 point
  0.0
  1.0
  0.0
} /mirror

red sphere 0.3 uscale /s
blue plane 0.0 -0.5 0.0 translate /p

0.0 -1.0 0.0 point    % straight down
1.0 1.0 1.0 point     % white
light /sun

1.0 1.0 1.0 point     % dim ambient light
[ ] 		      
s p union
2 		      % depth
90.0		      % FOV	
256		      % width
256		      % height
"result.ppm"	      % filename
render
