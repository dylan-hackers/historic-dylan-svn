{ /v /u /face         % ignore the args
  0.0 0.0 1.0 point   % color
  1.0		      % Full diffusion coef
  0.0		      % No specular
  0.0		      % No phong, either
} sphere 0.3 uscale /s

{ /v /u /face         % ignore the args
  1.0 0.0 0.0 point   % color
  1.0		      % Full diffusion coef
  0.0		      % No specular
  0.0		      % No phong, either
} plane 0.0 -0.5 0.0 translate /p

0.0 -1.0 0.0 point    % straight down
1.0 1.0 1.0 point     % white
light /sun

0.2 0.2 0.2 point     % dim ambient light
[ sun ] 		      
s p union
1 		      % depth
90.0		      % FOV	
256		      % width
256		      % height
"result.ppm"	      % filename
render
