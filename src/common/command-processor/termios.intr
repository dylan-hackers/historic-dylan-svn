module: command-processor

define interface
  #include { "termios.h" },
  import: {"termios", "tcgetattr", "tcsetattr", "TCSANOW"};
end interface;
