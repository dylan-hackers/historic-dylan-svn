library: gtk-hello-world
executable: gtk-hello-world
entry-point: gtk-hello-world:%main
unit-prefix: gtk_hello_world
linker-options: `gtk-config --libs`

gtk-hello-world-exports.dylan
gtk-intr.dylan
gtk-hello-world.dylan
