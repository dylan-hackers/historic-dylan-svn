module: dylan-user

define library ant-visualizer
    use dylan;
    use streams;
    use format;
    use print;
    use standard-io;
    use garbage-collection;
    use transcendental;
    use opengl;
end library;

define module ant-visualizer
    use dylan;
    use extensions;
    use system;
    use streams;
    use format;
    use print;
    use standard-io;
    use transcendental;
    use garbage-collection;

    use opengl;
    use opengl-glu;
    use opengl-glut;
end module;
