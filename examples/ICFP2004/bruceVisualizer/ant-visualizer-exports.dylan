module: dylan-user

define library ant-visualizer
    use common-dylan;
    use garbage-collection;
    use transcendental;
    use melange-support;
    use opengl;
    use ants;
end library;

define module ant-visualizer
    use common-dylan;
    use transcendental;
    use garbage-collection;
    use melange-support;

    use opengl;
    use opengl-glu;
    use opengl-glut;
    use ants;
end module;
