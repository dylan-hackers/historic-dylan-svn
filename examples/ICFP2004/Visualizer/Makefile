ant-visualizer: ant-visualizer.lid ant-visualizer.dylan ant-visualizer-exports.dylan
	d2c -M -L../../../libraries/libopengl-dylan -L.. ant-visualizer.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ ant-visualizer
	-rm -rf .libs

-include ant-visualizer.dep
