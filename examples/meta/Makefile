FILES := library \
	 meta-base \
	 with-collector \
	 meta-types \
	 meta-syntax \
	 meta

libmeta.a: $(FILES:.dylan)
	d2c meta.lid

clean:
	rm *.o *.c *~ cc-*.mak *.a *.du

