#!/bin/bash

if [ $# != 1 ]; then
    echo "Usage: `basename $0` relative-path-to-koala"
    echo "        The registry will be created under the current directory."
    exit 2
fi

koala_dir=$1

mkdir -p registry/Sources/registry/x86-linux
cd registry/Sources/
echo "abstract://dylan/../../$koala_dir/lib/anaphora/anaphora.lid" > registry/x86-linux/anaphora
echo "abstract://dylan/../../$koala_dir/lib/meta/meta.lid" > registry/x86-linux/meta
echo "abstract://dylan/../../$koala_dir/lib/multimap/multimap.lid" > registry/x86-linux/multimap
echo "abstract://dylan/../../$koala_dir/lib/xml-parser/xml-parser.lid" > registry/x86-linux/xml-parser
echo "abstract://dylan/../../$koala_dir/sources/xml-rpc-common/xml-rpc-common.hdp" > registry/x86-linux/xml-rpc-common
echo "abstract://dylan/../../$koala_dir/sources/dylan-basics/dylan-basics.hdp" > registry/x86-linux/dylan-basics
echo "abstract://dylan/../../$koala_dir/sources/koala/koala-unix.hdp" > registry/x86-linux/koala
echo "abstract://dylan/../../$koala_dir/sources/koala-app/koala-app.hdp" > registry/x86-linux/koala-app
echo "abstract://dylan/../../$koala_dir/sources/examples/koala-basics/koala-basics.hdp" > registry/x86-linux/koala-basics
echo "abstract://dylan/../../$koala_dir/sources/examples/web-objects/web-objects.hdp" > registry/x86-linux/web-objects
