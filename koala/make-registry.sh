#!/bin/bash

if [ $# != 1 ]; then
    echo "Usage: `basename $0` relative-path-to-koala"
    echo "        The registry will be created under the current directory."
    exit 2
fi

koala_dir=$1

mkdir -p registry/Sources/registry/generic
cd registry/Sources/
echo "abstract://dylan/../../$koala_dir/../anaphora/anaphora.lid" > registry/generic/anaphora
echo "abstract://dylan/../../$koala_dir/../meta/meta.lid" > registry/generic/meta
echo "abstract://dylan/../../$koala_dir/../xml-parser/xml-parser.lid" > registry/generic/xml-parser
echo "abstract://dylan/../../$koala_dir/sources/xml-rpc-common/xml-rpc-common.lid" > registry/generic/xml-rpc-common
echo "abstract://dylan/../../$koala_dir/sources/dylan-basics/dylan-basics.lid" > registry/generic/dylan-basics
echo "abstract://dylan/../../$koala_dir/sources/koala/koala-unix.lid" > registry/generic/koala
echo "abstract://dylan/../../$koala_dir/sources/koala-app/koala-app.lid" > registry/generic/koala-app
