#!/bin/bash

if [ $# != 1 ]; then
    echo "Usage: `basename $0` relative-path-to-dylan-checkout"
    echo "        The registry will be created under the current directory."
    exit 2
fi

dylan_root=$1


mkdir -p registry/Sources/registry/generic
cd registry/Sources/
echo "abstract://dylan/../../$dylan_root/libraries/anaphora/anaphora.lid" > registry/generic/anaphora
echo "abstract://dylan/../../$dylan_root/libraries/meta/meta.lid" > registry/generic/meta
echo "abstract://dylan/../../$dylan_root/libraries/xml-parser/xml-parser.lid" > registry/generic/xml-parser
echo "abstract://dylan/../../$dylan_root/libraries/koala/sources/xml-rpc-common/xml-rpc-common.lid" > registry/generic/xml-rpc-common
echo "abstract://dylan/../../$dylan_root/libraries/koala/sources/dylan-basics/dylan-basics.lid" > registry/generic/dylan-basics
echo "abstract://dylan/../../$dylan_root/libraries/koala/sources/koala/koala-unix.lid" > registry/generic/koala
echo "abstract://dylan/../../$dylan_root/libraries/koala/sources/koala-app/koala-app.lid" > registry/generic/koala-app
echo "abstract://dylan/../../$dylan_root/libraries/koala/sources/examples/koala-basics/koala-basics.lid" > registry/generic/koala-basics
echo "abstract://dylan/../../$dylan_root/libraries/koala/sources/examples/wiki/wiki.lid" > registry/generic/wiki
echo "abstract://dylan/../../$dylan_root/fundev/Sources/lib/regular-expressions/regular-expressions.lid" > registry/generic/regular-expressions
echo "abstract://dylan/../../$dylan_root/fundev/Sources/lib/string-extensions/string-extensions.lid" > registry/generic/string-extensions
