#!/bin/sh
rm -rf submit
mkdir submit
cd submit
cvs -z3 -d :pserver:anonymous@cvs.gwydiondylan.org:/var/lib/cvs co examples/ICFP2004

mkdir Dylantz
cp examples/ICFP2004/README Dylantz
cp examples/ICFP2004/goodOnes/combo2.ant Dylantz/solution-1.ant
cp examples/ICFP2004/goodOnes/andreas.ant Dylantz/solution-2.ant

mv examples/ICFP2004/doc Dylantz
rm -rf examples/ICFP2004/worlds
find examples -name CVS | xargs rm -rf
mv examples/ICFP2004 Dylantz/tools
tar zcf Dylantz.tar.gz Dylantz
