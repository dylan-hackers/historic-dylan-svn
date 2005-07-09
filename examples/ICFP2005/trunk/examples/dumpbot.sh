#!/bin/sh

echo "reg: foobar robber"

while (true); do
  read foo
  echo $foo >> /tmp/foobar
done
