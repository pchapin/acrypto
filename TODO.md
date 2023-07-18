
## TODO Items

The items in this file are in no particular order. They are grouped by area.


# Crypto

+ Submit my Blowfish implementation to Schneier for possible inclusion on his Blowfish
  implementation page. That might be a nice way to get Ada's name in front of some more
  programmers.


# Math

+ Improve the benchmark tests for the Very_Long type. Look into profiling the execution with an
  eye toward improving that performance. This type is fundamental to the public key algorithms
  so good efficiency is important.


# Benchmarks

+ Consider creating a table on the web site of ACO performance relative to OpenSSL.


# Build System

+ Convert the tools build to being Alire based.

+ FIgure out how incorporate SPARK in the Alire build system so that SPARK analysis can be done.


# Documentation

+ Improve the build documentation (which is now based on Alire). Also describe how to run the
  unit tests.

+ Right now package ACO.Math.Very_Longs is documented using a <variablelist>. Should the
  other packages be documented in a similar way? In any event the method used should be
  consistent across all packages.
