Ada Cryptographic Objects
=========================

This repository contains the source code for Ada Cryptographic Objects (ACO), an Ada library of
crypto primitives distributed under the terms of the Apache 2.0 license.

ACO provides both high level interfaces to cryptographic services using the features of Ada
2012, and low level implementations of those services written in SPARK. The correctness and
security of the implementation is a high priority. The use of SPARK is intended to support
that goal.

It is my intention to eventually support various high level cryptographic services in ACO such
as OpenPGP, X.509 certificates, SSL, and similar things. Obviously this is a very ambitious
plan.

To build ACO and all related programs, install [Alire](https://alire.ada.dev/). There are
three crates defined in this repository. Running `alr build` in the top level folder will
build the library itself. Running `alr build` in `tests` and again in `benchmarks` will build
the test program and the benchmark program, respectively.

SPARK is not needed to build and use ACO or its associated programs. However, if you wish to
develop ACO or check the SPARK proofs that come with ACO you should install SPARK. Alire
can do this for you.

ACO is developed primarly on Windows but it is intended to support both Windows, Linux, and
macOS.

Peter Chapin  
spicacality@kelseymountain.org  
