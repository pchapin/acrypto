Ada Cryptographic Objects
=========================

This repository contains the source code for Ada Cryptographic Objects (ACO), an Ada library of
crypto primitives distributed under the terms of the Lesser GNU Public License.

ACO provides both high level interfaces to cryptographic services using the features of Ada
2012, and low level implementations of those services written in SPARK 2014. The correctness and
security of the implementation is a high priority. The use of SPARK is intended to help prompt
that goal.

It is my intention to eventually support various high level cryptographic services in ACO such
as OpenPGP, X.509 certificates, SSL, and similar things. Obviously this is a very ambitious
plan.

To build ACO and all related programs, install GNAT GPL 2015. There are two GNAT project files.
The project master.gpr builds the library and the test programs (both functional tests and
benchmark tests). The project src/aco.gpr builds the library by itself (this is the only project
file needed if you want to use the library in your own applications). There are also project
files in the tools and applications folder that control the build of those executables.

SPARK is not needed to build and use ACO or its associated programs. However, if you wish to
develop ACO or check the SPARK proofs that come with ACO you should install SPARK GPL 2015.

ACO is developed primarly on Windows but it is intended to support both Windows and Linux.

Peter C. Chapin  
PChapin@vtc.vsc.edu
