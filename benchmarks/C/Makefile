#---------------------------------------------------------------------------
# FILE    : Makefile
# SUBJECT : The build control file for the C benchmark tests.
# AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
#
# The C benchmarks use the OpenSSL crypto library which is not part of GNAT
# GPL 2011. Thus my assumption is that you will be using a "third party" gcc
# to execute these tests. This make file can be used to build the program with
# that third party compiler.
#
# Please send comments or bug reports to
#
# Peter C. Chapin <PChapin@vtc.vsc.edu>
#---------------------------------------------------------------------------

benchmarks:	benchmarks.c
	gcc -O2 -o benchmarks benchmarks.c -lcrypto
