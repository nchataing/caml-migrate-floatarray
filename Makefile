##################################################################
# Copyright (C) 2020-2021 Nicolas Chataing. All rights reserved. #
#                                                                #
# This software may be modified and distributed under the terms  #
# of the BSD license.  See the LICENSE file for details.         #
##################################################################

all: clean
	dune build src/cmd.exe
	cp _build/default/src/cmd.exe refactor

format:
	dune build @fmt --auto-promote | true

clean:
	rm -f refactor
