#
# Build program using GNAT gprbuild.
#
# Copyright 2023 by Martin Moene
#
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

PROGRAM = scrape_electricity_per_day_vattenfall
PROJECT = ${PROGRAM}.gpr

MODULES = pair.ads pair.adb scoped.ads scoped.adb

VPATH = sdb:../src exe:../bin

all: ${PROGRAM}.exe

${PROGRAM}.exe: ${PROGRAM}.adb ${MODULES}
	gprbuild -d -P${PROJECT} $<
