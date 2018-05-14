#!/bin/sh
cat jsonify.m4 $1 | sed -E -n \
			-e "/###date/{n;s%([0-9]+)%date(dfix(\1))%}"\
			-e "s:#*::g"\
			-e "s:/proc/stat:procstat:"\
			-e "s:/proc/net/dev:procnetdev:"\
			-e "s:(cpu) (.*):cpuent0(\1,\2):"\
			-e "s:(cpu[0-9]+) (.*):cpuentn(\1,\2):"\
			-e "/^ face/,/date/ s/(.*):(.*)/interfacedata(\1,\2)/"\
			-e "p" | m4




