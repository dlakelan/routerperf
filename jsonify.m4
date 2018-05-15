divert(-1)dnl
define(`statformat',`[')
define(`nstimestart',`define(startsec,eval('$1`-1)) dnl')
define(`nstimestamp',`,
{"time" : eval($1-'startsec`).$2,
')
pushdef(`nstimestamp',`popdef(`nstimestamp') {"time" : eval($1-'startsec`).$2,
')
define(`nocomma',```dnl''')
define(`procstat',`procstatformat(nocomma $1)')

define(`procstatformat',`"cpu":[patsubst($1,`\(cpu[0-9]*\) *\([0-9 ]+\)
',`,
["\1",patsubst(\2,` +',`,')]')],
')
define(`Inter',`dnl')
define(`face',`dnl')
define(`procnetdev',`procnetformat(nocomma $1])')
define(`procnetformat',`"interfaces":[patsubst($1,` *\([a-zA-Z0-9.]+\):\([ 0-9]*\)
',`,
["\1",patsubst(\2,` +',`,')]')
}
')
define(`ctxt',`dnl')
define(`intr',`dnl')
define(btime,`dnl')
define(processes,`dnl')
define(procs_running,`dnl')
define(procs_blocked,`dnl')
define(softirq,`dnl')
define(`id',$1)
divert dnl
m4wrap(])dnl
