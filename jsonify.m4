divert(-1)dnl
define(`delfirstbrack',`define(`delfirstbrack',`')dnl')
define(`delcomma',`')
pushdef(`delcomma',`dnl')
define(`cpuent0',["$1"`,translit('$2`,` ',`,')]')
define(`cpuentn',`,'["$1"`,translit('$2`,` ',`,')]')
define(`date',`delfirstbrack]}
delcomma,
{
"time" : $1 popdef(`delcomma')')
define(`procstat',`,
"cpuvals" :[')

define(`ctxt',`] dnl')
define(`intr',`dnl')
define(btime,`dnl')
define(processes,`dnl')
define(procs_running,`dnl')
define(procs_blocked,`dnl')
define(softirq,`dnl')
define(procnetdev,`,
"interfaces" :[pushdef(`delcomma',`dnl')dnl')
define(Inter,`
dnl')
define(face,`dnl')

dnl handle fixing the date by doing arithmetic on the first 6 digits to reduce sigfigs
define(`dfix',`sub6(substr($1,0,6))substr($1,6)')')
define(`stripzero',`ifelse(0,$1,`',$1)')
define(`sub6',`ifdef(`zeropoint',`stripzero(eval('$1 - zeropoint`))',`define(`zeropoint','$1`)')')

define(interfacedata,`delcomma,
define(`delcomma',`')dnl
[patsubst("'$1`",` +',`')`,' patsubst('$2`,` +',`,')]')
divert dnl
m4wrap(]}])dnl
[
