import re,sys,subprocess
from pathlib import Path
source=Path(sys.argv[1]); target=Path(sys.argv[2]); s=source.read_text()
transfer=len(sys.argv)>3 and sys.argv[3]=='transfer'
callees=set(); calls=0
pattern=re.compile(r'    jmp (?!\.)(\w+)\n(\.L\w+:\n)')
def replace(m):
 global calls
 callees.add(m[1]); calls+=1
 return f'    call {m[1]}\n'+m[2]
s=pattern.sub(replace,s)
s,indirect=re.subn(r'    jmpq (\*%\w+)\n(\.L(?:ret_|closure_drop_return_)\w+:\n)',r'    callq \1\n\2',s)
entries=0
def prolog(m):
 global entries
 name=m[1]
 if name.startswith('fn_') or name in callees:
  entries+=1
  return m[0]+('    popq -8(%r14)\n' if transfer else '    leaq 8(%rsp), %rsp\n')
 return m[0]
s=re.sub(r'^(\w+):\n',prolog,s,flags=re.M)
s,returns=re.subn(r'    jmpq \*\(%r14\)\n','    pushq (%r14)\n    ret\n',s)
left=re.findall(r'    jmpq? (fn_\w+|\*%\w+)\n',s)
assert not left,left[:5]
if transfer:
 s,removed=re.subn(r'    leaq (\.L\w+)\(%rip\), %(rax|rcx)\n    movq %\2, -8\(%r14\)\n','',s)
 assert removed==calls+indirect,(removed,calls,indirect)
 print('Transferred return addresses:',removed,flush=True)
target.with_suffix('.s').write_text(s)
print({'calls':calls,'indirect':indirect,'entries':entries,'returns':returns,'helpers':sorted(n for n in callees if not n.startswith('fn_'))},flush=True)
subprocess.run(['as',str(target.with_suffix('.s')),'-o',str(target.with_suffix('.o'))],check=True)
subprocess.run(['cc','-nostdlib','-no-pie','-Wl,-e,_start','-Wl,-z,noexecstack','-o',str(target),str(target.with_suffix('.o'))],check=True)
