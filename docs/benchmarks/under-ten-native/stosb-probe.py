import re,sys,subprocess
from pathlib import Path
source=Path(sys.argv[1]);target=Path(sys.argv[2]);s=source.read_text()
p=re.compile(r'    movq \$(\d+), %rcx\n    xorq %rax, %rax\n    rep stosq\n')
def change(m):
 n=int(m[1]);assert f'    addq ${n*8}, %r14\n' in s[max(0,m.start()-250):m.start()]
 return f'    movq ${n*8}, %rcx\n    xorq %rax, %rax\n    rep stosb\n'
t,n=p.subn(change,s);target.with_suffix('.s').write_text(t);print('Changed frames:',n,flush=True)
subprocess.run(['as',str(target.with_suffix('.s')),'-o',str(target.with_suffix('.o'))],check=True)
subprocess.run(['cc','-nostdlib','-no-pie','-Wl,-e,_start','-Wl,-z,noexecstack','-o',str(target),str(target.with_suffix('.o'))],check=True)
