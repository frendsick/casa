import re,sys,collections,subprocess
from pathlib import Path
source=Path(sys.argv[1]); cutoff=int(sys.argv[2]); target=Path(sys.argv[3])
counts=collections.Counter()
pattern=re.compile(r'    movq \$(\d+), %rcx\n    xorq %rax, %rax\n    rep stosq\n')
def replace(m):
 n=int(m[1])
 if n>cutoff: return m[0]
 assert f'    addq ${n*8}, %r14\n' in source_text[max(0,m.start()-250):m.start()]
 counts[n]+=1
 return '    xorq %rax, %rax\n'+''.join(f'    movq %rax, {i*8}(%rdi)\n' for i in range(n))+f'    leaq {n*8}(%rdi), %rdi\n    movq $0, %rcx\n'
source_text=source.read_text(); output=pattern.sub(replace,source_text)
target.with_suffix('.s').write_text(output)
subprocess.run(['as',str(target.with_suffix('.s')),'-o',str(target.with_suffix('.o'))],check=True)
subprocess.run(['cc','-nostdlib','-no-pie','-Wl,-e,_start','-Wl,-z,noexecstack','-o',str(target),str(target.with_suffix('.o'))],check=True)
print(cutoff,dict(sorted(counts.items())))
