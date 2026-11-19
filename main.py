import json,os
from urllib.request import*
U=os.environ['MAKEFUN_ENV_URL']
V=(0,100,320,330,500,900,0)
N8=(-33,-31,-18,-14,14,18,31,33)
B8=(15,17,-15,-17)
R8=(1,-1,16,-16)
K8=R8+B8
MV=(0,0,(N8,0),(B8,1),(R8,1),(K8,1),(K8,0))
H='abcdefgh'
KM={0:13,7:14,4:12,112:7,119:11,116:3}
B=[0]*128
E=0;s=1;ep=-1;rg=0;K=[4,116];N=0;W=999999
def P(t,q):
 f=q&7;r=q>>4;c=6-((abs(2*f-7)+abs(2*r-7))//2)
 return (6*r-6+2*c,8*c,6*c,2*c+15*(r==6),3*c,10-20*r)[t-1]
def C(p,q):
 return (V[abs(p)]+P(abs(p),q^112*(p<0)))*(1,-1)[p<0]
def su(f):
 global B,E,s,ep,rg,K
 B=[0]*128;E=0
 a=f.split()
 q=112
 for ch in a[0]:
  if ch=='/':q-=24
  elif ch.isdigit():q+=int(ch)
  else:i='PNBRQKpnbrqk'.index(ch);B[q]=(i%6+1)*(1 if i<6 else-1);E+=C(B[q],q);q+=1
 K=[B.index(6),B.index(-6)]
 s=(a[1]=='w')*2-1
 rg=('K'in a[2])+2*('Q'in a[2])+4*('k'in a[2])+8*('q'in a[2])
 ep=H.index(a[3][0])+(int(a[3][1])-1)*16 if a[3]!='-' else-1
def ak(q,z):
 if any(not(q-e*z)&136 and B[q-e*z]==z for e in(15,17)):return 1
 for D,v in((N8,2),(K8,6)):
  if any(not(q+d)&136 and B[q+d]==v*z for d in D):return 1
 for D,v in((B8,(3,5)),(R8,(4,5))):
  for d in D:
   t=q+d
   while not t&136 and not B[t]:t+=d
   if not t&136 and abs(B[t]) in v and B[t]*z>0:return 1
 return 0
def gen(c):
 M=[]
 for q in range(128):
  p=B[q]
  if not p or p*s<0:continue
  t=abs(p)
  if t==1:
   d=16*s;r=q>>4;pr=r==6-5*(s<0)
   for e in(d-1,d,d+1):
    u=q+e
    if u&136:continue
    x=B[u]
    if e==d and x==0 and(pr or c<1) or e!=d and(x*s<0 or u==ep):
     if pr:
      for z in(5,4,3,2):M.append(q|(u<<7)|(z<<14))
     else:M.append(q|(u<<7))
    if e==d and x==0 and c<1 and pr<1 and r==1+5*(s<0):
     w=q+2*d
     if not B[w]:M.append(q|(w<<7))
  else:
   D,sl=MV[t]
   for d in D:
    u=q+d
    while not u&136:
     x=B[u]
     if x:
      if x*s<0:M.append(q|(u<<7))
      break
     if c<1:M.append(q|(u<<7))
     if sl<1:break
     u+=d
   if t==6 and c<1:
    for bt,k,dr in((1,4,1),(2,4,-1),(4,116,1),(8,116,-1)):
     r=k+3*dr if dr>0 else k-4;g=k+2*dr;a=k+dr;e=k+3*dr if dr<0 else-1
     if rg&bt and not(B[a] or B[g] or B[e]) and B[r]==4*s and not any(ak(x,-s) for x in(k,a,g)):M.append(k|(g<<7))
 M.sort(key=lambda m:(-V[abs(B[(m>>7)&127])],V[abs(B[m&127])]))
 return M
def mk(m):
 global E,s,ep,rg
 f=m&127;t=(m>>7)&127;z=m>>14;p=B[f];v=abs(p)
 cs=t;cp=B[t]
 if v==1 and t==ep:
  cs=t-16*s;cp=B[cs];B[cs]=0
 u=[f,t,p,cp,cs,rg,ep,E]
 E-=C(p,f)
 if cp:E-=C(cp,cs)
 B[f]=0
 n=z*p if z else p
 B[t]=n
 E+=C(n,t)
 if v==6:
  K[s<0]=t
  if abs(t-f)==2:
   rf,rt=(t+1,t-1) if t>f else(t-2,t+1);rp=B[rf]
   B[rf]=0;B[rt]=rp;E-=C(rp,rf);E+=C(rp,rt)
 rg&=KM.get(f,15)&KM.get(t,15)
 ep=f+16*s if v==1 and abs(t-f)==32 else-1
 s=-s
 return u
def un(u):
 global E,s,ep,rg
 f,t,p,cp,cs,rg,ep,E=u
 if abs(p)==6:
  K[s>0]=f
  if abs(t-f)==2:
   rf,rt=(t+1,t-1) if t>f else(t-2,t+1)
   B[rf]=B[rt];B[rt]=0
 B[f]=p;B[t]=0;B[cs]=cp
 s=-s
def lg():
 M=[]
 for m in gen(0):
  u=mk(m)
  if not ak(K[s>0],s):M.append(m)
  un(u)
 return M
def S(d,a,b):
 global N
 N+=1
 if N>11e4:raise Exception
 if d>0:M=gen(0)
 else:
  if d<-40:return s*E
  ck=ak(K[s<0],-s)
  if ck:M=gen(0)
  else:
   a=max(a,s*E)
   if a>=b:return b
   M=gen(1)
 n=0
 for m in M:
  u=mk(m)
  if not ak(K[s>0],s):
   n+=1
   v=-S(d-1,-b,-a)
   a=max(a,v)
   if a>=b:un(u);return b
  un(u)
 if n==0:return -100000-d if d>0 and ak(K[s<0],-s) or d<=0 and ck else(0 if d>0 else a)
 return a
def root(L):
 global N
 bs=L[0]
 for d in(1,2,3,4):
  try:
   N=0;bv=-W;bm=bs
   for m in L:
    u=mk(m)
    v=-S(d-1,-bv-1,-bv)
    if v>bv:v=-S(d-1,-W,W)
    un(u)
    if v>bv:bv,bm=v,m
   bs=bm;L.remove(bm);L.insert(0,bm)
  except Exception:break
 return bs
def post(p,d):
 q=Request(U+p,json.dumps(d).encode())
 return json.load(urlopen(q))
def uc(m):
 f=m&127;t=(m>>7)&127
 w=f'{H[f&7]}{(f>>4)+1}{H[t&7]}{(t>>4)+1}'
 z=m>>14
 return w+'nbrq'[z-2] if z else w
while 1:
 g=post('/game',{})
 t=g['status']
 if t<'e':break
 if t<'y':continue
 su(g['fen'])
 L=[m for m in lg() if uc(m) in g['legal_moves']]
 mv=uc(root(L)) if L else g['legal_moves'][0]
 post('/move',{'game':g['game'],'move':mv})
