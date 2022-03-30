entry:
  r10 = 0x1000
  goto subfunc
entry.1:
  exit

subfunc:
  r0 = r10
  goto entry.1
