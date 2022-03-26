long sumLoop() {
  long start = * (long *) 0x10;
  long end = * (long *) 0x18;
  volatile long *out = * (volatile long **) 0x20;

  *out = 0x434200004100;
  for(long i = start; i < end; i++) {
    *out += i;
  }

  return *out;
}
