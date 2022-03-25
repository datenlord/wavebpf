static long (*getCoreIndex)() = (void *)1;

void indirectSort()
{
  long coreOffset = getCoreIndex() << 3;
  long stackAddr = 0x2000 - coreOffset * 0x100;
  asm volatile("r10 = %0" : : "r"(stackAddr));

  long len = *(long *)0x08;

  long *indices = *(long **)(0x10 + (coreOffset << 1));
  long *values = *(long **)(0x18 + (coreOffset << 1));

  long i, j;
  for (long i = 0; i < len - 1; i++)
    for (long j = 0; j < len - i - 1; j++)
      if (values[indices[j]] > values[indices[j + 1]])
      {
        long tmp = indices[j];
        indices[j] = indices[j + 1];
        indices[j + 1] = tmp;
      }
}
