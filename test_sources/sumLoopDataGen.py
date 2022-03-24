import struct
import sys

buf = bytes()

buf += struct.pack("<QQQQQ", 0, 0, 1, 10, 0x30)
sys.stdout.buffer.write(buf)
