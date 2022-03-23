import struct
import sys

buf = bytes()

buf += struct.pack("<QQQQQQ", 0, 0, 0x60, 0x05, 0x70, 0x03)
buf += struct.pack("<QQQQQQ", 0, 0, 0, 0, 0, 0)
buf += struct.pack("<BBBBBBBBQ", 0x01, 0x02, 0x03, 0x04, 0x05, 0, 0, 0, 0)
buf += struct.pack("<BBB", 0x42, 0x43, 0x44)

sys.stdout.buffer.write(buf)
