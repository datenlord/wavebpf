# wBPF

WaveBPF (wBPF) is a "tightly-coupled multi-core" eBPF CPU, designed to be a high-throughput coprocessor for processing in-memory data (e.g. network packets).

This repository contains the hardware. Other resources:

- [wBPF Linux driver](https://github.com/losfair/wbpf-driver)
- [Rust library and userspace utilities](https://github.com/losfair/wbpf-userspace)

## Features

- Supports all [eBPF instructions](https://github.com/iovisor/bpf-docs/blob/master/eBPF.md).
- AXI4-compatible peripheral interface.
- Exception delegation to AXI4 master (so you can handle eBPF helper calls, etc. in a separate "cold-path" software logic).

## Try it

Prebuilt Zynq-7020 (xc7z020clg400-2) BOOT.BIN including the bitstream and kernel is available [here](https://github.com/losfair/wbpf-zynq-build/actions). Grab it and build the [driver](https://github.com/losfair/wbpf-driver) yourself.

## Build

See the [GitHub action workflow](https://github.com/datenlord/wavebpf/blob/main/.github/workflows/ci.yml) for instructions on generating Verilog from the design. The ports are:

- `mmio` (AxiLite4, control port)
- `dataMemAxi4` (Axi4, tightly-coupled data memory)
- `excInterrupt` (Interrupt line for exception delegation)
