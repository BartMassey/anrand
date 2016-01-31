# anrand: Analysis tool for a Hardware Random Number Generator
Copyright (c) 2016 Bart Massey

This tool is currently customized for analyzing the raw
random bits produced by our
[AltusMetrum](http://altusmetrum.org)
[ChaosKey](http://altusmetrum.org/USBtrng) hardware RNG
(formerly known as USBTRNG). The bits come out of the
ChaosKey as 16-bit little-endian samples with 12 bits of
possible range.

This code directly reads a file of these raw samples,
and computes a number of graphs and metrics that are
intended to verify the operation of the device. (In actual
production use, the samples are whitened to 8 bits using a
CRC, yielding -- as far as we can currently tell -- perfect
hardware entropy.)

## Build and Run

This is a pile of Haskell code. You will need to install GHC
and `cabal-install` on your platform, and figure out how to
operate it. The `anrand.cabal` file has a list of packages
you must install from Hackage. The code is intended to be
platform-independent, but I've only ever tried it on a
Debian Linux box.

Run the program with your unwhitened random bits on standard
input. The program will create an `analysis` subdirectory
with a bunch of text and PDF files in it.

The file prefixes are:

* `raw`: The unmodified samples.
* `low`: Just the bottom eight bits of each sample.
* `mid`: Bits 8..1 of the sample, because the ChaosKey has a
   problem with the bottom bit or two.
* `prng`: All the statistics are also run against a PRNG so
   that you can see what white bits should look like.

The file suffixes are:

* `stats`: Some summary statistics about the sample.
* `hist`: Sample histograms.
* `ts`: The initial time-series data.
* `dft`: Magnitude of DFT of a subsample of the data.

## Adapt

You can pretty easily hack up the input reader to support
other random sources of bits. You can also pretty easily add
analysis to taste. See the Haskell source for details.

## License

This work is made available under the "MIT License".  Please
see the file LICENSE in this distribution for license
details.
