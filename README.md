# BitcoinAnalysis

** Installation

Dependency: Haskell's Stack

In any directory of this repository run:
  stack install


** Applying the tool

The executable (BitcoinAnalysis.exe) is in the set installation directory of the installed Stack tool

Call BitcoinAnalysis.exe, with the output script in stdin, and optionally passing some arguments (call the tool with first argument '--help' for information regarding arguments)

For example, if file scriptA contains an output script (in ByteString format, the same format as these occur in the Blockchain itself), run the following in Bash: BitcoinAnalysis.exe < scriptA

The same but with more verbose output: BitcoinAnalysis.exe 2 < scriptA


Some example output scripts can be found in folder scripts/
