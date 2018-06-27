# BitcoinAnalysis

** Installation

Dependency: Haskell's Stack

In any directory of this repository run:
  stack install


** Applying the tool

The executable (BitcoinAnalysis.exe) is in the set installation directory of the installed Stack tool

Call BitcoinAnalysis.exe, with the output script in stdin, and optionally passing some arguments (see below for which arguments are available)

For example, if file scriptA contains an output script (in ByteString format, the same format as these occur in the Blockchain itself), run the following in Bash: BitcoinAnalysis.exe < scriptA
The same but with more verbose output: BitcoinAnalysis.exe 2 < scriptA

--

Usage:
  Arg 1 (optional): verbosity level
    0: minimal print, only prints verdicts
    1: Verbose prints, additionally prints inferred (default) constraints
    2: _More_ verbose prints, additionally prints inferred types of expressions, as well as a trace of stack mutations
    >=3: Verbose prints (debugging mode), additionally prints prolog related information
  Arg 2 (optional): path for creating temporary prolog code file (default is /tmp/)
  Arg 3 (optional): string to prepend the verdict line (useful to track metadata through large batch computations)
