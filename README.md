# SCRIPT Analyser
## Symbolic verification of Bitcoin's output scripts

###### Installation

Dependency: Haskell's Stack (https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Dependency: swi-prolog (http://www.swi-prolog.org/Download.html)

In the root directory of this repository run:
  stack install


###### Applying the tool

The executable (SCRIPTAnalyser-exe) can be executed in any directory of this repository through Stack as follows: stack exec SCRIPTAnalyser-exe -- _arguments_

Call SCRIPTAnalyser-exe, with the output script in stdin, and optionally passing some arguments (call the tool with first argument 'help' for information regarding arguments)

For example, if file scriptA contains an output script (in ByteString format, the same format as SCRIPTs occur in the Blockchain itself), run the following in Bash: stack exec SCRIPTAnalyser-exe -- < scriptA

The same but with more verbose output: stack exec SCRIPTAnalyser-exe -- 2 < scriptA


Some example output scripts can be found in folder scripts/


###### Understanding SCRIPTAnalyser-exe's output

(Note: this only describes the default verbosity output mode)

For every unique branch of the supplied output SCRIPT, the tool will print a verdict under a distinct "***** Gamma Solution for branch *****" section

In this section, the following information is printed:

- The respective branch's decision points are shown (i.e. for every encountered IF operation, is its True branch traversed or is its False branch traversed?)
- The symbolic stack that the input script must generate is printed.
- The inferred constraints (on these supplied variables).
- The final resulting symbolic stack (note, at this point we have already performed an additional OP_VERIFY, thus the constraint that the resulting full execution of this branch must end with a true value on the stack is already in the inferred constraints set)
