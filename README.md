# SCRIPT Analyser
## Symbolic verification of Bitcoin's output scripts

![Alt text](.imgs/outputExampleA.png?raw=true "Example")

SCRIPT Analyser is a tool that infers from a given _output_ script the constraints that must be met by an _input_ script to create a valid transaction. Consider for example a standard P2PkH output script, the tool will report that a correct input script must:
- establish a stack containing 2 entries: X\_0 and X\_-1 (with X\_0 at the head of the stack)
- X\_0 must be a correct hash pre-image of the constant value present in the output script
- X\_-1 must be a correct signature (signing the public key X\_0 plus the transaction)

The tool can analyse any partial Bitcoin script. That is, it is not limited to the analysis of the standard types of output scripts. However, it does not yet support all types of operations. For example, the locktime operations are not yet supported.

#### Installation

Dependency: Haskell's Stack (https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Dependency: swi-prolog (http://www.swi-prolog.org/Download.html)

In the root directory of this repository run:
  \# If not already installed, install Happy:
  stack install happy
  \# Install Script Analyser and all of its dependencies (other than Happy)
  stack install


#### Applying the tool

The executable (SCRIPTAnalyser-exe) can be executed in any directory of this repository through Stack as follows: stack exec SCRIPTAnalyser-exe -- _arguments_

Call SCRIPTAnalyser-exe, with the output script in stdin, and optionally passing some arguments (call the tool with first argument 'help' for information regarding arguments)

For example, if file scriptA contains an output script (in ByteString format, the same format as SCRIPTs occur in the Blockchain itself), run the following in Bash: stack exec SCRIPTAnalyser-exe -- < scriptA

The same but with more verbose output: stack exec SCRIPTAnalyser-exe -- 2 < scriptA


Some example output scripts can be found in folder scripts/


#### Understanding SCRIPTAnalyser-exe's output

![Alt text](.imgs/outputExample.png?raw=true "Example")

(Note: this only describes the default verbosity output mode)

For every unique branch of the supplied output SCRIPT, the tool will print a verdict under a distinct "--- Symbolic evaluation report of execution branch _i_" section

In this section, the following information is printed:

- The respective branch's decision points are shown (i.e. for every encountered IF operation, is its True branch traversed or is its False branch traversed?)
- The symbolic stack that the input script must supply
- The inferred constraints (on these supplied variables)
- The final resulting symbolic stack (note, at this point we have already performed an additional OP_VERIFY, thus the constraint that the resulting full execution of this branch must end with a true value on the stack is already in the inferred constraints set)
