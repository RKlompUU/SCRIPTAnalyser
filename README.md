# SCRIPT Analyser
![Alt text](.imgs/outputExampleA.png?raw=true "Example")
## Symbolic verification of Bitcoin's output scripts

SCRIPT Analyser is a tool that infers from a given _output_ script the constraints that must be met by an _input_ script to create a valid transaction. Consider for example a standard P2PKH output script, the tool will report that a correct input script must:
- establish a stack containing 2 entries: X\_(0) and X\_(-1) (with X\_(0) at the head of the stack)
- X\_(0) must be a correct hash pre-image of the constant value present in the output script
- X\_(-1) must be a correct signature (signing the transaction with the private key that belongs to public key X\_(0))

The tool can analyse any partial Bitcoin script. That is, it is not limited to the analysis of the standard types of output scripts. The tool supports the entire SCRIPT language (i.e. all operations that are permitted according the Bitcoin Core implementation are supported by this tool).

Try it out online at: https://vm100.cs.stir.ac.uk/~rkl/home.html

#### Installation
Dependency: Haskell's Stack (https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Dependency: swi-prolog (http://www.swi-prolog.org/Download.html)

In the root directory of this repository run:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\# If not already installed, install Happy:<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;stack install happy<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\# Install Script Analyser and all of its dependencies (other than Happy)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;stack install


#### Applying the tool

The executable (SCRIPTAnalyser-exe) can be executed in any directory of this repository through Stack as follows: stack exec SCRIPTAnalyser-exe -- _arguments_

Call SCRIPTAnalyser-exe, with the output script in stdin, and optionally passing some arguments (call the tool with first argument 'help' for information regarding arguments)

For example, if file scriptA contains an output script (see below for syntax description, or go to https://vm100.cs.stir.ac.uk/~rkl/docu.php), run the following in Bash: stack exec SCRIPTAnalyser-exe -- < scriptA

The same but with more verbose output: stack exec SCRIPTAnalyser-exe -- 2 < scriptA


Some example output scripts can be found in folder scripts/


#### Understanding SCRIPTAnalyser-exe's output

![Alt text](.imgs/outputExample.png?raw=true "Example")

(Note: this only describes the default verbosity output mode)

For every unique branch of the supplied output script, the tool will print a verdict under a distinct "--- Symbolic evaluation report of execution branch _i_" section

In this section, the following information is printed:

- The respective branch's decision points are shown (i.e. for every encountered IF operation, is its True branch traversed or is its False branch traversed?)
- The symbolic stack that the input script must supply
- The inferred constraints (on these supplied variables)
- The final resulting symbolic stack (note, at this point we have already performed an additional OP_VERIFY, thus the constraint that the resulting full execution of this branch must end with a true value on the stack is already in the inferred constraints set)

#### Custom SCRIPT syntax

Scripts can be specified using an extended SCRIPT language (which is a superset of the bytestring SCRIPT language). Visit https://vm100.cs.stir.ac.uk/%7Erkl/docu.php, or https://github.com/RKlompUU/SCRIPTWriter, for a description of the extended language's syntax.
