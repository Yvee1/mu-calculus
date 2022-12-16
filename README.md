# mu-calculus
Building the code is done using the [Stack](https://docs.haskellstack.org/en/stable/) build system.
Run the following command in the source directory to build the code (this may take a while).
```
stack build
```
The application can then be run using the following command.
```
stack exec mu-calculus-exe
```
Command-line options should be specified after `--`.
```
stack exec mu-calculus-exe "dining/dining_9.aut" "dining/invariantly_inevitably_eat.mcf" -- -n
```
For example, the above command runs the naive model checking algorithm on the specified LTS for the given mu-calculus formula.
