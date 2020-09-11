# BrainFuckHaskell

BrainFuck Interpreter implemented in Haskell.
See description of the brainfuck language here: https://en.wikipedia.org/wiki/Brainfuck.
The repository has the following structure:

```
|src/
    └ Lib.hs (1.)
```

**(1.)** The **src/Lib.hs** module contains the brainfuck interpreter sourcecode.
To test it, import the module to the GHCI, run the **translateBrainFuck** function
(**type signature**: translateBrainFuck :: [Char] -> BfProgramExecution) with a string 
that represents brainfuck code.

A program that prints "Hello World!" in brainfuck looks like this:

```
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.
```

So you just need to call the **translateBrainFuck** function with the above brainfuck code 
(represented as a string/list of characters)
