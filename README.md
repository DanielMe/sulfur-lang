
# Design Goals

SulfurLang is an experimental language that explores a particular
niche of the programming language design space.

The following goals 

   - Usability comes first
      - Documentation is a first class citizen
        - It must be painless and encouraged to write documentation
        - Documentation should support examples which should be type
          checked and tested.
      - Good tooling support
      - Good error messages
        - Should explain the error in simple terms
        - Should give indications about common causes
        - Should print an ascii art representation of the offending bit of code
        - Should propose a fix if feasible
      - Clear understandable unambiguous syntax
      - Batteries included standard library should be available
   - Type safety comes second
      - If type inference is not possible automatically that is okay as 
        long as we can provide sensible messages explaining the problem
      - Ability to reason about time, parallelism and effects in terms 
        of types
      - (limited) support for dependent types 
        (at least vector length, bounds of numbers)
      - It should be possible to express invariants about the program
        by using traits. These invariants can then either be checked at
        runtime or at compile time.
      - However not at the expense of usability
   - Performance comes third
      - Packed / unpacked representation should be controllable
      - Lazy/ strict should be controllable
      - Efficient default implementations for common data structures (vector, string)
      - However not at the expense of type safety and usability
   - Brevity comes fourth
      - it is desirable to not be too verbose
      - however not at the expense of any of the above goals

# Syntax

Everything can be structured in definitions. Every definition has a type and a value.
Example:

```
  define my_value
     my_value : Integer
     my_value = 5
```