# `nifty`
## Nick's Interactive Fiction with Types

This is the first commit. I have big plans, but it's very early days yet. Overview of what we have so far...

`lib/full_compile_intf.ml`
- An early plan for what a typed-compilation interface might look like (might be a bit out of date)

`lib/compile_intf.ml`
- A cut down typed-compilation interface, with just enough functionality to support...

`lib/game0.ml`
- The most trivial 2-room game with 3 commands.

`lib/compile.mli`  
`lib/compile.ml`
- That fraction of the typed-compilation interface implemented so far.

`lib/assemble.mli`  
`lib/assemble.ml`
- Partial z-code assembler, as needed by compile.ml

`lib/text.mli`  
`lib/text.ml`
- Encodes z-machine strings. Used when assembling.

`lib/emit.mli`  
`lib/emit.ml`
- The lovely `Emit.t` monad used by `assemble.ml` and `text.ml`
- Already in its 3rd incarnation.
    1. tried applicative style... a maze of twisty passages that went nowhere.
    2. monad with `allocate_space`/`backpatch`... worked in a fashion; bit hacky; and not as general as...
    3. the current monad which supports the `reverse_bind` operator!

The `reverse_bind` operator is very like `bind` (`>>=`) except that earlier-code gets to have a data-dependency on later-code. This allows, among other things:
1. assembling the 64 byte header which contains pointers to later sections in the z-image-file: the dictionary, globals, object_table, init_pc etc.
2. assembling the static text-strings section after the code-section which references the strings.
3. assembling all the object property tables after the objects with refernce them
4. assembling branch forward instructions

`play_assemble.ml`
- Work in progress to figure out how to emit z-code for an assembly language with branches/jumps both forward and backwards. This will make critical use of the Emit.reverse_bind operator.


To build, first checkout my `niz` project as a subdir: 
```
git clone https://github.com/Nick-Chapman/niz.git
```

And then run `make`
(Make requires jbuilder - for full instruction on this see the README in niz.)
which will generate `stuff.exe` (`./_build/default/images/from_assembler/stuff.exe`)
Running `stuff.exe` will generate `stuff.z3` - a tiny z-file which should play on any z-machine interpreter (although not the Android app TextFiction, for reasons yet to be discovered).
