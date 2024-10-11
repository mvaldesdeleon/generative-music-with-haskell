# Generative Music with Haskell

## Getting started

### Euterpea2

This workshop uses the Euterpea2 library. As of the date of the workshop, [Hackage](https://hackage.haskell.org/package/Euterpea-2.0.7) is currently hosting version 2.0.7, whereas version 2.0.8 can be fetched from [Github](https://github.com/Euterpea/Euterpea2).

#### What I did

```sh
stack new music-workshop --snapshot lts-21.25
```

Add `Euterpea` to the `package.yaml` dependencies section.

Add extra-deps to `stack.yaml`
```
extra-deps:
- Euterpea-2.0.7@sha256:a885d2f3e82680284a85822d1f1b0e39ae44afbd50e61bf92e38ddbc7694b3b9,2683
- PortMidi-0.2.0.0@sha256:0671e36ec72e95138bf396234b205864a8a6d0ee353e09e01cbfd57004c56f40,2383
```

Or you just follow the errors from `stack build`.

A global install should also work

```sh
cabal v1-install --allow-newer Euterpea
```

## Challenges

Go to the [Challenges.hs](src/MuniHac/Challenges.hs) file to get started.
Within this file you'll find suggestions for different audio modules to implement.
At the bottom you'll find suggestions on how to combine these modules to synthesize different sounds.

You do not need to implement all the modules, so I recommend coming up with a plan as to which kind of sounds you'd like to synthesize first, then working on the modules you need for those sounds.

## Solutions

Ultimately, I want you to have fun and experiment making sounds. If you need a specific module but don't want to implement it, or if you're curious to compare implementations, you can find the solutions alongside all my test code in the `experimental` branch.