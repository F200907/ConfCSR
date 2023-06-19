# ConfCSR

A confluence tool for context-sensitive rewriting

## Compilation

In order to build `confcsr` [cabal](https://www.haskell.org/cabal/) is required.

Assuming the following file structure:

```text
./confcsr-X.x.y.z
| -- app/
| -- src/
| -- test/
| -- README.md
| -- confcsr.cabal
```

Run the following commands to build `confcsr` in `./confcsr-X.x.y.z/bin`:

```bash
mkdir bin
cabal install --installdir=bin
```

## Usage

`ConfCSR` operates on TRSs in the [CSTRS format](http://project-coco.uibk.ac.at/problems/cstrs.php) defined on [CoCo](http://project-coco.uibk.ac.at/).

The following table shows the primary flags.

| Full Flag | Flag | Default | Description |
| --- | --- | --- | --- |
| `--brief` | `-b` | False | Only print the first line.
| `--canonical` | `-c` | False | Use canonical μ-replacement map.
| `--depth` | `-d` | 15 | Search depth used in joinability.
| `--termination` | | | Command to invoke termination tool.
| `--tpdb` | | True | Check termination in TPDB format.

For a full list of flags use `./confcsr --help`.

Here is a list of example invocations:

```bash
./confcsr --help
./confcsr --depth=20 -b problem.trs
./confcsr --termination="./aprove.sh" problem.trs
./confcsr --brief -c --termination="./ttt2" --tpdb=false problem.trs
```
