# rosetta-code-examples
[Rosetta Code](rosettacode.org) Examples in [Racket](racket-lang.org)

This repository contains Racket implementations of the Rosetta Code examples I have contributed.

Things to note, however:
* there might be some divergence between the code found here, and that on RC
* each task will find itself in a separate directory, with a `README.md`.
  The `README.md` file will possibly only contain a link to the RC task, so don't expect miracles

Unless the task is a _golf_ task (or there is some other requirement), the examples should fit with the [Racket Style Guide](https://docs.racket-lang.org/style/index.html).

## Licencing
See `LICENSE` about the licence for this code, and contrast it to the licence for RC. If you want to contribute to this repo, please do... but it needs to be your own work so you can grant the more liberal licence.

I'm not a lawyer, etc. so this is all done in good faith please.

## Example/data files

I'd rather not have copies of "AliceInWonderland.txt" distributed around this repo for every _Count the Words_ style task. Example data will be kept in the `data`. But I might prefer a download script or URL and have downloads put into `data-tmp`. :unicorn: We'll put together a script to do that downloading automatically.

Regards permissions - RC is pretty good with using public domain (or otherwise freely available) materials for its examples. But please check.

## Image files

Several RC tasks require images to be generated. Create an `images` directory in the task and link to that if needs be.
(I've always been very unlucky with images and RC's wikimedia implementation, but that could just be me).
