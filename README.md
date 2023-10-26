
# `find-cycles` - analyse dependency cycles in Haskell source code

## Build

`nix build`

## Usage

### Generate `.hie` files

Use the GHC option `-fwrite-ide-info`

For more information on generating `.hie` files, see [this GHC blog post](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html)

### Process `.hie` files with `find-cycles`

See `find-cycles --help`

### View generated `.html` files

e.g. with `firefox`

No web server needed.

## Troubleshooting

### "hie file versions don't match" error

`find-cycles` must be compiled with the same GHC version that generated the `.hie` files.

You can achieve this by changing the version of GHC in the`flake.nix` file, and rebuilding `find-cycles`

N.B. this might result in a slow build time for some GHC versions (e.g. `ghc927`) which do not have pre-built packages cached online.

For more information on the backwards-(in)compatability of `.hie` files, see [this GHC ticket](https://gitlab.haskell.org/ghc/ghc/-/issues/18329)
