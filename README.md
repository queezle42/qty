# qty

Experimental software for routing shell output to different terminals (e.g. to
continue working on another host).

## Development tools

Build:
`nix build -L`

Watch for changes (significantly faster that a full build):
`./ghcid`

Coverage:
`nix develop -c cabal test --enable-coverage`
