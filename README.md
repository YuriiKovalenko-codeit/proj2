Implementation of a client and a server for a sample API (see `src/API.hs`).

Uses port 8081.

There are currently 3 modes:
- `server` mode, requires a config file and a key file.
- `client` mode, requires `--username LOGIN --password PASS` command-line options.
- `keygen` mode to create a key file.

To build a docker container with `bin/Dockerfile` please put the output binary to the `bin/` folder (e.g. with `stack install --local-bin-path=bin` command).