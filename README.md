# Radiocarbon Pipeline

This repository is the Pipeline at work at [radiocarbon.ci.dev](radiocarbon.ci.dev).

## Building

```bash
opam depext -i dune current current_web current_git
dune build
```

## Running

Set the following:
  - Your username at `./env/account.txt`
  - The GitHub app ID at `./env/gappid.txt`
  - The Installation ID of that app at `./env/ginstid.txt`
  - The GH app's secret key at `./env/sk.pem`
  - The GH app's secret passkey at `./env/wsk.txt`

Then simply:

```bash
sh run.sh
```
