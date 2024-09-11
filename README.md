# Radiocarbon Pipeline

This repository is the Pipeline at work at [radiocarbon.ci.dev](radiocarbon.ci.dev).

## Building

```bash
opam install . --deps-only
dune build
```

## Running

Set the following:
  - Your GitHub username at `./env/account.txt`
  - The input/output repo's SSH endpoint at `./env/repo.txt`
  - The GitHub app ID at `./env/gappid.txt`
  - The Installation ID of that app at `./env/ginstid.txt`
  - The GitHub app's secret key at `./env/sk.pem`
  - The GitHub app's secret passkey at `./env/wsk.txt`
  - A random salt at `./env/salt`: `openssl rand -hex 32 > ./env/salt`
  - A GitHub SSH RSA key in the form that Awa accepts at `./env/github_sk.pem`

Then simply:
```bash
sh run.sh
```
