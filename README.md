<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Linear Vesting](#linear-vesting)
  - [Introduction](#introduction)
  - [Documentation](#documentation)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Building and Developing](#building-and-developing)
    - [Tests](#tests)
  - [License](#license)

<!-- markdown-toc end -->

# Linear Vesting

## Introduction

Linear Vesting contract aims at providing a reliable mechanism for releasing Cardano Native Tokens gradually over a specified timeframe, with customization options to fit different requirements.

With so many projects launching on Cardano, many a times, there arises a requirement of vesting a project's own tokens to either their core team or supporters of the project. Vesting assets to beneficiary in proportion to the elapsed time, i.e. Linear Vesting is a very straightforward and common preference. While there definitely exist more complex vesting requirements, we hope that the contract here will help those looking for something simple for their use case. Or serve as a reference for those who want to build more elaborate arrangements.

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-developer-ecosystem-the-evolution/anastasia-labs-open-source-production-grade-dapps).

## Documentation

```hs
pvalidateVestingScript :: Term s (PVestingDatum :--> PVestingRedeemer :--> PScriptContext :--> PUnit)
```

The linear vesting validator is not a parameterized one. All its customization needs are fulfilled by the datum (`VestingDatum`) of the locked UTxO.

```hs
data VestingDatum = VestingDatum
  { beneficiary :: Address
  , vestingAsset :: AssetClass
  , totalVestingQty :: Integer
  , vestingPeriodStart :: Integer
  , vestingPeriodEnd :: Integer
  , firstUnlockPossibleAfter :: Integer
  , totalInstallments :: Integer
  }
```

A single UTxO at the validator address corresponds to a single vesting arrangement. There can be multiple such UTxOs present there to facilitate multiple vestings. A single vesting UTxO can however only release tokens of a single Fungible Token, specified by `vestingAsset` field. 

After `firstUnlockPossibleAfter` and before `vestingPeriodEnd`, beneficiary can claim the vested asset in proportion to the time that has passed after `vestingPeriodStart`. The remaining assets needs to be sent back to the validator address with the original datum intact otherwise the validation will fail. This spending tx requires `PartialUnlock` redeemer. 

```hs
data VestingRedeemer
  = PartialUnlock
  | FullUnlock
```

After the `vestingPeriodEnd`, beneficiary can claim the entire remaining balance at the UTxO using `FullUnlock` redeemer.

An important thing to note about the validator is that it only allows one script input to be spent in a single tx. Allowing more than one script input to be spent within it, could result in a critical vulnerability in the form of [Double Satisfaction Attack](https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/double-satisfaction.html?highlight=double#unique-outputs) by spending two vesting UTxOs with the exact datum.

## Getting Started

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org) installed on your system. Nix is used for package management and to provide a consistent development environment. If you don't have Nix installed, you can do so by running the following command:

#### Official option

[Nix](https://nixos.org/download.html)

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

#### Preferred option

[Determinate Systems](https://zero-to-nix.com/concepts/nix-installer)

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes ca-derivations
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up binary caches by adding additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
```

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target which will be used with the `nix develop --accept-flake-config` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

### Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/linear-vesting.git
```

Navigate to the repository directory:

```sh
cd linear-vesting
```

Activate the development environment with Nix:

```sh
nix develop --accept-flake-config
```
Or
```sh
make shell
```

Please be patient when building nix development environment for the first time, as it may take a very long time. Subsequent builds should be faster. Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
make build
```

Execute the test suite:

```sh
make test
```

Compile and export Plutarch scripts:

```sh
make export
```

![linear-vesting](/assets/gifs/linear-vesting.gif)

### Tests

For comprehensive information on the test suite for Linear Vesting implementation, including unit tests and property-based tests, please refer to our [test documentation](/test/README.md).

## License

© 2023 Anastasia Labs.

All code is licensed under MIT License. See [LICENSE](./LICENSE) file
for details.