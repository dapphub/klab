let
  # --- nixpkgs ---

  nixpkgs = builtins.fetchGit rec {
    name = "nixpkgs-19.09-${rev}";
    url = https://github.com/nixos/nixpkgs;
    ref = "nixos-19.09";
    # git ls-remote https://github.com/nixos/nixpkgs-channels nixos-19.09
    rev = "9f453eb97ffe261ff93136757cd08b522fac83b7";
  };
  pkgs = import nixpkgs {};

  # --- dapptools ---

  dappSrc = builtins.fetchGit rec {
    name = "dapptools-${rev}";
    url = https://github.com/dapphub/dapptools;
    rev = "eb2380c990179ada97fc1ee376ad6f2a32bfe833";
    ref = "dapp/0.26.0";
  };
  dapptools = import dappSrc {};

  dapp = (
    pkgs.writeShellScriptBin "dapp" ''
      export DAPPTOOLS=${dappSrc}
      ${dapptools.dapp}/bin/dapp "$@"
    ''
  );
in
pkgs.stdenv.mkDerivation {
  name = "klab-tutorial-dapptools";
  buildInputs = with pkgs; [ coreutils dapp gnused ];
}

