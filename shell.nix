let
  self = builtins.toString ./.;
  pkgs = import <nixpkgs> {};
  overrides = (builtins.fromTOML (builtins.readFile (self + "/rust-toolchain.toml")));
in
pkgs.mkShell {
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = with pkgs; [
    cargo
    rustup
    clang
  ];

  RUSTC_VERSION = overrides.toolchain.channel;
  LIBCLANG_PATH = pkgs.lib.makeLibraryPath [ pkgs.llvmPackages_latest.libclang.lib ];

  shellHook = ''
    export PATH=''${CARGO_HOME:-~/.cargo}/bin:$PATH
    export PATH=''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin:$PATH
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.wayland}/lib";
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.libxkbcommon}/lib";
  '';
}
