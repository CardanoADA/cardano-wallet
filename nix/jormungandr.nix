############################################################################
# Packages for jormungandr and jcli.
#
# These use the functions from iohk-nix to build the version that we
# require for cardano-wallet.
#
# To change the version:
# 1. Adjust the "version" and/or "rev" variables below.
# 2. Then invert the first digit in *both* sha256 and
#    cargoSha256. That is, change 0 to 1 and 1 to 0. It's important
#    that you change them to something different than before,
#    otherwise you may get the previous version from your local cache.
# 3. Run "nix-build -A jormungandr.src" (or let CI do it for you).
#    It will say that the hash is wrong. Update sha256 to the value it got.
# 4. Run "nix-build -A jormungandr". After some time downloading
#    crates, it should say that the vendor hash is wrong.
#    Update cargoSha256 with the value it got.
# 5. Run "nix-build -A jormungandr". It should complete the build.
# 6. Test that "nix-build -A jormungandr-cli" also works.
# 7. If you now run "nix-shell" you should have updated versions of
#    jormungandr and jcli.
#
############################################################################

{ iohkLib ? import ./iohk-common.nix {}
, pkgs ? iohkLib.pkgs
}:

let
  release = rec {
    version = "0.7.0-alpha.dev.1";
    # Git revision of input-output-hk/jormungandr repo.
    rev = "v${version}";
    # Hash of git repo and all of its submodules.
    sha256 = "0r3icx42glrpa68sjxz4gr0z5660gh4n79lncy720s04cmgjcjci";
    # Hash of all Cargo dependencies.
    cargoSha256 = "0f9b2lr2xxlcn9j33b5ahzbndz6sjm8ybhqm472bv5hzisqm4lg4";
  };

  windows = rec {
    # URL and hash of windows binary release
    url = "https://github.com/input-output-hk/jormungandr/releases/download/v${release.version}/jormungandr-v${release.version}-x86_64-pc-windows-msvc.zip";
    sha256 = "16pxgi4igvfh2kccsbyizfc4wyxr8fs1872hpsmr99ppna09rqi3";
  };

  jormungandr-win64 = pkgs.runCommand "jormungandr-win64-${release.version}" {
    nativeBuildInputs = [ pkgs.buildPackages.unzip ];
  } ''
    mkdir -p $out/bin
    cd $out/bin
    unzip ${pkgs.fetchurl windows}
  '';

  nonWindows = pkg: if pkgs.stdenv.hostPlatform.isWindows
    then jormungandr-win64
    else pkg;

in {
  jormungandr = nonWindows (iohkLib.rust-packages.pkgs.makeJormungandr release);
  jormungandr-cli = nonWindows (iohkLib.rust-packages.pkgs.makeJcli release);

  inherit jormungandr-win64;
}