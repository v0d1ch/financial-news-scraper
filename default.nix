{ mkDerivation, base, curl, scalpel, stdenv, text, webdriver }:
mkDerivation {
  pname = "financial-news-scraper";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base curl scalpel text webdriver ];
  homepage = "https://github.com/v0d1ch/financial-news-scraper#readme";
  license = stdenv.lib.licenses.bsd3;
}
