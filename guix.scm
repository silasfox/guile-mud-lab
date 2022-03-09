(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix git-download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo))

(define-public guile-mud-lab
      (package
           (name "guile-mud-lab")
           (version "0.1")
           (source
                  (origin
                           (method git-fetch)
                           (uri (git-reference
                                                (url "https://github.com/silasfox/guile-mud-lab.git")
                                                (commit "3a40934")))
                           (file-name "guile-mud-lab-0.1-checkout")
                           (sha256 (base32 "1fpvpbplcvymnzxlcnj0warsflm1hicqf1y0vxqkvq58a3qw8vqc"))))
           (build-system gnu-build-system)
           (arguments `())
           (native-inputs
                  `(("autoconf" ,autoconf)
                            ("automake" ,automake)
                            ("pkg-config" ,pkg-config)
                            ("texinfo" ,texinfo)))
           (inputs `(("guile" ,guile-3.0)))
           (propagated-inputs
                  `(("guile-readline" ,guile-readline)))
           (synopsis "")
           (description "")
           (home-page "")
           (license license:gpl3+)))

guile-mud-lab
