(hall-description
  (name "mud-lab")
  (prefix "guile")
  (version "0.1")
  (author "Silas Vedder")
  (copyright (2020))
  (synopsis "")
  (description "")
  (home-page "")
  (license gpl3+)
  (dependencies `("guile-readline" (ice-9 readline) ,guile-readline))
  (files (libraries ((scheme-file "mud-lab")))
         (tests ((directory
                   "tests"
                   ((test-result-file "simple-test")
                    (scheme-file "simple-test")
                    (log-file "simple-test")))))
         (programs ())
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory
              "doc"
              ((text-file "stamp-vti")
               (info-file "guile-mud-lab")
               (texi-file "version")
               (text-file ".dirstamp")
               (texi-file "mud-lab")))
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")))
         (infrastructure
           ((scheme-file "guix")
            (scheme-file "hall")
            (directory
              "build-aux"
              ((text-file "mdate-sh")
               (tex-file "texinfo")
               (scheme-file "test-driver")
               (text-file "install-sh")
               (text-file "missing")))
            (autoconf-file "configure")
            (automake-file "Makefile")
            (in-file "pre-inst-env")))))
