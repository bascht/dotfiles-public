(define-module (bascht binary-packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix licenses))

(define-public gopass (package
                       (name "gopass")
                       (version "1.15.13")
                       (source (origin
                                (method url-fetch)
                                (uri (string-append "https://github.com/gopasspw/gopass/releases/download/v"
                                                    version "/gopass-" version "-linux-amd64.tar.gz" ))
                                (sha256
                                 (base32 "1w3vzrdmj9s9vf4lrmw90rc8zdxd64yz9rc00drc27fy9z2gn4nm"))))
                       (build-system copy-build-system)
                       (supported-systems '("x86_64-linux"))
                       (arguments
                        `(#:install-plan '(("gopass" "bin/"))))
                       (synopsis "The slightly more awesome standard unix password manager for teams")
                       (description "Manage your credentials with ease. In a globally distributed team, on multiple devices or fully offline on an air gapped machine.")
                       (home-page "https://www.gopass.pw/")
                       (license mit1.0)))

(define-public git-credential-gopass (package
                       (name "git-credential-gopass")
                       (version "1.15.3")
                       (source (origin
                                (method url-fetch)
                                (uri (string-append "https://github.com/gopasspw/git-credential-gopass/releases/download/v"
                                                    version "/git-credential-gopass-" version "-linux-amd64.tar.gz" ))
                                (sha256
                                 (base32 "02cql9mlni3iwwacd5pcyl64bnwydq24083774pw00dlxzfzdvyn"))))
                       (build-system copy-build-system)
                       (supported-systems '("x86_64-linux"))
                       (arguments
                        `(#:install-plan '(("git-credential-gopass" "bin/"))))
                       (synopsis "Gopass git-credentials helper ")
                       (description "This command allows you to cache your git-credentials with gopass.")
                       (home-page "https://www.gopass.pw/")
                       (license mit1.0)))

(define-public cliphist (package
                         (name "cliphist")
                         (version "0.5.0")
                         (source (origin
                                  (method url-fetch)
                                  (uri (string-append "https://github.com/sentriz/cliphist/releases/download/v"
                                                      version "/v" version "-linux-amd64" ))
                                  (sha256
                                   (base32 "1sjxn9b1ka2n3c23a0p8z6rz5sfaw6f7k3ds5sq0vp6w1zy2cdgx"))))
                         (build-system copy-build-system)
                         (supported-systems '("x86_64-linux"))
                         (arguments
                          `(#:phases (modify-phases %standard-phases
                                                    (add-after 'unpack 'chmod
                                                               (lambda* (#:key #:allow-other-keys)
                                                                 (rename-file ,(string-append "v" version "-linux-amd64")
                                                                              "cliphist")
                                                                 (chmod "cliphist" #o755))))
                            #:substitutable? #f
                            #:install-plan '(("cliphist" "bin/cliphist"))))
                         (synopsis "")
                         (description "")
                         (home-page "")
                         (license epl1.0)))

(define-public atuin (package
                      (name "atuin")
                      (version "18.2.0")
                      (source (origin
                               (method url-fetch)
                               (uri (string-append "https://github.com/atuinsh/atuin/releases/download/v"
                                                   version "/atuin-v" version "-x86_64-unknown-linux-musl.tar.gz" ))
                               (sha256
                                (base32 "04s3apdb80d4dsa5aklzw1bwq9zd7s67pxkdq6mwbbvn7ymdlf6h"))))
                      (build-system copy-build-system)
                      (supported-systems '("x86_64-linux"))
                      (arguments
                       `(#:install-plan '(("atuin" "bin/"))))
                      (synopsis "")
                      (description "")
                      (home-page "")
                      (license epl1.0)))

(define-public spotify-player (package
                               (name "spotify-player")
                               (version "0.18.1")
                               (source (origin
                                        (method url-fetch)
                                        (uri (string-append "https://github.com/aome510/spotify-player/releases/download/v"
                                                            version "/spotify_player-x86_64-unknown-linux-gnu.tar.gz" ))
                                        (sha256
                                         (base32 "0fib2rxn4zqv2bzp0mlvc5hw99s3lan5ql7y8pkp11jxgi90jkp2"))))
                               (build-system copy-build-system)
                               (supported-systems '("x86_64-linux" "i686-linux"))
                               (inputs
                                `(("openssl" ,openssl)))
                               (arguments
                                `(#:install-plan '(("spotify_player" "bin/"))))
                               (synopsis "")
                               (description "")
                               (home-page "")
                               (license epl1.0)))

(define-public starship (package
                         (name "starship")
                         (version "1.18.1")
                         (source (origin
                                  (method url-fetch)
                                  (uri (string-append "https://github.com/starship/starship/releases/download/v"
                                                      version "/starship-x86_64-unknown-linux-musl.tar.gz" ))
                                  (sha256
                                   (base32 "1b0r613wsvvpdxndvv4m6mcxwljafbsklkgxpa56pmiggk8zh4gq"))))
                         (build-system copy-build-system)
                         (supported-systems '("x86_64-linux" "i686-linux"))
                         (arguments
                          `(#:install-plan '(("starship" "bin/"))))
                         (synopsis "")
                         (description "")
                         (home-page "")
                         (license epl1.0)))

