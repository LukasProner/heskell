cabal-version:      3.0
name:               guess
version:            0.1.0.0
synopsis:           Hra - hádat cislo

executable guess
    main-is:          Main.hs
    -- Tu je kľúčová závislosť na balíku random:
    build-depends:    base ^>=4.20.2.0,
                      random
    default-language: Haskell2010