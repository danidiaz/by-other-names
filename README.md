(**NOTE**: this is a cabal package with multiple public libraries. Hackage
doesn't currently render correctly the Haddocks for packages with multiple
public libraries.)

# by-other-names

Give aliases to record fields.

# stuff

    Rep Foo :: * -> *
    = D1
        ('MetaData "Foo" "Main" "main" 'False)
        (C1
           ('MetaCons "Foo" 'PrefixI 'True)
           (S1
              ('MetaSel
                 ('Just "aa")
                 'NoSourceUnpackedness
                 'NoSourceStrictness
                 'DecidedLazy)
              (Rec0 Int)
            :*: (S1
                   ('MetaSel
                      ('Just "bb")
                      'NoSourceUnpackedness
                      'NoSourceStrictness
                      'DecidedLazy)
                   (Rec0 Bool)
                 :*: S1
                       ('MetaSel
                          ('Just "cc")
                          'NoSourceUnpackedness
                          'NoSourceStrictness
                          'DecidedLazy)
                       (Rec0 Char))))

    Prelude Main> import GHC.Generics
    Prelude GHC.Generics Main> :info Summy
    type Summy :: *
    data Summy = Aa Int | Bb Bool | Cc
            -- Defined in `Main'
    instance Generic Summy -- Defined in `Main'
    instance Read Summy -- Defined in `Main'
    instance Show Summy -- Defined in `Main'
    type instance Rep Summy
      = D1
          ('MetaData "Summy" "Main" "main" 'False)
          (C1
             ('MetaCons "Aa" 'PrefixI 'False)
             (S1
                ('MetaSel
                   'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                (Rec0 Int))
           :+: (C1
                  ('MetaCons "Bb" 'PrefixI 'False)
                  (S1
                     ('MetaSel
                        'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                     (Rec0 Bool))
                :+: C1 ('MetaCons "Cc" 'PrefixI 'False) U1))
            -- Defined in `Main'

    cabal repl test:tests
    cabal repl lib:by-other-names

