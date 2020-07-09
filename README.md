# by-other-names
Give aliases to record fields

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
