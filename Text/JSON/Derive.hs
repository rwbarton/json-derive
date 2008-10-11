module Text.JSON.Derive (makeJSON) where

import Control.Arrow (first)
import Language.Haskell.TH.All

makeJSON :: Derivation
makeJSON = derivation derive "JSON"

stringLit :: Valcon a => String -> a
stringLit = raw_lit . StringL

where' :: Clause -> Dec -> Clause
where' (Clause ps body decs) d = Clause ps body (d:decs)

derive :: DataDef -> [Dec]
derive dat = simple_instance "JSON" dat [funN "showJSON" sbody, funN "readJSON" rbody]
  where sbody = [ sclause [ctp ctor 'x'] (show_case ctor) | ctor <- ctors ]
        show_case ctor@NormalC {} = show_normal ctor
        show_case ctor@InfixC {}  = show_normal ctor
        show_case ctor@RecC {}    = show_record ctor
        show_case      ForallC {} = error "show_case: can't handle existential constructors"
        show_normal ctor =      -- Array of fields, possibly with constructor tag at beginning
          l1 "JSArray" . lst . map (l1 "showJSON") . tag $ ctv ctor 'x'
            where tag | multipleCtors = (stringLit (ctorName ctor) :)
                      | otherwise     = id
        show_record ctor =      -- Either an object or a [constructor tag, object] pair (array)
          tag . l1 "showJSON" . l1 "toJSObject" . lst $ zipWith (\x y -> tup [stringLit x, l1 "showJSON" y]) (ctorFields ctor) (ctv ctor 'x')
            where tag | multipleCtors = \x -> l1 "showJSON" $ lst [l1 "showJSON" . stringLit $ ctorName ctor, x]
                      | otherwise     = id

        rbody | multipleCtors = [(sclause [l1 "JSArray" $ l2 ":" (vr "tag") (vr "v")] $ case' (tup [l1 "readJSON" $ vr "tag", vr "v"]) $
                                  [ first (tup . (l1 "Ok" (stringLit (ctorName ctor)) :) . (:[])) $ read_clause ctor | ctor <- ctors ] ++ [failure_case])
                                 `where'` lookupField_def,
                                 failure_clause
                                ]
              | otherwise     = [(sclause [vr "v"] $ case' (vr "v") (map read_clause ctors ++ [failure_case])) `where'` lookupField_def]
        read_clause ctor@NormalC {} = read_normal ctor
        read_clause ctor@InfixC {}  = read_normal ctor
        read_clause ctor@RecC {}    = read_record ctor
        read_clause      ForallC {} = error "read_clause: can't handle existential constructors"
        read_normal ctor =
          (munge $ lst $ ctv ctor 'x', liftmk (ctc ctor) (map (l1 "readJSON") (ctv ctor 'x')))
            where munge | multipleCtors = id
                        | otherwise     = l1 "JSArray"
        read_record ctor =
          (munge . l1 "JSObject" $ vr "obj", LetE [ValD (vr "alist") (NormalB (l1 "fromJSObject" $ vr "obj")) []] $ liftmk (ctc ctor) [ (l2 "lookupField" (stringLit name) (vr "alist")) >>=: l0 "readJSON" | name <- ctorFields ctor ])
            where munge | multipleCtors = \x -> lst [x]
                        | otherwise     = id
        err = (l1 "Error" $ stringLit ("Unable to read " ++ dataName dat))
        failure_case = (WildP, err)
        failure_clause = sclause [fst failure_case] (snd failure_case)
        lookupField_def = funN "lookupField" $ [sclause [vr "s", vr "a"] $ lK "maybe" [err, l0 "Ok", (l2 "lookup" (vr "s") (vr "a"))]]

        ctors = dataCtors dat
        multipleCtors = length ctors > 1
