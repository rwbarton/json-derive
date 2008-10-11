module Text.JSON.Derive (makeJSON) where

import Language.Haskell.TH.All

makeJSON :: Derivation
makeJSON = derivation derive "JSON"

stringLit :: Valcon a => String -> a
stringLit = raw_lit . StringL

derive :: DataDef -> [Dec]
derive dat = simple_instance "JSON" dat [funN "showJSON" sbody]
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

        ctors = dataCtors dat
        multipleCtors = length ctors > 1
