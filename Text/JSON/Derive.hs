module Text.JSON.Derive (makeJSON) where

import Language.Haskell.TH.All

makeJSON :: Derivation
makeJSON = derivation derive "JSON"

derive :: DataDef -> [Dec]
derive dat = simple_instance "JSON" dat [funN "showJSON" sbody]
  where sbody = [ sclause [ctp ctor 'x'] (show_case ctor) | ctor <- ctors ]
        show_case ctor@NormalC {} = show_normal ctor
        show_case ctor@InfixC {}  = show_normal ctor
        show_case ctor@RecC {}    = show_record ctor
        show_case      ForallC {} = error "show_case (ForallC {})" -- It's currently impossible to reify existential constructors anyways.
        show_normal ctor =      -- Array of fields, possibly with constructor tag at beginning
          l1 "JSArray" . lst . map (l1 "showJSON") . tag $ ctv ctor 'x'
            where tag | multipleCtors = (lit (ctorName ctor) :)
                      | otherwise     = id
        show_record ctor =      -- Either an object or a [constructor tag, object] pair (array)
          l1 "showJSON" . tag . l1 "showJSON" . l1 "toJSObject" . lst $ zipWith (\x y -> tup [lit x, l1 "showJSON" y]) (ctorFields ctor) (ctv ctor 'x')
            where tag | multipleCtors = \x -> lst [l1 "showJSON" . lit $ ctorName ctor, x]
                      | otherwise     = id

        ctors = dataCtors dat
        multipleCtors = length ctors > 1
