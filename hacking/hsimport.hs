import Data.List
import qualified Language.Haskell.Exts as HS
import HsImport

main :: IO ()
main = hsimport $ defaultConfig { prettyPrint = prettyPrint
                                , findImportPos = findImportPos }
   where
      -- This is a bogus implementation of prettyPrint, because it doesn't
      -- handle the qualified import case nor does it considers any explicitely
      -- imported or hidden symbols.
      prettyPrint :: HS.ImportDecl -> String
      prettyPrint (HS.ImportDecl sloc modname qual _ _ mpkg mas mspec) =
        "import " ++ (ifStr qual "qualified") ++
        (maybe "" (\pkg -> " \"" ++ pkg ++ "\" ") mpkg)  ++
        getMN modname ++ (maybe "" (\name -> " as " ++ getMN name) $ mas) ++
        specprint mspec

      specprint :: Maybe (Bool, [HS.ImportSpec]) -> String
      specprint Nothing = ""
      specprint (Just (False, xs))
        = "\n  (\n" ++ printImportSpecs xs ++ "\n  )"
      specprint (Just (True, xs))
        = "\n  hiding (\n" ++ printImportSpecs xs ++ "\n  )"

      printImportSpecs :: [HS.ImportSpec] -> String
      printImportSpecs ins
          = let (x:xs) = sort ins
            in  "    " ++ printSpec x ++ "\n" ++ go xs
        where
          go []       = ""
          go [x']     = "  , " ++ printSpec x'
          go (x':xs') = "  , " ++ printSpec x' ++ "\n" ++ go xs'
          printSpec :: HS.ImportSpec -> String
          printSpec = HS.prettyPrint
    

      -- This findImportPos implementation will always add the new import
      -- declaration at the end of the current ones. The data type ImportPos
      -- has the two constructors After and Before.
      findImportPos :: HS.ImportDecl -> [HS.ImportDecl] -> Maybe ImportPos
      findImportPos _         []             = Nothing
      findImportPos newImport currentImports = Just findPos
        where
          lastPos = After . last $ currentImports
          findPos = let xs = takeWhile (\x -> (getMN $ HS.importModule x)
                                              <
                                              (getMN $ HS.importModule newImport)
                                       )
                             . sort
                             $ currentImports
                    in if null xs then lastPos else After . last $ xs

      ifStr :: Bool -> String -> String
      ifStr True str = str
      ifStr False _  = ""

      getMN :: HS.ModuleName -> String
      getMN (HS.ModuleName name) = name
