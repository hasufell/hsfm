import Data.List
import qualified Language.Haskell.Exts as HS
import HsImport

main :: IO ()
main = hsimport $ defaultConfig { prettyPrint = prettyPrint
                                , findImportPos = findImportPos }
   where
      prettyPrint :: HS.ImportDecl -> String
      prettyPrint (HS.ImportDecl sloc modname qual _ _ mpkg mas mspec) =
        "import " ++ (ifStr qual "qualified") ++
        (maybe "" (\pkg -> " \"" ++ pkg ++ "\" ") mpkg)  ++
        getMN modname ++ (maybe "" (\name -> " as " ++ getMN name) $ mas) ++
        specprint mspec

      specprint :: Maybe (Bool, [HS.ImportSpec]) -> String
      specprint Nothing = ""
      specprint (Just (False, xs))
        = "\n  (\n" ++ printImportSpecs xs ++ "  )"
      specprint (Just (True, xs))
        = "\n  hiding (\n" ++ printImportSpecs xs ++ "  )"

      printImportSpecs :: [HS.ImportSpec] -> String
      printImportSpecs ins
          = let (x:xs) = sort ins
            in  "    " ++ printSpec x ++ "\n" ++ go xs
        where
          go []       = ""
          go [x']     = "  , " ++ printSpec x' ++ "\n"
          go (x':xs') = "  , " ++ printSpec x' ++ "\n" ++ go xs'
          printSpec :: HS.ImportSpec -> String
          printSpec = HS.prettyPrint
    

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
