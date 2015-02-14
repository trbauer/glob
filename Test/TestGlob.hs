module TestGlob where

import Control.Monad
import Debug.Trace
import Data.Bits
import Data.Char
import Data.List
import Text.Printf
import System.FilePath
import System.Directory
import System.Random

import Glob


-- TODO:
--  * Test non-standard abstract syntax (GlobSeq's nested, unit GlobSeq's, what happens with empty string literals)
--  * Update pureTests to do matching itself somehow... so we get concise output
--  * Start a first pass at file system tests.
--

------------------------------------------------
------------------------------------------------
--     SYNTAX TESTS   parseGlob/ppGlob        --
------------------------------------------------
------------------------------------------------
syntaxTests = mapM_ testSyntax [
   oky ("abcd",GlobLit 1 "abcd")
 , oky ("\\\\abcd",GlobLit 1 "\\abcd")
 , oky ("{a{b,c}d}e",GlobSeq 1 [GlobAlt 1 "1" [GlobSeq 2 [GlobLit 2 "a",GlobAlt 3 "2" [GlobLit 4 "b",GlobLit 6 "c"],GlobLit 8 "d"]],GlobLit 10 "e"])
 , oky ("ab/cd", GlobSeq 1 [GlobLit 1 "ab",GlobSep 3,GlobLit 4 "cd"])
 , oky ("a$var{b,c}d", GlobSeq 1 [GlobLit 1 "a",GlobAlt 2 "var" [GlobLit 7 "b",GlobLit 9 "c"],GlobLit 11 "d"])
 , oky ("$\"var (\\\"quoted\\\")\"{b,c}",GlobAlt 1 "\"var (\"quoted\")\"" [GlobLit 19 "b",GlobLit 21 "c"])
 , oky ("ab{c,d}ef",GlobSeq 1 [GlobLit 1 "ab",
                    GlobAlt 3 "1" [GlobLit 4 "c",GlobLit 6 "d"],
                    GlobLit 8 "ef"])
 , oky ("{a,b}",GlobAlt 1 "1" [GlobLit 2 "a",GlobLit 4 "b"])
 , oky ("ab/{c,d}/ef", GlobSeq 1 [GlobLit 1 "ab",
                        GlobSep 3, GlobAlt 4 "1" [GlobLit 5 "c",GlobLit 7 "d"],
                        GlobSep 9, GlobLit 10 "ef"])
 , oky ("a*b?c", GlobSeq 1 [GlobLit 1 "a",GlobAnyStr 2 "1",
                            GlobLit 3 "b",GlobAnyChr 4 "2",GlobLit 5 "c"])
 , oky ("{aa,b*b}",GlobAlt 1 "1"
                    [GlobLit 2 "aa",GlobSeq 5
                      [GlobLit 5 "b",
                      GlobAnyStr 6 "2",
                      GlobLit 7 "b"]])
 , oky ("{aaa/bb}", GlobAlt 1 "1" [GlobSeq 2 [GlobLit 2 "aaa",GlobSep 5,GlobLit 6 "bb"]])

 -- ERRORS
 , err ("","1. empty glob expression")
 , err ("ab,cd","3. ',' outside of alternation")
 , err ("$1{a,b}","2. simple variables must start with alphabetic character")
 , err ("abc{unmatched","4. could not find matching '}'")
 , err ("foo$var", "8. variables must annotate variable expressions")
 , err ("foo$\"var\"bar", "10. variables must annotate variable expressions")
 , err ("$\"\\\"{a,b}","2. unclosed quoted variable")
 , err ("a{x,y}*$v?{a,b\\}..","11. could not find matching '}'")
 ]
 where oky (s,r) = (s,Right r)
       err (s,e) = (s,Left e)

testSyntax :: (String,Either String GlobExpr) -> IO ()
testSyntax (str,eeg) = do
  -- print (str,eeg)
  putStr $ printf "%-36s  " (show str++":")
  let ind = unlines . map ("    "++) . lines
      s = case parseGlob str of
            Left err ->
              case eeg of
                Left rerr
                  | rerr `isInfixOf` err -> "ok (err good)"
                  | otherwise -> "FAILED: error message wrong:\n" ++ ind err
                Right r -> "FAILED: parse failed: " ++ err
            Right exp
              | ppGlob exp /= str -> "FAILED: ppGlob mismatches: "++ppGlob exp
              | otherwise ->
                case eeg of
                  Left _ -> "FAILED: parse should have failed: "++show exp
                  Right ref
                    | exp == ref -> "ok"
                    | otherwise -> "FAILED: parsed value mismatch: "++show exp
  putStrLn s

------------------------------------------------
------------------------------------------------
--      PURE TESTS    expandGlob              --
------------------------------------------------
------------------------------------------------
pureTests = sequence
  [litTest1, litTest2,
   altTest1, altTest2, altTest3, altTest4, altTest5,
   strTest1, strTest2,
   chrTest1,
   cmbTest1]

-- ab - literal composition
litTest1 = runPureTestE (GlobSeq 1 [GlobLit 1 "a", GlobLit 4 "b"]) litVals1
litVals1 = ["", "a", "b", "aa","ab", "abs"]
-- ab - with embedded sequences
litTest2 = runPureTestE (GlobSeq 0 [GlobSeq 1 [GlobSeq 0 [GlobLit 1 "a"], GlobSeq 0 [GlobLit 4 "b"]]]) litVals1
litTest3 = runPureTestS "a/b" ["","a","a/b","a/bs","a/b/s"]

-- {a,b}x - prefix alternation
altTest1 = runPureTestE (GlobSeq 1 [
  GlobAlt 1 "1" [GlobLit 2 "a", GlobLit 4 "b"],
  GlobLit 6 "x"])
  ["","a","ax","axs","ay","bx"]

-- a{x,y} - suffix alternation
altTest2 = runPureTestE (GlobSeq 1 [
  GlobLit 1 "a",
  GlobAlt 2 "1" [GlobLit 3 "x", GlobLit 5 "y"]]) ["","a","ax","axs","ay","b"]

-- {a,b}x{c,d} - multiple variables
altTest3 = runPureTestE (GlobSeq 1 [
  GlobAlt 1 "1" [GlobLit 2 "a", GlobLit 4 "b"],
  GlobLit 6 "x",
  GlobAlt 8 "2" [GlobLit 2 "c", GlobLit 4 "d"]]) altVals3
altVals3 = ["","a","ax","bx","axc","axd","axds","axx","bxd","bxa","bxds"]

-- {a,b}x{c,d} - multiple variables with sequences embedded
altTest4 = runPureTestE (GlobSeq 1 [
  GlobSeq 1 [GlobAlt 1 "1" [GlobLit 2 "a", GlobSeq 4 [GlobLit 4 "b"]]],
  GlobSeq 6 [GlobLit 6 "x"],
  GlobSeq 8 [GlobAlt 8 "2" [GlobLit 10 "c", GlobLit 12 "d"]]]) altVals3

-- a{x,y{z,w}} - nested expressions
altTest5 = runPureTestE (GlobSeq 1 [
  GlobLit 1 "a",
  GlobAlt 2 "1" [
    GlobLit 3 "x",
    GlobSeq 5 [GlobLit 5 "y",GlobAlt 6 "2" [GlobLit 7 "z",GlobLit 9 "w"]]]])
  ["","a","ax","axs","ay","ayz","ayw","ayzs"]

-- * - any string
strTest1 = runPureTestE (GlobSeq 1 [GlobAnyStr 1 "1"])
  ["","a","ab","cd","ab/cd"]
-- ** - any string combination
strTest2 = runPureTestE (GlobSeq 1 [GlobAnyStr 1 "1", GlobAnyStr 2 "2"])
  ["","a","abc"]

-- ? - any char
chrTest1 = runPureTestE (GlobSeq 1 [GlobAnyChr 1 "1"])
  ["","a","ab"]
-- ?a?
chrTest2 = runPureTestE (GlobSeq 1 [GlobAnyChr 1 "1",GlobLit 2 "b",GlobAnyChr 3 "2"])
  ["","a","ab","abc","abcd"]

-- "{a?*,b*}s" - combination of all constructs
cmbTest1 = runPureTestE (GlobSeq 1 [GlobAlt 1 "1" [GlobSeq 2 [GlobLit 2 "a",GlobAnyChr 3 "2",GlobAnyStr 4 "3"],GlobSeq 6 [GlobLit 6 "b",GlobAnyStr 7 "4"]],GlobLit 1 "s"])
  ["",
   "axaas/t", -- yes
   "axs/t",   -- no (nothing for * after the a?)
   "axyyyys/t", -- yes (x yyyy
   "bzzz", -- no s after alt
   "bzzzs" -- yes
   ]

runPureTestE :: GlobExpr -> [String] -> IO ()
runPureTestE g xs = runPureTest (Right g) xs
runPureTestS :: String -> [String] -> IO ()
runPureTestS s xs = runPureTest (Left s) xs
runPureTest :: Either String GlobExpr -> [String] -> IO ()
runPureTest eg vals = do
  case eg of
    Left str -> case parseGlob str of
                  Left err -> putStrLn $ str ++ ": FAILED: " ++ err
                  Right g -> runPureTest (Right g) vals
    Right g -> do
      putStr $ show (ppGlob g) ++ ": "
      let res = concatMap (expandGlob g) vals
      putStrLn $ show g
      putStrLn $ "   inp: " ++ show vals
      putStrLn $ "   res: " ++ show res
      forM_ vals $ \v -> do
        putStr $ printf "  %-16s " (show v)
        let fs = filter (\(defs,val,sfx) -> v == val ++ sfx) res
        if null fs then putStrLn "no match" else do
          forM_ (zip ("" : repeat "                   ") fs) $ \(indent,(defs,val,sfx)) -> do
              putStrLn $ indent ++
                printf "%-8s" val ++ " " ++  printf "%-16s  " (show defs) ++
                if null sfx then "" else " with suffix " ++ show sfx

------------------------------------------------
------------------------------------------------
--   PATH TESTS  expandGlobPath               --
------------------------------------------------
------------------------------------------------
pathTests = sequence [pathTest1,pathTest2,pathTest3]
pathTest1 = runPathTest "testdir" "top/*"
  [([("1","b_y")],"top/b_y"),
   ([("1","b_x")],"top/b_x"),
   ([("1","a_y")],"top/a_y"),
   ([("1","a_x")],"top/a_x")]
pathTest2 = runPathTest "testdir/top" "{a,b}_{x,y}"
  [([("1","b"),("2","y")],"b_y"),
   ([("1","b"),("2","x")],"b_x"),
   ([("1","a"),("2","y")],"a_y"),
   ([("1","a"),("2","x")],"a_x")]
pathTest3 = runPathTest "testdir" "*/{a,b}_{x,y}/*/dat"
  [([("1","top"),("2","b"),("3","y"),("4","foo")],"top/b_y/foo/dat"),
   ([("1","top"),("2","b"),("3","y"),("4","baz")],"top/b_y/baz/dat"),
   ([("1","top"),("2","b"),("3","y"),("4","bar")],"top/b_y/bar/dat"),
   ([("1","top"),("2","b"),("3","x"),("4","qux")],"top/b_x/qux/dat"),
   ([("1","top"),("2","b"),("3","x"),("4","foo")],"top/b_x/foo/dat"),
   ([("1","top"),("2","b"),("3","x"),("4","baz")],"top/b_x/baz/dat"),
   ([("1","top"),("2","b"),("3","x"),("4","bar")],"top/b_x/bar/dat"),
   ([("1","top"),("2","a"),("3","y"),("4","qux")],"top/a_y/qux/dat"),
   ([("1","top"),("2","a"),("3","y"),("4","foo")],"top/a_y/foo/dat"),
   ([("1","top"),("2","a"),("3","y"),("4","baz")],"top/a_y/baz/dat"),
   ([("1","top"),("2","a"),("3","y"),("4","bar")],"top/a_y/bar/dat"),
   ([("1","top"),("2","a"),("3","x"),("4","qux")],"top/a_x/qux/dat"),
   ([("1","top"),("2","a"),("3","x"),("4","foo")],"top/a_x/foo/dat"),
   ([("1","top"),("2","a"),("3","x"),("4","baz")],"top/a_x/baz/dat"),
   ([("1","top"),("2","a"),("3","x"),("4","bar")],"top/a_x/bar/dat")]




runPathTest :: FilePath -> String -> [(VarDefs,String)] -> IO ()
runPathTest testDirRoot expr rpaths = do
  putStr $ expr ++ ": "
  case parseGlob expr of
    Left err -> putStrLn $ ": FAILED: parse error: " ++ err
    Right ge -> do
      putStrLn $ show ge
      ps <- expandGlobPath ge testDirRoot
      putStr $ "  - under dir " ++ testDirRoot ++ " this gives "
      putStrLn $ show (length ps) ++ " matches"
      forM_ ps $ \(vs,p) -> do
        putStr $ "    * " ++ show (vs,p) ++ ": "
        if (vs,p) `elem` rpaths then putStr "ok"
          else putStr "ERROR: unexpected entry"
        putStrLn ""
      let missing = rpaths \\ ps
      when (not (null missing)) $ do
        putStrLn "ERROR:  the following are missing"
        mapM_ (\(vs,p) -> putStrLn $ "    * " ++ show (vs,p)) missing

makePathTestTree = do
  let getDatLine f = do
        let hash f = foldl1' xor (map ord f) `mod` 10
        rs <- sequence (replicate 5 (randomRIO (1,10)))
        let vs = map ((hash f)+) rs
        return $ intercalate " " (map (printf "%6d") vs) ++ "\n"
      mkFile f = do
        n <- randomRIO (1,7) :: IO Int
        ls <- sequence (replicate n (getDatLine f))
        createDirectoryIfMissing True (takeDirectory f)
        putStrLn $ f ++ ":\n" ++ concat ls
        writeFile f ("#" ++ f ++ "\n" ++ concat ls)

      files = ["testdir/top/" ++ a_b : "_" ++ x_y : "/" ++ fbbq ++ "/dat" |
               a_b <- "ab",
               x_y <- "xy",
               fbbq <- ["foo","bar","baz","qux"],
               a_b /= 'b' || x_y /= 'y' || (fbbq /= "baz" && fbbq /= "qux")]
  mapM_ mkFile files
  let badFile = "testdir/top/b_y/baz/dat"
      partialBadFile = "testdir/top/b_x/baz/dat"

  putStrLn $ "adding bad line to: " ++ partialBadFile
  appendFile partialBadFile "malformed line in otherwise good file"

  putStrLn $ "creating bad file:  " ++ badFile
  createDirectoryIfMissing True (takeDirectory badFile)
  writeFile badFile "malformed file data"

{-
test e = expandGlobPath e "testdir"
ge0 = GlobSeq 1 [ -- top/a_x
      GlobLit 1 "top", GlobSep 4, GlobLit 1 "a_x"]
ge1 = GlobSeq 1 [ -- top/*
      GlobLit 1 "top", GlobSep 4,
      GlobAnyStr 5 "1"]
ge2 = GlobSeq 1 [ -- top/?*
      GlobLit 1 "top", GlobSep 4,
      GlobAnyChr 5 "1", GlobAnyStr 6 "2"]
gt = GlobSeq 1 [
      GlobLit 1 "top", GlobSep 4,
      GlobAnyStr 5 "1", GlobSep 6,
      GlobSeq 7 [GlobAlt 7 "2" [GlobLit 8 "a",GlobLit 10 "b"],GlobLit 7 "_",GlobAlt 13 "3" [GlobLit 14 "x",GlobLit 16 "y"]], GlobSep 18,
      GlobLit 19 "dat"]
-}















