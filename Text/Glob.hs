module Text.Glob where
--
-- TODO:
--  * Test nested variables. (We could disallow shadowing?)
--     - E.g. @A{@A?x@A?}_@A*
--       Permit a repeated variable "1x1_1x1"
--       How do we label the inner A?
--
-- SPECIFY:
--  * How to handle unit alternation f{o}o
--      * Treat it as var with 1 value (current case).
--      * Grouping construct (parse it as a GlobSeq) (not a var)

import Control.Applicative
import Data.Char
import Data.List
-- import Debug.Trace
import System.Directory

-- | A GlobQuery expression.  Constructed via 'parseGlob' and formatted
-- by 'ppGlob', glob queries can be used to match a set of strings or
-- binding necessary captures to each variable.
data Glob = GlobSeq     Int [Glob]          -- ^ expr1 expr2 ..
          | GlobLit     Int String          -- ^ foo
          | GlobAlt     Int GlobVar [Glob]  -- ^ {ge1,ge2,..}
                                            -- ^ or @var{ge1,ge2,..}
          | GlobAnyStr  Int GlobVar         -- ^ * or @var
          | GlobAnyChr  Int GlobVar         -- ^ ? or @?
          | GlobSep     Int                 -- ^ /
          deriving (Show,Eq)
type GlobVar = String
type VarDefs = [(GlobVar,String)]

-- arguments are [(defs,prefix-created-so-far)] -> [suffixes-left] -> [glob-left] -> [(newdefs,matchedstring,possible-leftovers)]
-- expandGlob :: [String] -> Glob -> [(VarDefs,FilePath,[FilePath])]
-- expandGlob vs (GlobPath _ [GlobSeq _ [gs]]) = expandGlobSeq  [([],"")] vs gs
-- expandGlob vs (GlobSeq _ gs) = expandGlobSeq [([],"")] vs gs

-- | Expands a glob expression against a string.  This returns
-- triples containing the: (1) bindings needed for (2) the full matched
-- string and the suffix leftover.  Typically users want full matches;
-- simply filter out those with non-empty suffixes
-- @filter (\(d,v,s) -> null s) (expandGlob ..)@.
--
-- Note, unlike 'expandGlobPath', this function does not work against
-- a file system, and all the strings to match against are passed in.
-- 'GlobSep' values are permitted in the glob, but are matched
-- literally against the '/' character.
expandGlob :: Glob -> String -> [(VarDefs,String,String)]
expandGlob g = reverse . expandGlobSeq [g] []

-- | Don't pass var defs so down the tree.  Why do we need the prefixes so far?
-- Build it up on the way up.             (defs, what-they-generate (values), leftover (suffixes))
expandGlobSeq :: [Glob] -> VarDefs -> String -> [(VarDefs,String,String)]
-- expandGlobSeq gs ds sfx
--   | trace ("expandGlobSeq " ++ show gs ++ " " ++ show sfx) False = undefined
expandGlobSeq []                       _        sfx    =  [([],"",sfx)]
expandGlobSeq (GlobSeq _ g : gs)       ds       sfx    =
  concatMap (\(d1,v1,s1) ->
    map (\(d2,v2,s2) -> (d1 ++ d2, v1 ++ v2, s2)) (expandGlobSeq gs ds s1))
    (expandGlobSeq g ds sfx)
expandGlobSeq (GlobLit _ lit : gs)     ds       sfx
  | lit `isPrefixOf` sfx = res
  | otherwise = [] -- mismatch
  where rest = expandGlobSeq gs ds (drop (length lit) sfx)
        res = map (\(v,val,sfx) -> (v, lit ++ val, sfx)) rest
expandGlobSeq (GlobAnyChr _ vnm : gs)  ds       sfx
  | null sfx = [] -- no more chars to match
  | hasVarDefNotEqTo ds vnm [c] = [] -- repeated variable with different value
  | otherwise = addDefsAndVals (expandGlobSeq gs ((vnm,[c]):ds) (tail sfx))
  where c = head sfx
        addDefsAndVals -- only add binding if not already defined by parent
          | hasVarDef ds vnm = map (\(d,v,s) -> (d, c:v, s))
          | otherwise        = map (\(d,v,s) -> ((vnm,[c]) : d, c:v, s))
expandGlobSeq (GlobAnyStr _ vnm : gs)  ds       sfx = concatMap expandStr (lists pfxs)
  where (pfxs,esfx) = span (/='/') sfx -- needed to prevent expansion past '/'
                                       -- inputs already contain '/'
        lists str = map (flip splitAt str) [1 .. length str]
        expandStr (pfx1,pfx2)
          | hasVarDefNotEqTo ds vnm pfx1 = [] -- repeated variable with different value
          | pfx1 `isPrefixOf` sfx = addDefsAndVals (expandGlobSeq gs ds_str (pfx2 ++ esfx))
          | otherwise = []
          where ds_str = (vnm,pfx1) : ds
                addDefsAndVals
                  | hasVarDef ds vnm = map (\(d,v,s) -> (d,             pfx1 ++ v, s))
                  | otherwise        = map (\(d,v,s) -> ((vnm, pfx1):d, pfx1 ++ v, s))
expandGlobSeq (GlobAlt _ vnm as : gs)  ds       sfx = concatMap expandAlt as
  where -- expands one alternate
        -- FOR each glob alt a in {a1,a2,...}
        --   expand a
        --   FOR each match in a: IF value of a conflicts with
        --       previous variable defn., skip it
        --       ELSE expand all suffixes given that match
        --            prefix the match's defs and values onto the result
        expandAlt :: Glob -> [(VarDefs,String,String)]
        expandAlt a = concatMap expandSuffixesUnder (expandGlobSeq [a] ds sfx)
        -- expand the results of expanding a on all the suffixes of a
        expandSuffixesUnder (da,va,sa)
          | hasVarDefNotEqTo ds vnm va = [] -- alt. mismatches repeated variable
          | otherwise = prefix_vas $ (expandGlobSeq gs ((vnm,va) : da) sa)
          where prefix_vas
                  | hasVarDef ds vnm = map (\(ds,vs,ss) -> (da ++ ds, va ++ vs, ss))
                  | otherwise        = map (\(ds,vs,ss) -> ((vnm,va) : (da ++ ds), va ++ vs, ss))


  -- old algorithm:
  -- for each alternate a expansion is
  --   expand the alternate against the current suffix
  --     for each match (d1,v1, s1) expand the rest of the glob sequence with suffix s1
  --       for each of those matches (d2,v2,s2):
  --            * update defs (d2), by binding vnm to v1 (the alt.),
  --                  add the defs for d1 as well
  --            * catenate the the values (v1++v2)
  --            * return suffix s2
--  where expandAlt :: Glob -> [(VarDefs,String,String)]
--        expandAlt a = concatMap (\(d1,v1,s1) ->
--                                  map (\(d2,v2,s2) -> ((vnm,v1) : (d1 ++ d2), v1 ++ v2, s2))
--                                  (expandGlobSeq gs ds s1))
--                                altMatches
--            where altMatches = expandGlobSeq [a] ds sfx -- everything we get by expanding this alt
--                  ds_alt = (vnm, ) : ds
-- Treat path separators as '/' literals
expandGlobSeq (GlobSep i : gs)         ds       sfx =
  expandGlobSeq (GlobLit i "/" : gs) ds sfx

-- defs have binding for k *not* equal to v
hasVarDefNotEqTo :: VarDefs -> String -> String -> Bool
hasVarDefNotEqTo vs k v = case lookup k vs of
                            Just val -> val /= v
                            Nothing -> False
hasVarDef :: VarDefs -> String -> Bool
hasVarDef vs k = case lookup k vs of
                   Just _ -> True
                   Nothing -> False

-- | This function matches a glob expression against a given directory root.
-- The rules of matching are roughly the same as 'expandGlobSeq' except:
--
--   (1) The list of strings to match against are generated by the file
--       system walk.  We only expand potential matches, and hence, don't
--       walk the entire tree during matching.
--
--   (2) Matches are all valid existing paths (directories or regular files).
--       Hence, the notion of a suffix following the match
--       (as in 'expandGlobSeq') is absent.  Consequently, this function
--       returns pairs, not triples.
--
expandGlobPath :: Glob -> FilePath -> IO [(VarDefs,FilePath)]
expandGlobPath g root = do
  fs <- getSubPaths root
  -- concatMapM (fmap (filter emptySuffix) . expandGlobSeqPath [g] root) fs
  -- putStrLn $ "root: " ++ root ++ ", fs: " ++ show fs
  let hasEmptySuffix (_,_,s) = null s
      contract (d,v,_) = (d,v)
  concatMapM (fmap (reverse .  map contract . filter hasEmptySuffix) . expandGlobSeqPath [g] [] (root <//> "")) fs

-- Helper used by 'expandGlobPath'
expandGlobSeqPath :: [Glob]   -- | The sequence
                  -> VarDefs  -- | The current bindings (for linear)
                  -> FilePath -- | The partial path so far
                  -> String   -- | The suffix left over to match against
                  -> IO [(VarDefs,String,String)]
-- expandGlobSeqPath (g:gs) path sfx
--  | trace ("expandGlobSeqPath (" ++ ppGlob g++") "++show path ++ " " ++ show sfx) False = undefined
expandGlobSeqPath []               _      _    sfx = return [([],"",sfx)]
expandGlobSeqPath (GlobSep _ : gs) ds     path "" = do
  exists <- doesDirectoryExist path
  if not exists then return [] -- non-existent dir
    else do fs <- getSubPaths path
            concatMapM (fmap (map (\(d,v,s) -> (d, '/':v, s))) . expandGlobSeqPath gs ds (path <//> "")) fs
expandGlobSeqPath (GlobSep _ : _)  _      _    _ = return [] -- suffix must be non-empty for GlobSep (last rule)
expandGlobSeqPath (GlobSeq _ g : gs) ds     path sfx = do
  gres <- expandGlobSeqPath g ds path sfx
  concatForM gres $ \(d1,v1,s1) -> do
    rest <- expandGlobSeqPath gs (d1 ++ ds) (path ++ v1) s1
    return $ map (\(d2,v2,s2) -> (d1 ++ d2, v1 ++ v2, s2)) rest
expandGlobSeqPath (GlobLit _ lit : gs) ds   path sfx
  | lit `isPrefixOf` sfx = res
  | otherwise = return [] -- mismatch
  where rest = expandGlobSeqPath gs ds (path ++ lit) (drop (length lit) sfx)
        res = fmap (map (\(v,val,sfx) -> (v, lit ++ val, sfx))) rest
expandGlobSeqPath (GlobAnyChr _ vnm : gs) ds path sfx
  | null sfx = return []
  | hasVarDefNotEqTo ds vnm [c] = return [] -- mismatch
  | otherwise = addDef <$> expandGlobSeqPath gs ds_c (path ++ [c]) (tail sfx)
  where c = head sfx
        ds_c = (vnm,[c]) : ds
        addDef
          | hasVarDef ds vnm = map (\(d,v,s) -> (d, c:v, s))
          | otherwise = map (\(d,v,s) -> ((vnm,[c]) : d, c:v, s))
expandGlobSeqPath (GlobAnyStr _ vnm : gs) ds path sfx = concatMapM expandStr (lists pfxs)
  where (pfxs,esfx) = span (/='/') sfx -- don't match past '/' if they happen to
                                       -- be in the sfx, maybe we should assert out
        lists str = map (flip splitAt str) [1 .. length str]
        expandStr (pfx1,pfx2)
          | hasVarDefNotEqTo ds vnm pfx1 = return []
          | pfx1 `isPrefixOf` sfx = addDef <$> expandGlobSeqPath gs ds_str (path ++ pfx1) (pfx2 ++ esfx)
          | otherwise = return []
          where ds_str = (vnm,pfx1) : ds
                addDef
                  | hasVarDef ds vnm = map (\(d,v,s) -> (d,               pfx1 ++ v, s))
                  | otherwise        = map (\(d,v,s) -> ((vnm, pfx1) : d, pfx1 ++ v, s))
expandGlobSeqPath (GlobAlt _ vnm as : gs) ds path sfx = concatMapM expandAltM as
  -- for each alternate a expansion is
  --   expand the alternate against the current suffix
  --     for each match (da,va, sa) expand the rest of the glob
  --                                       sequence with suffix sa
  --       for each of those matches (d2,v2,s2) that
  --               doesn't violate a repeated variable constraint
  --            * update defs (d2), by binding vnm to v1 (the alt.),
  --                                add the defs for d1 as well
  --            * catenate the the values (v1++v2)
  --            * return suffix s2
  where expandAltM :: Glob -> IO [(VarDefs,String,String)]
        expandAltM a = do
          alts <- expandGlobSeqPath [a] ds path sfx
          concatForM alts $ \(da,va,sa) -> do
            if hasVarDefNotEqTo ds vnm va then return [] -- repeated variable
              else do
                suffixes <- expandGlobSeqPath gs ((vnm,va) : ds) (path ++ va) sa
                let prefix
                      | hasVarDef ds vnm = map (\(d2,v2,s2) -> (((da ++ d2), va ++ v2, s2)))
                      | otherwise        = map (\(d2,v2,s2) -> (((vnm,va) : (da ++ d2), va ++ v2, s2)))
                return $ prefix suffixes

concatMapM :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f = fmap concat . mapM f
concatForM :: [a] -> (a -> IO [b]) -> IO [b]
concatForM = flip concatMapM

infixr 5 <//>
(<//>) :: FilePath -> FilePath -> FilePath
(<//>) "" b = b
(<//>) a  b
  | a_end == '/' || a_end == '\\' = a ++ b
  | otherwise = a ++ '/' : b
  where a_end = last a

getSubPaths :: FilePath -> IO [FilePath]
getSubPaths dir = do
  exists <- doesDirectoryExist dir
  if not exists then return []
--    else fmap (map (dir <//>)) (getDirPaths dir)
    else getDirPaths dir

getDirPaths :: FilePath -> IO [FilePath]
getDirPaths dir = fmap (filter isValid) (getDirectoryContents dir)
  where isValid "." = False
        isValid ".." = False
        isValid _ = True

-- | Pretty prints a 'Glob'.  This is almost the inverse of
-- 'parseGlob'.  Idempotent escapes are the exception:
-- @fmap ppGlob (parseGlob ("\\a"))@ produces @Right "a"@.
ppGlob :: Glob -> String
ppGlob (GlobSeq _ gs) = concatMap ppGlob gs
ppGlob (GlobLit _ l) = esc l
  where esc [] = []
        esc ('\\':cs) = '\\':'\\':esc cs
        esc (c:cs)
          | c `elem` "{}@,/*?" =  '\\' : c : esc cs
          | otherwise = c : esc cs
ppGlob (GlobAlt _ v as) = ppVar v ++ "{" ++ intercalate "," (map ppGlob as) ++ "}"
ppGlob (GlobAnyStr _ v) = ppVar v ++ "*"
ppGlob (GlobAnyChr _ v) = ppVar v ++ "?"
ppGlob (GlobSep _) = "/"
ppVar ('\"':vs) = "@\"" ++ esc (init vs) ++ "\""
  where esc [] = []
        esc ('"':cs) = '\\' : '"' : esc cs
        esc (c:cs) = c : esc cs
ppVar v
  | all isDigit v = "" -- anonymous
  | otherwise = "@" ++ v


-- | Parses a glob expression into abstract syntax.  The grammar follows:
-- let list(e,s) generate the separated list: e s e s ... e
--     + means one or more
--     (e) mean grammatical grouping
--
-- > glob = <glob_seq>
-- > glob_seq :: (<glob_lit> | <glob_alt> |
-- >              <glob_anystr> | <glob_anychr>)+
-- > glob_lit ::= ([^@{},/] | '\'[^])+       -- literal \c escapes any character c)
-- > glob_alt ::=            '{' list(<glob_seq>) '}'
-- >            | <glob_var> '{' list(<glob_seq>) '}'
-- > glob_anystr ::=            '*' -- anonymous variable
-- >               | <glob_var> '*'
-- > glob_anychr ::=            '?' -- anonymous variable
-- >               | <glob_var> '?'
-- > glob_var ::= '@' [_:alpha:][:alnum:]*  -- alpha numeric simple name
-- >            | '@' '"' [^"] '"'          -- quoted name (\" escapes to '"' in the name)
--
-- Upon successful parse 'Right' holds the value; otherwise the parse
-- error message is stored in 'Left'.  Anonymous glob exprssions
-- (those without variable names) are automatically named with a unique
-- integer.
parseGlob :: String -> Either String Glob
parseGlob "" = Left "1. empty glob expression"
parseGlob str0 =
  case parseGlobSeq 1 [] 1 1 str0 of
    Left e -> Left e
    Right (ge,_,_,"") -> Right ge
    Right (_,_,c,',':_) ->
      parseError c "',' outside of alternation (escape it \"\\,\")"
    Right (_,_,c,_) -> parseError c "syntax error (leftover string)"

  where parseError :: Int -> String -> Either String a
        parseError col msg = Left (show col ++ ". " ++ msg ++ "\n" ++
                               str0 ++ "\n" ++ replicate (col - 1) ' ' ++
                               "^\n")
        --  c0: beginning of this seq
        --  gs: reverse of globs in this seq so far
        --   v: the next fresh variable name
        -- col: the current column of the string
        -- str: the string we're parsing
        parseGlobSeq :: Int -> [Glob] -> Int -> Int -> String ->
                        Either String (Glob,Int,Int,String)
--        parseGlobSeq c0 gs v col str
--          | trace "*" False = undefined
        parseGlobSeq c0 gs v col ('@':str) =
          case parseGlobVarName (col+1) str of
            Left e -> Left e
            Right (var,col1,rest1) ->
              case rest1 of
                -- "" -> parseGlobSeq c0 (GlobAnyStr col var : gs) v col1 ""
                "" -> parseError col1 "variables must annotate variable expressions"
                ('{':rest2) ->
                  case parseGlobAlt col1 [] v (col1 + 1) rest2 of
                    Left e -> Left e
                    Right (alts,v2,col1,rest1) ->
                      parseGlobSeq c0 (GlobAlt col var alts : gs)
                                                     v2 col1    rest1
                ('*':rest2) ->
                    parseGlobSeq c0 (GlobAnyStr col1 var : gs)
                                                     v  (col1+1) rest2
                ('?':rest2) ->
                    parseGlobSeq c0 (GlobAnyChr col1 var : gs)
                                                     v  (col1+1) rest2
                _ -> parseError col1 "variables must annotate variable expressions"
        -- path separator
        parseGlobSeq c0 gs v col ('/':rest) =
          parseGlobSeq c0 (GlobSep col : gs) v (col + 1) rest
        -- anon vars
        parseGlobSeq c0 gs v col ('{':rest) =
          case parseGlobAlt col [] (v + 1) (col + 1) rest of
            Left e -> Left e
            Right (alts,v1,col1,rest1) ->
              parseGlobSeq c0 (GlobAlt col (show v) alts : gs) v1 col1 rest1
        parseGlobSeq c0 gs v col ('*':rest) =
          parseGlobSeq c0 (GlobAnyStr col (show v) : gs) (v + 1) (col + 1) rest
        parseGlobSeq c0 gs v col ('?':rest) =
          parseGlobSeq c0 (GlobAnyChr col (show v) : gs) (v + 1) (col + 1) rest

        -- literals, end of sequence (follow set), or end of string
        parseGlobSeq c0 gs v col s@(c:_)
          | c `elem` ",}" = Right (ge,v,col,s)
          | otherwise = case parseGlobLit "" col s of
                           Left e -> Left e
                           Right (lit,cn,rest) ->
                             parseGlobSeq c0 (GlobLit col lit : gs) v cn rest
          where ge | length gs == 1 = head gs
                   | otherwise = GlobSeq c0 (reverse gs)

        -- EOS
        parseGlobSeq _  [ge] v col "" = Right (ge,v,col,"") -- lift optimization
        parseGlobSeq c0 gs   v col "" = Right (GlobSeq c0 (reverse gs),v,col,"")


        parseGlobAlt :: Int -> [Glob] -> Int -> Int -> String ->
                                  Either String ([Glob],Int,Int,String)
        parseGlobAlt c0 gs v col str =
          case parseGlobSeq col [] v col str of
            Left e -> Left e
            Right (e,v,cn,',':rest) -> parseGlobAlt c0 (e:gs) v (cn+1) rest
            Right (e,v,cn,'}':rest) -> Right (reverse (e:gs),v,cn+1,rest)
            Right (_,_,_,"") ->
              parseError c0 "could not find matching '}'"
            Right (_,_,cn,_) ->
              parseError cn ("illegal chars in alternation")

        -- @var1
        -- @"var name (anything goes)"
        -- @"here's \"quotes\""
        parseGlobVarName :: Int -> String -> Either String (GlobVar,Int,String)
        parseGlobVarName col ('\"':str) = takeVarName "" str  -- quoted variable
          where takeVarName _ [] = parseError col "unclosed quoted variable"
                takeVarName v ('"':rest) = Right ('"' : reverse ('\"':v), col + 1 + length v + 1, rest)
                takeVarName v ('\\':'"':cs) = takeVarName ('"':v) cs
                takeVarName v (c:cs) = takeVarName (c:v) cs
        parseGlobVarName col (c:str) -- simple variable
          | isAlpha c || c == '_' = Right (c:var, col + 1 + length var, rest)
          where (var,rest) = span (\c -> isAlphaNum c || c == '_') str
        parseGlobVarName col _ =
          parseError col "simple variables must start with alphabetic character"

        parseGlobLit :: String -> Int -> String ->
                                         Either String (String,Int,String)
        parseGlobLit litR col ('\\':c:cs) = parseGlobLit (c:litR) (col+2) cs
        parseGlobLit litR col s@(c:cs) -- [^@{},/]+
          | c `elem` "{}@,/*?" = Right (reverse litR,col,s)
          | otherwise          = parseGlobLit (c:litR) (col + 1) cs
        parseGlobLit litR col "" = Right (reverse litR,col,"")

-- | Same as 'parseGlob', but calls 'error' upon parse failure.
parseGlob' :: String -> Glob
parseGlob' s =
  case parseGlob s of
    Left e -> error ("Text.Glob.parseGlob': " ++ e)
    Right r -> r
