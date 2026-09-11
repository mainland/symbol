module Main (main) where

import           Control.Concurrent    (forkFinally, killThread, newEmptyMVar,
                                        putMVar, readMVar, takeMVar)
import           Control.Exception     (ErrorCall, evaluate, finally, throwIO,
                                        try)
import           Control.Monad         (forM, forM_)
import           Data.Data             (Data, fromConstrB, gmapM, gmapT,
                                        toConstr)
import           Data.Maybe            (fromMaybe)
import           Data.String           (fromString)
import           Data.Symbol           (Symbol, intern, unintern)
import           Data.Typeable         (Typeable, cast)
import           Test.Tasty            (TestTree, defaultMain, localOption,
                                        mkTimeout, testGroup)
import           Test.Tasty.HUnit      (Assertion, assertFailure, testCase,
                                        (@?=))
import           Test.Tasty.QuickCheck (UnicodeString (..), testProperty)
import           Text.Read             (readMaybe)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "symbol"
    [ testGroup "interning"
        [ testProperty "preserves strings" $ \(UnicodeString s) ->
            unintern (intern s) == s
        , testProperty "equality agrees with string equality" $
            \(UnicodeString s) (UnicodeString t) ->
                (intern s == intern t) == (s == t)
        , testProperty "repeated interning preserves identity" $ \(UnicodeString s) ->
            intern s == intern (reverse (reverse s))
        , testProperty "IsString agrees with intern" $ \(UnicodeString s) ->
            fromString s == intern s
        ]
    , testGroup "ordering laws"
        [ testProperty "compare agrees with equality" $
            \(UnicodeString s) (UnicodeString t) ->
                (compare (intern s) (intern t) == EQ) == (s == t)
        , testProperty "comparison is antisymmetric" $
            \(UnicodeString s) (UnicodeString t) ->
                compare (intern s) (intern t) == invert (compare (intern t) (intern s))
        , testProperty "ordering is transitive" $
            \(UnicodeString s) (UnicodeString t) (UnicodeString u) ->
                let x = intern s
                    y = intern t
                    z = intern u
                in not (x <= y && y <= z) || x <= z
        ]
    , testGroup "Read and Show"
        [ testProperty "Show agrees with String" $ \(UnicodeString s) ->
            show (intern s) == show s
        , testProperty "symbols round-trip" $ \(UnicodeString s) ->
            readMaybe (show (intern s)) == Just (intern s)
        , testProperty "lists of symbols round-trip" $ \strings ->
            let symbols = map (intern . getUnicodeString) strings
            in readMaybe (show symbols) == Just symbols
        , testGroup "edge cases"
            [ testCase (show s) $ do
                unintern (intern s) @?= s
                readMaybe (show (intern s)) @?= Just (intern s)
            | s <- ["", "\NUL", "\n\t", "quote\"slash\\", "\x3bb\x1f600"]
            ]
        , testCase "reads parenthesized strings" $
            readMaybe "((\"alpha\"))" @?= Just (intern "alpha")
        , testCase "reads character lists" $
            readMaybe "['a', 'b']" @?= Just (intern "ab")
        , testCase "leaves trailing input" $
            reads "\"alpha\" rest" @?= [(intern "alpha", " rest")]
        , testCase "rejects malformed input" $
            (readMaybe "\"unterminated" :: Maybe Symbol) @?= Nothing
        ]
    , testGroup "Data instance"
        [ testCase "generic string updates preserve interning" $ do
            let original = intern "generic:original"
                expected = intern "generic:changed"
                changed = gmapT (replace ("generic:changed" :: String)) original
            unintern changed @?= "generic:changed"
            changed @?= expected
            (changed == original) @?= False
            compare changed expected @?= EQ
        , testCase "generic integer updates cannot forge identifiers" $ do
            let original = intern "generic:original"
                changed = gmapT (replace (0 :: Int)) original
            changed @?= original
            unintern changed @?= unintern original
        , testProperty "generic identity traversal preserves symbols" $
            \(UnicodeString s) ->
                let sym = gmapT id (intern s)
                in sym == intern s && unintern sym == s
        , testCase "monadic generic updates preserve interning" $ do
            changed <- gmapM (return . replace ("generic:changed" :: String))
                (intern "generic:original")
            changed @?= intern "generic:changed"
            unintern changed @?= "generic:changed"
        , testCase "generic construction interns its argument" $ do
            let rebuilt = fromConstrB (stringField "generic:rebuilt")
                    (toConstr (intern "generic:original")) :: Symbol
            rebuilt @?= intern "generic:rebuilt"
            unintern rebuilt @?= "generic:rebuilt"
        ]
    , localOption (mkTimeout 10000000) $ testGroup "evaluation and concurrency"
        [ testCase "nested interning does not deadlock" $ do
            sym <- evaluate (intern ("outer:" ++ unintern (intern "inner")))
            unintern sym @?= "outer:inner"
        , testCase "input exceptions leave the table usable" $ do
            result <- try (evaluate (intern ('x' : error "bad input")))
                :: IO (Either ErrorCall Symbol)
            case result of
                Left _ -> return ()
                Right _ -> assertFailure "intern did not evaluate the entire input"
            sym <- evaluate (intern "after-exception")
            unintern sym @?= "after-exception"
        , testCase "concurrent interning preserves identity" concurrentInterning
        ]
    ]

replace :: (Typeable a, Typeable b) => a -> b -> b
replace replacement value = fromMaybe value (cast replacement)

stringField :: Data a => String -> a
stringField value = fromMaybe (error "Unexpected non-string Symbol field") (cast value)

invert :: Ordering -> Ordering
invert LT = GT
invert EQ = EQ
invert GT = LT

concurrentInterning :: Assertion
concurrentInterning = do
    gate <- newEmptyMVar
    workers <- forM [0 .. 7 :: Int] $ \offset -> do
        done <- newEmptyMVar
        tid <- forkFinally
            (do readMVar gate
                forM [0 .. 255 :: Int] $ \i -> do
                    let s = "concurrent:" ++ show ((i + offset) `mod` 128)
                    sym <- evaluate (intern s)
                    return (s, sym))
            (putMVar done)
        return (tid, done)
    flip finally (mapM_ (killThread . fst) workers) $ do
        putMVar gate ()
        results <- mapM (takeMVar . snd) workers
        symbols <- mapM (either throwIO return) results
        forM_ (concat symbols) $ \(s, sym) -> do
            unintern sym @?= s
            sym @?= intern s
