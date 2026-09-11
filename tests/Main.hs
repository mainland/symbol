module Main (main) where

import           Control.Concurrent    (forkFinally, killThread, newEmptyMVar,
                                        putMVar, readMVar, takeMVar)
import           Control.Exception     (ErrorCall, evaluate, finally, throwIO,
                                        try)
import           Control.Monad         (forM, forM_)
import           Data.String           (fromString)
import           Data.Symbol           (Symbol, intern, unintern)
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
