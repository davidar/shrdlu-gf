{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List
import Control.Monad hiding (join)

import PGF

import Blocks

main :: IO ()
main = do
  gr <- readPGF "Blocks.pgf"
  loop (translate gr)

loop trans = do
  s <- getLine
  putStrLn s
  if s == "quit" then putStrLn "bye" else do
    forM_ (trans s) $ \t -> do
      --putStrLn $ show t
      putStrLn $ transMessage t
    putStrLn ""
    loop trans

translate :: PGF -> String -> [GMessage]
translate gr s = case parseAllLang gr (startCat gr) s of
  (lg,ts):_ -> map fg ts
  _ -> error "NO PARSE"

conj :: String -> String -> String
conj x "" = x
conj "" y = y
conj x y = x ++" "++ y

join :: String -> String -> String
join r x =
  if (' ' ==) `any` x
  then r ++"["++ x ++"]"
  else r ++"."++ x

rev :: String -> String
rev r = if last r == '\'' then init r else r ++"'"

pol :: GPolarity -> String -> String
pol GPositive x = x
pol GNegative x = "~"++ x

agg :: String -> String -> String
agg f x = "#"++ f ++"("++ x ++")"

sup :: String -> String -> String -> String
sup f a x = "#"++ f ++"("++ a ++", "++ x ++")"

transMessage :: GMessage -> String
transMessage = \case
  GCommand GBuild x -> transItem x ++"!"
  GCommand GFind x -> transItem x ++"?"
  GCommand v (GThing GTwo x) ->
    transVerb v ++"("++ transKind x ++", "++ transKind x ++")!"
  GCommand v (GAnd (GListItem [GThing GBoth x, y])) ->
    transVerb v ++"("++ transKind x ++", "++ transKind x ++", "++ transItem y ++")!"
  GCommand v (GAnd (GListItem [x,y])) ->
    transVerb v ++"("++ transItem x ++", "++ transItem y ++")!"
  GCommand v x -> transVerb v ++"("++ transItem x ++")!"
  GCommandLocation GPut x (GIn y) ->
    "in("++ transItem x ++", "++ transItem y ++")!"
  GCommandLocation GPut x (GOn y) ->
    "on("++ transItem x ++", "++ transItem y ++")!"
  GDeclare (GStmt _ _ _ (GSIdentity x y)) -> transItem x ++"("++ transItem y ++")."
  GQuery _ _ p q -> (++"?") $ case q of
    GPolar s -> agg "any" $ transSPredicate s
    GHow s -> "how" `join` transSPredicate s
    GWhy s -> "why" `join` transSPredicate s
    GQIdentity (GWhich GColour) x -> "colour" `join` transItem x
    GQPred1 (GWhich x) v y -> transKind x `conj` (transVerb v `join` transItem y)
    GQPred1 GWhat v y -> transVerb v `join` transItem y
    GQPred2 GWhat x v -> rev (transVerb v) `join` transItem x
    GQPred2Time (GHowMany x) GYou v t -> agg "count" $ (transVerb v `join` transKind x) `conj` transTime t
    GQPred2Time GWhat x v t -> rev (transVerb v) ++"["++ transItem x ++", "++ transTime t ++"]"
    GQLocation (GHowMany x) y ->
      agg "count" $ transKind x `conj` pol p (transLocation y)
    GQLocationTime (GHowMany x) y t ->
      agg "count" $ transKind x `conj` pol p (transLocationTime y t)

transVerb :: GVerb -> String
transVerb = \case
  GClean -> "cleanoff"
  GContain -> "contains"
  GGrasp -> "holding"
  GHold -> "holding"
  GSitOn -> "on"
  GStackUp -> "stack"
  GSupport -> "supports"
  GTouch -> "pickup"

transItem :: GItem -> String
transItem = \case
  GIt -> "that"
  GThatItem -> "that"
  GThing GThe (GMod (GMost a) x) -> sup "most" (transAdjective a) (transKind x)
  GThing GThat x -> "that" `conj` transKind x
  GThing GTwo x -> "pair("++ transKind x ++", "++ transKind x ++")"
  GThing _ x -> transKind x
  GPartitive GAtLeastOne GThey -> "that"
  GR GThe (GMod (GMost a) x) p -> sup "most" (transAdjective a) $ transKind x `conj` transRelative p
  GR _ x p -> transKind x `conj` transRelative p
  GOr (GListItem xs) -> intercalate " | " $ map transItem xs
  GAnd (GListItem xs) -> "set("++ intercalate ", " (map transItem xs) ++")"
  GSupportOf x -> transVerb GSupport `join` transItem x

transKind :: GKind -> String
transKind = \case
  GOne -> ""
  GObject -> ""
  GBlock -> "block"
  GBox -> "box"
  GCube -> "cube"
  GPyramid -> "pyramid"
  GStack -> "stack"
  GSteeple -> "steeple"
  GTable -> "table"
  GMod a x -> transQuality a `conj` transKind x

transQuality :: GQuality -> String
transQuality = \case
  GQual a -> transAdjective a
  GMoreThan a (GThing GEvery x) -> sup "each" (transAdjective a) (transKind x)
  GMoreThan a x -> transAdjective a `join` transItem x

transAdjective :: GAdjective -> String
transAdjective = \case
  GBig -> "big"
  GBlue -> "blue"
  GGreen -> "green"
  GNarrow -> "narrow"
  GRed -> "red"
  GShort -> "short"
  GSmall -> "small"
  GTall -> "tall"
  GWide -> "wide"

transRelative :: GRelative -> String
transRelative = \case
  GRel _ _ GPositive r -> case r of
    GRQual a -> transQuality a
    GRLocation x -> transLocation x
    GRPred1 v x -> transVerb v `join` transItem x
    GRPred2 GYou v -> transVerb v
    GRPred2 x v -> rev (transVerb v) `join` transItem x
    GRITellYou v -> "history$goal$"++ transVerb v
  GRel _ _ GNegative (GRQual (GAsAs a x)) -> rev (transAdjective a) `join` transItem x
  GRelAnd x y -> transRelative x `conj` transRelative y

transLocation :: GLocation -> String
transLocation = \case
  GBehind x -> "behind" `join` transItem x
  GIn x -> "in" `join` transItem x
  GOn x -> "above" `join` transItem x

transSPredicate :: GSPredicate -> String
transSPredicate = \case
  GSPred GYou GDo x -> transItem x
  GSPred GYou v y -> transVerb v `join` transItem y
  GSPred x v y -> transItem x `conj` (transVerb v `join` transItem y)
  GSPred1 v x -> transVerb v `join` transItem x
  GSPredTime GYou v x a -> (transVerb v `join` transItem x) `conj` transTime a
  GSQual x a -> transItem x `conj` transQuality a
  GSExists x -> transItem x
  GSExistsTime x t -> transItem x `conj` ("holds" `join` transTime t)

transTime :: GTime -> String
transTime = \case
  GBefore s -> "before" `join` transStatement s
  GWhile s -> "during" `join` transStatement s
  GThen -> "during.that"
  GNow -> "now"

transStatement :: GStatement -> String
transStatement = \case
  GStmt _ _ _ s -> case s of
    GSPred GYou GDo x -> transItem x
    GSPredLocation GYou GPut x (GOn y) -> "put_on["++ transItem x ++", "++ transItem y ++"]"
    GSPreverb GYou GStartTo v x -> transVerb v `join` transItem x

transLocationTime :: GLocation -> GTime -> String
transLocationTime = \case
  GLeftOf x -> \t -> "left_of["++ transItem x ++"; "++ transTime t ++"]"
