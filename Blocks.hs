module Blocks where

import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String  deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GAdjective =
   GBig 
 | GBlue 
 | GGreen 
 | GNarrow 
 | GRed 
 | GShort 
 | GSmall 
 | GTall 
 | GWide 
  deriving Show

data GAnteriority =
   GAnterior 
 | GSimultaneous 
  deriving Show

data GDeterminer =
   GAPlural 
 | GASg 
 | GAny 
 | GAnyPlural 
 | GAtLeastOne 
 | GBoth 
 | GEach 
 | GEvery 
 | GFive 
 | GThat 
 | GThe 
 | GThePlural 
 | GTwo 
  deriving Show

data GInterrogative =
   GHowMany GKind 
 | GWhat 
 | GWhich GKind 
  deriving Show

data GItem =
   GAnd GListItem 
 | GIt 
 | GMe 
 | GOneItem 
 | GOr GListItem 
 | GPartitive GDeterminer GItem 
 | GR GDeterminer GKind GRelative 
 | GSomething 
 | GSupportOf GItem 
 | GThatItem 
 | GThey 
 | GThing GDeterminer GKind 
 | GTopOf GItem 
 | GYou 
  deriving Show

data GKind =
   GBlock 
 | GBox 
 | GColour 
 | GCube 
 | GLoc 
 | GMod GQuality GKind 
 | GObject 
 | GOne 
 | GPyramid 
 | GStack 
 | GSteeple 
 | GTable 
  deriving Show

newtype GListItem = GListItem [GItem] deriving Show

newtype GListStatement = GListStatement [GStatement] deriving Show

data GLocation =
   GBehind GItem 
 | GIn GItem 
 | GLeftOf GItem 
 | GOn GItem 
 | GTowards GItem 
  deriving Show

data GMessage =
   GCommand GVerb GItem 
 | GCommandLocation GVerb GItem GLocation 
 | GDeclare GStatement 
 | GQuery GVerbTense GAnteriority GPolarity GQPredicate 
  deriving Show

data GPolarity =
   GNegative 
 | GPositive 
  deriving Show

data GPreverb =
   GCan 
 | GMust 
 | GStartTo 
  deriving Show

data GQPredicate =
   GHow GSPredicate 
 | GPolar GSPredicate 
 | GQIdentity GInterrogative GItem 
 | GQLocation GInterrogative GLocation 
 | GQLocationTime GInterrogative GLocation GTime 
 | GQPred1 GInterrogative GVerb GItem 
 | GQPred2 GInterrogative GItem GVerb 
 | GQPred2Time GInterrogative GItem GVerb GTime 
 | GQQual GInterrogative GQuality 
 | GWhen GSPredicate 
 | GWhy GSPredicate 
  deriving Show

data GQuality =
   GAsAs GAdjective GItem 
 | GMoreThan GAdjective GItem 
 | GMost GAdjective 
 | GQual GAdjective 
  deriving Show

data GRPredicate =
   GRITellYou GVerb 
 | GRLocation GLocation 
 | GRPred1 GVerb GItem 
 | GRPred2 GItem GVerb 
 | GRPreverbLocation GPreverb GLocation 
 | GRQual GQuality 
  deriving Show

data GRelative =
   GRel GVerbTense GAnteriority GPolarity GRPredicate 
 | GRelAnd GRelative GRelative 
  deriving Show

data GSPredicate =
   GSExists GItem 
 | GSExistsTime GItem GTime 
 | GSIdentity GItem GItem 
 | GSIdentityTime GItem GItem GTime 
 | GSLocation GItem GLocation 
 | GSLocationTime GItem GLocation GTime 
 | GSPred GItem GVerb GItem 
 | GSPred1 GVerb GItem 
 | GSPredLocation GItem GVerb GItem GLocation 
 | GSPredTime GItem GVerb GItem GTime 
 | GSPreverb GItem GPreverb GVerb GItem 
 | GSPreverbLocation GItem GPreverb GLocation 
 | GSQual GItem GQuality 
  deriving Show

data GStatement =
   GSAnd GListStatement 
 | GStmt GVerbTense GAnteriority GPolarity GSPredicate 
  deriving Show

data GTime =
   GBefore GStatement 
 | GInitially 
 | GNow 
 | GThen 
 | GWhile GStatement 
  deriving Show

data GVerb =
   GBuild 
 | GClean 
 | GContain 
 | GDo 
 | GFind 
 | GGrasp 
 | GHold 
 | GMove 
 | GPut 
 | GSitOn 
 | GStackUp 
 | GSupport 
 | GTouch 
  deriving Show

data GVerbTense =
   GConditional 
 | GFuture 
 | GPast 
 | GPresent 
  deriving Show


instance Gf GAdjective where
  gf GBig = mkApp (mkCId "Big") []
  gf GBlue = mkApp (mkCId "Blue") []
  gf GGreen = mkApp (mkCId "Green") []
  gf GNarrow = mkApp (mkCId "Narrow") []
  gf GRed = mkApp (mkCId "Red") []
  gf GShort = mkApp (mkCId "Short") []
  gf GSmall = mkApp (mkCId "Small") []
  gf GTall = mkApp (mkCId "Tall") []
  gf GWide = mkApp (mkCId "Wide") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Big" -> GBig 
      Just (i,[]) | i == mkCId "Blue" -> GBlue 
      Just (i,[]) | i == mkCId "Green" -> GGreen 
      Just (i,[]) | i == mkCId "Narrow" -> GNarrow 
      Just (i,[]) | i == mkCId "Red" -> GRed 
      Just (i,[]) | i == mkCId "Short" -> GShort 
      Just (i,[]) | i == mkCId "Small" -> GSmall 
      Just (i,[]) | i == mkCId "Tall" -> GTall 
      Just (i,[]) | i == mkCId "Wide" -> GWide 


      _ -> error ("no Adjective " ++ show t)

instance Gf GAnteriority where
  gf GAnterior = mkApp (mkCId "Anterior") []
  gf GSimultaneous = mkApp (mkCId "Simultaneous") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Anterior" -> GAnterior 
      Just (i,[]) | i == mkCId "Simultaneous" -> GSimultaneous 


      _ -> error ("no Anteriority " ++ show t)

instance Gf GDeterminer where
  gf GAPlural = mkApp (mkCId "APlural") []
  gf GASg = mkApp (mkCId "ASg") []
  gf GAny = mkApp (mkCId "Any") []
  gf GAnyPlural = mkApp (mkCId "AnyPlural") []
  gf GAtLeastOne = mkApp (mkCId "AtLeastOne") []
  gf GBoth = mkApp (mkCId "Both") []
  gf GEach = mkApp (mkCId "Each") []
  gf GEvery = mkApp (mkCId "Every") []
  gf GFive = mkApp (mkCId "Five") []
  gf GThat = mkApp (mkCId "That") []
  gf GThe = mkApp (mkCId "The") []
  gf GThePlural = mkApp (mkCId "ThePlural") []
  gf GTwo = mkApp (mkCId "Two") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "APlural" -> GAPlural 
      Just (i,[]) | i == mkCId "ASg" -> GASg 
      Just (i,[]) | i == mkCId "Any" -> GAny 
      Just (i,[]) | i == mkCId "AnyPlural" -> GAnyPlural 
      Just (i,[]) | i == mkCId "AtLeastOne" -> GAtLeastOne 
      Just (i,[]) | i == mkCId "Both" -> GBoth 
      Just (i,[]) | i == mkCId "Each" -> GEach 
      Just (i,[]) | i == mkCId "Every" -> GEvery 
      Just (i,[]) | i == mkCId "Five" -> GFive 
      Just (i,[]) | i == mkCId "That" -> GThat 
      Just (i,[]) | i == mkCId "The" -> GThe 
      Just (i,[]) | i == mkCId "ThePlural" -> GThePlural 
      Just (i,[]) | i == mkCId "Two" -> GTwo 


      _ -> error ("no Determiner " ++ show t)

instance Gf GInterrogative where
  gf (GHowMany x1) = mkApp (mkCId "HowMany") [gf x1]
  gf GWhat = mkApp (mkCId "What") []
  gf (GWhich x1) = mkApp (mkCId "Which") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "HowMany" -> GHowMany (fg x1)
      Just (i,[]) | i == mkCId "What" -> GWhat 
      Just (i,[x1]) | i == mkCId "Which" -> GWhich (fg x1)


      _ -> error ("no Interrogative " ++ show t)

instance Gf GItem where
  gf (GAnd x1) = mkApp (mkCId "And") [gf x1]
  gf GIt = mkApp (mkCId "It") []
  gf GMe = mkApp (mkCId "Me") []
  gf GOneItem = mkApp (mkCId "OneItem") []
  gf (GOr x1) = mkApp (mkCId "Or") [gf x1]
  gf (GPartitive x1 x2) = mkApp (mkCId "Partitive") [gf x1, gf x2]
  gf (GR x1 x2 x3) = mkApp (mkCId "R") [gf x1, gf x2, gf x3]
  gf GSomething = mkApp (mkCId "Something") []
  gf (GSupportOf x1) = mkApp (mkCId "SupportOf") [gf x1]
  gf GThatItem = mkApp (mkCId "ThatItem") []
  gf GThey = mkApp (mkCId "They") []
  gf (GThing x1 x2) = mkApp (mkCId "Thing") [gf x1, gf x2]
  gf (GTopOf x1) = mkApp (mkCId "TopOf") [gf x1]
  gf GYou = mkApp (mkCId "You") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "And" -> GAnd (fg x1)
      Just (i,[]) | i == mkCId "It" -> GIt 
      Just (i,[]) | i == mkCId "Me" -> GMe 
      Just (i,[]) | i == mkCId "OneItem" -> GOneItem 
      Just (i,[x1]) | i == mkCId "Or" -> GOr (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Partitive" -> GPartitive (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "R" -> GR (fg x1) (fg x2) (fg x3)
      Just (i,[]) | i == mkCId "Something" -> GSomething 
      Just (i,[x1]) | i == mkCId "SupportOf" -> GSupportOf (fg x1)
      Just (i,[]) | i == mkCId "ThatItem" -> GThatItem 
      Just (i,[]) | i == mkCId "They" -> GThey 
      Just (i,[x1,x2]) | i == mkCId "Thing" -> GThing (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "TopOf" -> GTopOf (fg x1)
      Just (i,[]) | i == mkCId "You" -> GYou 


      _ -> error ("no Item " ++ show t)

instance Gf GKind where
  gf GBlock = mkApp (mkCId "Block") []
  gf GBox = mkApp (mkCId "Box") []
  gf GColour = mkApp (mkCId "Colour") []
  gf GCube = mkApp (mkCId "Cube") []
  gf GLoc = mkApp (mkCId "Loc") []
  gf (GMod x1 x2) = mkApp (mkCId "Mod") [gf x1, gf x2]
  gf GObject = mkApp (mkCId "Object") []
  gf GOne = mkApp (mkCId "One") []
  gf GPyramid = mkApp (mkCId "Pyramid") []
  gf GStack = mkApp (mkCId "Stack") []
  gf GSteeple = mkApp (mkCId "Steeple") []
  gf GTable = mkApp (mkCId "Table") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Block" -> GBlock 
      Just (i,[]) | i == mkCId "Box" -> GBox 
      Just (i,[]) | i == mkCId "Colour" -> GColour 
      Just (i,[]) | i == mkCId "Cube" -> GCube 
      Just (i,[]) | i == mkCId "Loc" -> GLoc 
      Just (i,[x1,x2]) | i == mkCId "Mod" -> GMod (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Object" -> GObject 
      Just (i,[]) | i == mkCId "One" -> GOne 
      Just (i,[]) | i == mkCId "Pyramid" -> GPyramid 
      Just (i,[]) | i == mkCId "Stack" -> GStack 
      Just (i,[]) | i == mkCId "Steeple" -> GSteeple 
      Just (i,[]) | i == mkCId "Table" -> GTable 


      _ -> error ("no Kind " ++ show t)

instance Gf GListItem where
  gf (GListItem [x1,x2]) = mkApp (mkCId "BaseItem") [gf x1, gf x2]
  gf (GListItem (x:xs)) = mkApp (mkCId "ConsItem") [gf x, gf (GListItem xs)]
  fg t =
    GListItem (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseItem" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsItem" -> fg x1 : fgs x2


      _ -> error ("no ListItem " ++ show t)

instance Gf GListStatement where
  gf (GListStatement [x1,x2]) = mkApp (mkCId "BaseStatement") [gf x1, gf x2]
  gf (GListStatement (x:xs)) = mkApp (mkCId "ConsStatement") [gf x, gf (GListStatement xs)]
  fg t =
    GListStatement (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseStatement" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsStatement" -> fg x1 : fgs x2


      _ -> error ("no ListStatement " ++ show t)

instance Gf GLocation where
  gf (GBehind x1) = mkApp (mkCId "Behind") [gf x1]
  gf (GIn x1) = mkApp (mkCId "In") [gf x1]
  gf (GLeftOf x1) = mkApp (mkCId "LeftOf") [gf x1]
  gf (GOn x1) = mkApp (mkCId "On") [gf x1]
  gf (GTowards x1) = mkApp (mkCId "Towards") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Behind" -> GBehind (fg x1)
      Just (i,[x1]) | i == mkCId "In" -> GIn (fg x1)
      Just (i,[x1]) | i == mkCId "LeftOf" -> GLeftOf (fg x1)
      Just (i,[x1]) | i == mkCId "On" -> GOn (fg x1)
      Just (i,[x1]) | i == mkCId "Towards" -> GTowards (fg x1)


      _ -> error ("no Location " ++ show t)

instance Gf GMessage where
  gf (GCommand x1 x2) = mkApp (mkCId "Command") [gf x1, gf x2]
  gf (GCommandLocation x1 x2 x3) = mkApp (mkCId "CommandLocation") [gf x1, gf x2, gf x3]
  gf (GDeclare x1) = mkApp (mkCId "Declare") [gf x1]
  gf (GQuery x1 x2 x3 x4) = mkApp (mkCId "Query") [gf x1, gf x2, gf x3, gf x4]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "Command" -> GCommand (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "CommandLocation" -> GCommandLocation (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "Declare" -> GDeclare (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Query" -> GQuery (fg x1) (fg x2) (fg x3) (fg x4)


      _ -> error ("no Message " ++ show t)

instance Gf GPolarity where
  gf GNegative = mkApp (mkCId "Negative") []
  gf GPositive = mkApp (mkCId "Positive") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Negative" -> GNegative 
      Just (i,[]) | i == mkCId "Positive" -> GPositive 


      _ -> error ("no Polarity " ++ show t)

instance Gf GPreverb where
  gf GCan = mkApp (mkCId "Can") []
  gf GMust = mkApp (mkCId "Must") []
  gf GStartTo = mkApp (mkCId "StartTo") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Can" -> GCan 
      Just (i,[]) | i == mkCId "Must" -> GMust 
      Just (i,[]) | i == mkCId "StartTo" -> GStartTo 


      _ -> error ("no Preverb " ++ show t)

instance Gf GQPredicate where
  gf (GHow x1) = mkApp (mkCId "How") [gf x1]
  gf (GPolar x1) = mkApp (mkCId "Polar") [gf x1]
  gf (GQIdentity x1 x2) = mkApp (mkCId "QIdentity") [gf x1, gf x2]
  gf (GQLocation x1 x2) = mkApp (mkCId "QLocation") [gf x1, gf x2]
  gf (GQLocationTime x1 x2 x3) = mkApp (mkCId "QLocationTime") [gf x1, gf x2, gf x3]
  gf (GQPred1 x1 x2 x3) = mkApp (mkCId "QPred1") [gf x1, gf x2, gf x3]
  gf (GQPred2 x1 x2 x3) = mkApp (mkCId "QPred2") [gf x1, gf x2, gf x3]
  gf (GQPred2Time x1 x2 x3 x4) = mkApp (mkCId "QPred2Time") [gf x1, gf x2, gf x3, gf x4]
  gf (GQQual x1 x2) = mkApp (mkCId "QQual") [gf x1, gf x2]
  gf (GWhen x1) = mkApp (mkCId "When") [gf x1]
  gf (GWhy x1) = mkApp (mkCId "Why") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "How" -> GHow (fg x1)
      Just (i,[x1]) | i == mkCId "Polar" -> GPolar (fg x1)
      Just (i,[x1,x2]) | i == mkCId "QIdentity" -> GQIdentity (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QLocation" -> GQLocation (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "QLocationTime" -> GQLocationTime (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "QPred1" -> GQPred1 (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "QPred2" -> GQPred2 (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "QPred2Time" -> GQPred2Time (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "QQual" -> GQQual (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "When" -> GWhen (fg x1)
      Just (i,[x1]) | i == mkCId "Why" -> GWhy (fg x1)


      _ -> error ("no QPredicate " ++ show t)

instance Gf GQuality where
  gf (GAsAs x1 x2) = mkApp (mkCId "AsAs") [gf x1, gf x2]
  gf (GMoreThan x1 x2) = mkApp (mkCId "MoreThan") [gf x1, gf x2]
  gf (GMost x1) = mkApp (mkCId "Most") [gf x1]
  gf (GQual x1) = mkApp (mkCId "Qual") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AsAs" -> GAsAs (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MoreThan" -> GMoreThan (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Most" -> GMost (fg x1)
      Just (i,[x1]) | i == mkCId "Qual" -> GQual (fg x1)


      _ -> error ("no Quality " ++ show t)

instance Gf GRPredicate where
  gf (GRITellYou x1) = mkApp (mkCId "RITellYou") [gf x1]
  gf (GRLocation x1) = mkApp (mkCId "RLocation") [gf x1]
  gf (GRPred1 x1 x2) = mkApp (mkCId "RPred1") [gf x1, gf x2]
  gf (GRPred2 x1 x2) = mkApp (mkCId "RPred2") [gf x1, gf x2]
  gf (GRPreverbLocation x1 x2) = mkApp (mkCId "RPreverbLocation") [gf x1, gf x2]
  gf (GRQual x1) = mkApp (mkCId "RQual") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "RITellYou" -> GRITellYou (fg x1)
      Just (i,[x1]) | i == mkCId "RLocation" -> GRLocation (fg x1)
      Just (i,[x1,x2]) | i == mkCId "RPred1" -> GRPred1 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPred2" -> GRPred2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPreverbLocation" -> GRPreverbLocation (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "RQual" -> GRQual (fg x1)


      _ -> error ("no RPredicate " ++ show t)

instance Gf GRelative where
  gf (GRel x1 x2 x3 x4) = mkApp (mkCId "Rel") [gf x1, gf x2, gf x3, gf x4]
  gf (GRelAnd x1 x2) = mkApp (mkCId "RelAnd") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Rel" -> GRel (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "RelAnd" -> GRelAnd (fg x1) (fg x2)


      _ -> error ("no Relative " ++ show t)

instance Gf GSPredicate where
  gf (GSExists x1) = mkApp (mkCId "SExists") [gf x1]
  gf (GSExistsTime x1 x2) = mkApp (mkCId "SExistsTime") [gf x1, gf x2]
  gf (GSIdentity x1 x2) = mkApp (mkCId "SIdentity") [gf x1, gf x2]
  gf (GSIdentityTime x1 x2 x3) = mkApp (mkCId "SIdentityTime") [gf x1, gf x2, gf x3]
  gf (GSLocation x1 x2) = mkApp (mkCId "SLocation") [gf x1, gf x2]
  gf (GSLocationTime x1 x2 x3) = mkApp (mkCId "SLocationTime") [gf x1, gf x2, gf x3]
  gf (GSPred x1 x2 x3) = mkApp (mkCId "SPred") [gf x1, gf x2, gf x3]
  gf (GSPred1 x1 x2) = mkApp (mkCId "SPred1") [gf x1, gf x2]
  gf (GSPredLocation x1 x2 x3 x4) = mkApp (mkCId "SPredLocation") [gf x1, gf x2, gf x3, gf x4]
  gf (GSPredTime x1 x2 x3 x4) = mkApp (mkCId "SPredTime") [gf x1, gf x2, gf x3, gf x4]
  gf (GSPreverb x1 x2 x3 x4) = mkApp (mkCId "SPreverb") [gf x1, gf x2, gf x3, gf x4]
  gf (GSPreverbLocation x1 x2 x3) = mkApp (mkCId "SPreverbLocation") [gf x1, gf x2, gf x3]
  gf (GSQual x1 x2) = mkApp (mkCId "SQual") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "SExists" -> GSExists (fg x1)
      Just (i,[x1,x2]) | i == mkCId "SExistsTime" -> GSExistsTime (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SIdentity" -> GSIdentity (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "SIdentityTime" -> GSIdentityTime (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "SLocation" -> GSLocation (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "SLocationTime" -> GSLocationTime (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "SPred" -> GSPred (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "SPred1" -> GSPred1 (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "SPredLocation" -> GSPredLocation (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "SPredTime" -> GSPredTime (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "SPreverb" -> GSPreverb (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "SPreverbLocation" -> GSPreverbLocation (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "SQual" -> GSQual (fg x1) (fg x2)


      _ -> error ("no SPredicate " ++ show t)

instance Gf GStatement where
  gf (GSAnd x1) = mkApp (mkCId "SAnd") [gf x1]
  gf (GStmt x1 x2 x3 x4) = mkApp (mkCId "Stmt") [gf x1, gf x2, gf x3, gf x4]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "SAnd" -> GSAnd (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Stmt" -> GStmt (fg x1) (fg x2) (fg x3) (fg x4)


      _ -> error ("no Statement " ++ show t)

instance Gf GTime where
  gf (GBefore x1) = mkApp (mkCId "Before") [gf x1]
  gf GInitially = mkApp (mkCId "Initially") []
  gf GNow = mkApp (mkCId "Now") []
  gf GThen = mkApp (mkCId "Then") []
  gf (GWhile x1) = mkApp (mkCId "While") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Before" -> GBefore (fg x1)
      Just (i,[]) | i == mkCId "Initially" -> GInitially 
      Just (i,[]) | i == mkCId "Now" -> GNow 
      Just (i,[]) | i == mkCId "Then" -> GThen 
      Just (i,[x1]) | i == mkCId "While" -> GWhile (fg x1)


      _ -> error ("no Time " ++ show t)

instance Gf GVerb where
  gf GBuild = mkApp (mkCId "Build") []
  gf GClean = mkApp (mkCId "Clean") []
  gf GContain = mkApp (mkCId "Contain") []
  gf GDo = mkApp (mkCId "Do") []
  gf GFind = mkApp (mkCId "Find") []
  gf GGrasp = mkApp (mkCId "Grasp") []
  gf GHold = mkApp (mkCId "Hold") []
  gf GMove = mkApp (mkCId "Move") []
  gf GPut = mkApp (mkCId "Put") []
  gf GSitOn = mkApp (mkCId "SitOn") []
  gf GStackUp = mkApp (mkCId "StackUp") []
  gf GSupport = mkApp (mkCId "Support") []
  gf GTouch = mkApp (mkCId "Touch") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Build" -> GBuild 
      Just (i,[]) | i == mkCId "Clean" -> GClean 
      Just (i,[]) | i == mkCId "Contain" -> GContain 
      Just (i,[]) | i == mkCId "Do" -> GDo 
      Just (i,[]) | i == mkCId "Find" -> GFind 
      Just (i,[]) | i == mkCId "Grasp" -> GGrasp 
      Just (i,[]) | i == mkCId "Hold" -> GHold 
      Just (i,[]) | i == mkCId "Move" -> GMove 
      Just (i,[]) | i == mkCId "Put" -> GPut 
      Just (i,[]) | i == mkCId "SitOn" -> GSitOn 
      Just (i,[]) | i == mkCId "StackUp" -> GStackUp 
      Just (i,[]) | i == mkCId "Support" -> GSupport 
      Just (i,[]) | i == mkCId "Touch" -> GTouch 


      _ -> error ("no Verb " ++ show t)

instance Gf GVerbTense where
  gf GConditional = mkApp (mkCId "Conditional") []
  gf GFuture = mkApp (mkCId "Future") []
  gf GPast = mkApp (mkCId "Past") []
  gf GPresent = mkApp (mkCId "Present") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Conditional" -> GConditional 
      Just (i,[]) | i == mkCId "Future" -> GFuture 
      Just (i,[]) | i == mkCId "Past" -> GPast 
      Just (i,[]) | i == mkCId "Present" -> GPresent 


      _ -> error ("no VerbTense " ++ show t)


