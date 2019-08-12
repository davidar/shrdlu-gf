incomplete concrete BlocksI of Blocks = open Syntax, Extend, Idiom, Lexicon, LexBlocks, Progr in {
lincat
  Message = Utt;
  Statement = S;
  Relative = RS;
  Determiner = Det;
  Verb = V2;
  Preverb = VV;
  Interrogative = IP;
  Adjective = A;
  Item = NP;
  Kind = CN;
  Quality = AP;
  Location = Adv;
  Time = Adv;
  QPredicate = QCl;
  RPredicate = RCl;
  SPredicate = Cl;
  VerbTense = Tense;
  Anteriority = Ant;
  Polarity = Pol;

lin
  Command verb item = mkUtt (mkImp (mkVP verb item));
  CommandLocation verb item loc = mkUtt (mkImp (mkVP (mkVP verb item) loc));
  Query tense ant pol pred = mkUtt (mkQS tense ant pol pred);
  Declare s = mkUtt s;

  Stmt tense ant pol pred = mkS tense ant pol pred;

  Rel tense ant pol pred = mkRS tense ant pol pred;
  RelAnd x y = mkRS (and_Conj | but_Conj) x y;

  ASg = a_Det;
  APlural = aPl_Det;
  The = the_Det;
  ThePlural = thePl_Det;
  That = that_Det;
  AtLeastOne = mkDet (mkCard at_least_AdN (mkCard (mkNumeral n1_Unit)));
  Two = mkDet (mkNumeral n2_Unit);
  Five = mkDet (mkNumeral n5_Unit);
  Both = both_Det;
  Any = mkDet any_Quant;
  AnyPlural = mkDet any_Quant pluralNum;
  Every = every_Det;

  Do = do_V2;
  Grasp = grasp_V2 | pick_up_V2;
  Put = put_V2;
  Find = find_V2;
  Contain = contain_V2;
  Hold = hold_V2;
  Support = support_V2;
  StackUp = stack_up_V2;
  SitOn = sit_on_V2;
  Touch = touch_V2;
  Clean = clean_V2 | clear_V2 | clean_off_V2 | clear_off_V2;
  Build = build_V2;

  Can = can_VV;
  StartTo = start_to_VV;

  What = what_IP;
  HowMany kind = mkIP how8many_IDet kind;
  Which kind = mkIP which_IQuant kind;

  Big = big_A | large_A;
  Small = small_A | little_A;
  Red = red_A;
  Green = green_A;
  Blue = blue_A;
  Tall = tall_A;
  Short = short_A;
  Narrow = narrow_A;
  Wide = wide_A;

  Thing det kind = mkNP det kind;
  It = it_NP;
  You = you_NP;
  They = they_NP;
  Something = something_NP;
  ThatItem = that_NP;
  OneItem = one_NP;
  R det kind r = mkNP det (mkCN kind r);
  BothAnd x y = mkNP (both7and_DConj | and_Conj) x y;
  EitherOr x y = mkNP (either7or_DConj | or_Conj) x y;
  SupportOf item
    = mkNP the_Det (mkCN support_N2 item)
    | mkNP (mkDet (Extend.GenNP item)) support_N -- genitive
    ;

  Mod quality kind = mkCN quality kind;
  Block = mkCN block_N;
  Pyramid = mkCN pyramid_N;
  Box = mkCN box_N;
  One = mkCN one_N;
  Table = mkCN table_N;
  Cube = mkCN cube_N;
  Object = mkCN (object_N | thing_N);
  Colour = mkCN colour_N;
  Stack = mkCN stack_N;
  Steeple = mkCN steeple_N;

  Qual adj = mkAP adj;
  MoreThan adj item = mkAP adj item;
  AsAs adj item = mkAP as_CAdv (mkAP adj) item;
  Most adj = mkAP (mkOrd adj);

  Behind item = mkAdv behind_Prep item;
  In item = mkAdv (inside_Prep | in_Prep | into_Prep) item;
  On item = mkAdv (on_top_of_Prep | on_Prep | onto_Prep) item;
  LeftOf item = mkAdv (to_the_left_of_Prep | left_of_Prep) item;

  Before s = mkAdv before_Subj s;
  While s = mkAdv (when_Subj | while_Subj) s;
  Then = then_Adv;
  Now = now_Adv;

  Polar pred = mkQCl pred;
  When pred = mkQCl when_IAdv pred;
  Why pred = mkQCl why_IAdv pred;
  How pred = mkQCl how_IAdv pred;
  QPred1 i verb item
    = mkQCl i (mkVP verb item)
    | mkQCl i (mkClSlash (mkCl item (passiveVP verb)) by8agent_Prep) -- passive
    | mkQCl i (progressiveVP (mkVP verb item)) -- progressive
    ;
  QPred2 i item verb
    = mkQCl i (mkClSlash item verb)
    | mkQCl i (passiveVP verb item) -- passive
    | mkQCl i (mkClSlash item (Progr.ProgrVPSlash (mkVPSlash verb))) -- progressive
    ;
  QPred2Time i item verb t
    = mkQCl i (mkClSlash (mkClSlash item verb) t)
    | mkQCl i (mkVP (passiveVP verb item) t) -- passive
    | mkQCl i (mkClSlash (mkClSlash item (Progr.ProgrVPSlash (mkVPSlash verb))) t) -- progressive
    ;
  QQual i quality = mkQCl i quality;
  QLocation i loc = mkQCl i loc;
  QIdentity i item = mkQCl i item;

  RPred1 verb item
    = mkRCl (that_RP) (mkVP verb item)
    | mkRCl (that_RP) (mkClSlash (mkCl item (passiveVP verb)) by8agent_Prep) -- passive
    | mkRCl (that_RP) (progressiveVP (mkVP verb item)) -- progressive
    ;
  RPred2 item verb
    = mkRCl (that_RP) (mkClSlash item verb)
    | mkRCl (that_RP) (passiveVP verb item) -- passive
    | mkRCl (that_RP) (mkClSlash item (Progr.ProgrVPSlash (mkVPSlash verb))) -- progressive
    ;
  RQual quality
    = mkRCl (that_RP) quality;
  RLocation loc
    = mkRCl (that_RP) loc;
  RITellYou verb
    = mkRCl (that_RP) (mkClSlash i_NP (mkVPSlash tell_to_V2V you_NP (mkVPSlash verb)));

  SPred s v o
    = mkCl s (mkVP v o)
    | mkCl s (Extend.ComplSlashPartLast (mkVPSlash v) o) -- separate particle from verb in phrasal verb
    | mkCl s (progressiveVP (mkVP v o)) -- progressive
    ;
  SPredLocation s v o loc = mkCl s (mkVP (mkVP v o) loc);
  SPredTime s v o t = mkCl s (mkVP (mkVP v o) t);
  SPred1 verb item = mkCl item (passiveVP verb);
  SQual item quality = mkCl item quality;
  SPreverb subject preverb verb item
    = mkCl subject preverb (mkVP verb item)
    | mkCl subject preverb (Extend.ComplSlashPartLast (mkVPSlash verb) item) -- separate particle
    | mkCl item preverb (passiveVP verb subject) -- passive
    ;
  SLocation item loc = mkCl item loc;
  SLocationTime item loc t = mkCl item (mkVP (mkVP loc) t);
  SExists item = mkCl item;
  SExistsTime item t = Idiom.ExistNPAdv item t;
  SIdentity x y = mkCl x y;
  SIdentityTime x y t = mkCl x (mkVP (mkVP y) t);

  Past = pastTense;
  Present = presentTense;
  Future = futureTense;
  Conditional = conditionalTense;

  Anterior = anteriorAnt;
  Simultaneous = simultaneousAnt;

  Negative = negativePol | Extend.UncontractedNeg;
  Positive = positivePol;
}

