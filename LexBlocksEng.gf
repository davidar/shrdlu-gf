instance LexBlocksEng of LexBlocks = open SyntaxEng, ParadigmsEng, ExtraEng, IrregEng, MorphoEng in {
oper
  but_Conj = mkConj "but";

  any_Quant = ExtraEng.any_Quant;

  both_Det = MorphoEng.mkDeterminer plural "both";
  each_Det = MorphoEng.mkDeterminer singular "each";

  build_V2 = mkV2 (mkV "build");
  clean_off_V2 = mkV2 (partV (mkV "clean") "off");
  clean_V2 = mkV2 (mkV "clean");
  clear_off_V2 = mkV2 (partV (mkV "clear") "off");
  clear_V2 = mkV2 (mkV "clear");
  contain_V2 = mkV2 (mkV "contain");
  grasp_V2 = mkV2 (mkV "grasp");
  move_V2 = mkV2 (mkV "move");
  pick_up_V2 = mkV2 (partV (mkV "pick") "up");
  sit_on_V2 = mkV2 (partV (mkV "sit") "on");
  stack_up_V2 = mkV2 (partV (mkV "stack") "up");
  support_V2 = mkV2 (mkV "support");
  touch_V2 = mkV2 (mkV "touch");

  start_to_VV = mkVV (mkV "start");

  tell_to_V2V = mkV2V IrregEng.tell_V noPrep to_Prep;

  one_NP = mkNP (mkN "one");

  block_N = mkN "block";
  box_N = mkN "box";
  colour_N = mkN "colour";
  cube_N = mkN "cube";
  location_N = mkN "location";
  object_N = mkN "object";
  one_N = mkN "one";
  pyramid_N = mkN "pyramid";
  stack_N = mkN "stack";
  steeple_N = mkN "steeple" "steeples";
  support_N = mkN "support";
  thing_N = mkN "thing";
  top_N = mkN "top" "tops";

  support_N2 = mkN2 support_N;
  top_N2 = mkN2 top_N;

  large_A = mkA "large";
  little_A = mkA "little";
  tall_A = mkA "tall";

  inside_Prep = mkPrep "inside";
  into_Prep = mkPrep "into";
  left_of_Prep = mkPrep ["left of"];
  on_top_of_Prep = mkPrep ["on top of"];
  onto_Prep = mkPrep "onto";
  to_the_left_of_Prep = mkPrep ["to the left of"];

  before_Subj = mkSubj "before";
  while_Subj = mkSubj "while";

  then_Adv = mkAdv "then";
  initially_Adv = mkAdv "initially";

  that_RP = which_RP | ExtraEng.that_RP;
}
