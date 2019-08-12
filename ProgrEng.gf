concrete ProgrEng of Progr = CatEng ** open Prelude, Coordination, ResEng in {
lin ProgrVPSlash vp =
  insertObjc (\\a => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a)
    (predAux auxBe ** {c2 = vp.c2; gapInMiddle = vp.gapInMiddle; missingAdv = vp.missingAdv});
}
