abstract Blocks = {
flags startcat = Message;

cat
  Message;
  Statement;
  [Statement]{2};
  Relative;
  Determiner;
  Verb;
  Preverb;
  Interrogative;
  Adjective;
  Item;
  [Item]{2};
  Kind;
  Quality;
  Location;
  Time;
  QPredicate;
  RPredicate;
  SPredicate;
  VerbTense;
  Anteriority;
  Polarity;

fun
  Command : Verb -> Item -> Message;
  CommandLocation : Verb -> Item -> Location -> Message;
  Query : VerbTense -> Anteriority -> Polarity -> QPredicate -> Message;
  Declare : Statement -> Message;

  Stmt : VerbTense -> Anteriority -> Polarity -> SPredicate -> Statement;
  SAnd : ListStatement -> Statement;

  Rel : VerbTense -> Anteriority -> Polarity -> RPredicate -> Relative;
  RelAnd : Relative -> Relative -> Relative;

  ASg, APlural, The, ThePlural, That, AtLeastOne, Two, Five, Both, Any, AnyPlural, Every, Each : Determiner;

  Do, Grasp, Put, Find, Contain, Hold, Support, StackUp, SitOn, Touch, Clean, Build, Move : Verb;

  Can, StartTo, Must : Preverb;

  What : Interrogative;
  HowMany, Which : Kind -> Interrogative;

  Big, Small, Red, Green, Blue, Tall, Short, Narrow, Wide : Adjective;

  Thing : Determiner -> Kind -> Item;
  It, Me, You, They, Something, ThatItem, OneItem : Item;
  R : Determiner -> Kind -> Relative -> Item;
  And, Or : [Item] -> Item;
  SupportOf : Item -> Item;
  TopOf : Item -> Item;

  Mod : Quality -> Kind -> Kind;
  Block, Pyramid, Box, One, Table, Cube, Object, Colour, Stack, Steeple, Loc : Kind;

  Qual : Adjective -> Quality;
  MoreThan, AsAs : Adjective -> Item -> Quality;
  Most : Adjective -> Quality;

  Behind, In, On, LeftOf, Towards : Item -> Location;

  Before, While : Statement -> Time;
  Then, Now, Initially : Time;

  Polar, When, Why, How : SPredicate -> QPredicate;
  QPred1 : Interrogative -> Verb -> Item -> QPredicate;
  QPred2 : Interrogative -> Item -> Verb -> QPredicate;
  QPred2Time : Interrogative -> Item -> Verb -> Time -> QPredicate;
  QQual : Interrogative -> Quality -> QPredicate;
  QLocation : Interrogative -> Location -> QPredicate;
  QIdentity : Interrogative -> Item -> QPredicate;

  RPred1 : Verb -> Item -> RPredicate;
  RPred2 : Item -> Verb -> RPredicate;
  RQual : Quality -> RPredicate;
  RLocation : Location -> RPredicate;
  RITellYou : Verb -> RPredicate;
  RPreverbLocation : Preverb -> Location -> RPredicate;

  SPred : Item -> Verb -> Item -> SPredicate;
  SPredLocation : Item -> Verb -> Item -> Location -> SPredicate;
  SPredTime : Item -> Verb -> Item -> Time -> SPredicate;
  SPred1 : Verb -> Item -> SPredicate;
  SQual : Item -> Quality -> SPredicate;
  SPreverb : Item -> Preverb -> Verb -> Item -> SPredicate;
  SPreverbLocation : Item -> Preverb -> Location -> SPredicate;
  SLocation : Item -> Location -> SPredicate;
  SLocationTime : Item -> Location -> Time -> SPredicate;
  SExists : Item -> SPredicate;
  SExistsTime : Item -> Time -> SPredicate;
  SIdentity : Item -> Item -> SPredicate;
  SIdentityTime : Item -> Item -> Time -> SPredicate;

  Past, Present, Future, Conditional : VerbTense;

  Anterior, Simultaneous : Anteriority;

  Negative, Positive : Polarity;
}
