package uk.ac.cdrc.data.utility.text

import scala.util.matching.Regex


trait Normalizer[U] extends PreprocessedAnalyzer[U] {
  val dictionary: Seq[(Regex, String)]

  abstract override def preprocess(s: String): String = substitute(super.preprocess(s), dictionary)

  def substitute(s: String, patterns: Seq[(Regex, String)]) =
    (s /: patterns) {case(x, (ptn, sub)) => ptn replaceAllIn (x, sub)}
}


trait NumberNormalizer[U] extends Normalizer[U]{
  override val dictionary = Seq(
    "g f f".r -> " 0 ",
    "^g(?=[^a-z])".r -> " 0 ",
    "\\bg f\\b".r -> " 0 ",
    "\\bf f\\b".r -> " 1 ",
    "^gnd(?=[^a-z])".r -> "0 ",
    "^grd(?=[^a-z])".r -> "0 ",
    "^gr(?=[^a-z])".r -> "0 ",
    "\\bfirst\\b".r -> " 1 ",
    "\\bsecond".r -> " 2 ",
    "\\bground\\b".r -> " 0 "
  )
}


trait TextNormalizer[U] extends Normalizer[U]{

  override val dictionary = Seq(
    // Flat abbr
    "\\bfl\\b".r -> " flat ",
    "g f f".r -> " ground floor flat ",
    "^g(?=[^a-z])".r -> " ground ",
    "\\bg f\\b".r -> " ground floor",
    "\\bf f\\b".r -> " first floor ",
    "\\bt f\\b".r -> " first floor ",
    "^gnd(?=[^a-z])".r -> "ground ",
    "^grd(?=[^a-z])".r -> "ground ",
    "^gr(?=[^a-z])".r -> "ground ",
    "1st".r -> "first",
    "2nd".r -> "second",
    //common abbr
    "\\bst\\b".r -> "street",
    "\\brd\\b".r -> "road",
    // from https://en.wikipedia.org/wiki/Postal_counties_of_the_United_Kingdom
    // England
    "beds$".r -> "bedfordshire",
    "berks$".r -> "berkshire",
    "bucks$".r -> "buckinghamshire",
    "cambs$".r -> "cambridgeshire",
    "co durham$".r -> "county durham",
    "derbys$".r -> "derbyshire",
    "e sussex$".r -> "east sussex",
    "glos$".r -> "gloucestershire",
    "hants$".r -> "hampshire",
    "herts$".r -> "hertfordshire",
    "lancs$".r -> "lancashire",
    "leics$".r -> "leicestershire",
    "lincs$".r -> "lincolnshire",
    "middx$".r -> "middlesex",
    "n humbs$".r -> "north humberside",
    "n yorks$".r -> "north yorkshire",
    "northants$".r -> "northamptonshire",
    "northd$".r -> "northumberland",
    "notts$".r -> "nottinghamshire",
    "oxon$".r -> "oxfordshire",
    "salop$".r -> "shropshire",
    "s humbs$".r -> "south humberside",
    "s yorks$".r -> "south yorkshire",
    "staffs$".r -> "staffordshire",
    "tyne & wear$".r -> "tyne and wear",
    "warks$".r -> "warwickshire",
    "w mids$".r -> "west midlands",
    "w sussex$".r -> "west sussex",
    "w yorks$".r -> "west yorkshire",
    "wilts$".r -> "wiltshire",
    "worcs$".r -> "worcestershire",

    // Wales
    "M Glam$".r -> "Mid Glamorgan",
    "S Glam$".r -> "South Glamorgan",
    "W Glam$".r -> "West Glamorgan",

    // Northern Ireland
    "Co Antrim$".r -> "County Antrim",
    "Co Armagh$".r -> "County Armagh",
    "Co Down$".r -> "County Down",
    "Co Fermanagh$".r -> "County Fermanagh",
    "Co Londonderry$".r -> "County Londonderry",
    "Co Tyrone$".r -> "County Tyrone"
  )

}
