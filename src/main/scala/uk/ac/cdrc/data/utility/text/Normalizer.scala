package uk.ac.cdrc.data.utility.text

import scala.util.matching.Regex


trait Normalizer[U] extends PreProcessor[U] {

  val dictionary: Seq[(Regex, String)]

  override def preProcess(s: String): String = {
    lazy val input: Stream[String] = s #:: output
    lazy val output: Stream[String] = (input zip dictionary) map {x => x._2._1 replaceAllIn (x._1, x._2._2)}
    output.reduce((_: String, b: String) => b)
  }
}


trait CommonNormalizer[U] extends Normalizer[U]{
  override val dictionary = Seq(
    // Flat abbr
    "\\bfl\\b".r -> " flat ",
    "g f f".r -> " ground floor flat ",
    "\\bg f\\b".r -> " ground floor",
    "\\bf f\\b".r -> " first floor ",
    "^gnd\\b".r -> "ground ",
    "^grd\\b".r -> "ground ",
    "^gr\\b".r -> "ground ",
    "first".r -> "1st",
    "second".r -> "2nd",
    "ground".r -> "0ground",
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
