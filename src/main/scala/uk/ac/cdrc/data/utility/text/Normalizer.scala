package uk.ac.cdrc.data.utility.text

import scala.util.matching.Regex


trait Normalizer {
  self =>
  val dictionary: Seq[(Regex, String)]
  def normalize(s: String): String = {
    lazy val input: Stream[String] = s #:: output
    lazy val output: Stream[String] = (input zip dictionary) map {x => x._2._1 replaceAllIn (x._1, x._2._2)}
    output.reduce((a: String, b: String) => b)
  }

  def ++(that: Normalizer): Normalizer = new Normalizer{
    override val dictionary: Seq[(Regex, String)] = self.dictionary ++ that.dictionary
  }
}


object FlatAbbrNormalizer extends Normalizer{
  override val dictionary = Seq(
    "\\bfl\\b".r -> "flat",
    "^g f f".r -> "ground floor flat",
    "^g f".r -> "ground floor",
    "^f f".r -> "first floor ",
    "^gnd ".r -> "ground",
    "^grd ".r -> "ground",
    "^gr ".r -> "ground",
    "1st".r -> "1",
    "2nd".r -> "2",
    "first".r -> "1",
    "second".r -> "2",
    "ground".r -> "0"
  )
}

object CommonAbbrNormalizer extends Normalizer{

  override val dictionary = Seq(
    "\\bst\\b".r -> "street"
  )

}

object CountyAbbrNormalizer extends Normalizer {

  // from https://en.wikipedia.org/wiki/Postal_counties_of_the_United_Kingdom
  override val dictionary = Seq(
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
