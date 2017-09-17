
// Paradise macros needed for Circe's @JsonCodec annotation
//import $plugin.$ivy.`org.scalamacros:paradise_2.12.2:2.1.0`

// imports related to JSON encoding
package wap_parse_type1


import fastparse.all._
import io.circe.generic.JsonCodec

import scala.util.Try

object parserUtils {

    @JsonCodec case class CWAPFileHeader(
                                          source: String,
                                          tail: String,
                                          acpuTime: Option[String],
                                          lat: Option[Double],
                                          lon: Option[Double],
                                          alt_m: Option[Int],
                                          agl_ft: Option[Int],
                                          flightNumber: Option[String],
                                          cityPair: Option[String],
                                          flightDuration: Option[String],
                                          flightPhase: Option[String],
                                          linkType: Option[String],
                                          cwapId: Option[Int],
                                          absSoftware: String)


    val space = P(CharsWhileIn(" \t"))
    val stars = P((space.rep ~ "*".rep(1) ~ space.rep ~ "\n".rep).rep(1))
    val source = P(StringIn("SM_80211_Client_CWAP", "SM_80211_CWAP").! ~ " Report File")
    val tail = P(" for Tail " ~ (CharsWhile(_ != ',').! | "".!) ~ ", ")

    val csv = P((
      CharsWhile(_ != ',').!.?.rep(exactly = 8, sep = ", ") ~
        ", " ~ CharsWhile(c => c != ',' && c != ' ').!.? ~
        ", ".? ~ CharsWhile(_ != ' ').!.?
      ).map({ case (seq, phase, linkType) => seq ++ Seq(phase, linkType) }))

    val version = P("ABS Software " ~/ CharsWhile(c => c != '*' && c != ' ').!)
    val cwapIdInStars = P(stars ~ "CWAP " ~ CharIn("123456789").!.map(_.toInt) ~ stars)
    val acpuNumber = P(
      "ACPU Part Number " ~/ CharsWhile(c => c != '*' && c != ' ') ~ "\n".rep)

    val fileHeader = P(
      (stars ~ source ~ tail ~ csv ~ cwapIdInStars.? ~ stars.? ~
        version ~ stars ~ stars.? ~ acpuNumber.? ~ stars.?).
        map({ case (
          source,
          tail,
          Seq(
          acpuTime, lat, lon, alt_m, agl_ft, flightNumber,
          cityPair, flightDuration, flightPhase, linkType),
          cwapId,
          absSoftware)
        => CWAPFileHeader(
          source,
          tail,
          acpuTime,
          Try(lat.get.toDouble).toOption,
          Try(lon.get.toDouble).toOption,
          Try(alt_m.get.toInt).toOption,
          Try(agl_ft.get.toInt).toOption,
          flightNumber,
          cityPair,
          flightDuration,
          flightPhase,
          linkType,
          cwapId,
          absSoftware)
        }))

    val cwapType = P("CwapType, " ~ CharIn("1234").! ~ "\n".rep)

    val digits = P(CharsWhileIn("0123456789"))
    val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
    val fractional = P("." ~ digits)
    val integral = P("0" | CharIn('1' to '9') ~ digits.?)

    val number = P((CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!)
    val numberInt = P((CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(_.toLong))

    val eof = P("\n".rep ~ stars ~ "END OF FILE" ~ stars ~ "\n".rep)

}