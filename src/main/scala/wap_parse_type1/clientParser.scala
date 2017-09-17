
// Paradise macros needed for Circe's @JsonCodec annotation
//import $plugin.$ivy.`org.scalamacros:paradise_2.12.2:2.1.0`
package wap_parse_type1
// imports related to JSON encoding
import io.circe._
import io.circe.syntax._
import io.circe.generic.JsonCodec
import io.circe.generic.auto._
import io.circe.generic.semiauto._

// imports provided by Ammonite
import fastparse.all._


import parserUtils._


object clientParser {

    @JsonCodec case class ClientCWAPMetrics(
                                             clientNumber: Long,
                                             clientMAC: String,
                                             pakIn: Long,
                                             bytesIn: Long,
                                             pakOut: Long,
                                             bytesOut: Long,
                                             dup: Long,
                                             decrpytErr: Long,
                                             micMismatch: Long,
                                             micMiss: Long,
                                             txRetries: Long,
                                             dataRetries: Long,
                                             rtsRetries: Long,
                                             signalStrength: Long,
                                             signalQuality: Long)


    @JsonCodec case class ClientCWAPInterface(
                                               interface: String,
                                               clients: Seq[ClientCWAPMetrics])


    @JsonCodec case class ClientCWAPAssociation(
                                                 clientIP: String,
                                                 clientMac: String,
                                                 userAgent: String)


    val clientMac = P(CharsWhileIn(('0' to '9') ++ ('a' to 'z')).rep(sep = ".").!)
    val clientRow1 = P(
      numberInt ~ "-" ~ clientMac ~ " pak in " ~
        numberInt ~ " bytes in " ~ numberInt ~ " pak out " ~
        numberInt ~ " bytes out " ~ numberInt)

    val clientRow2 = P(
      "dup " ~ numberInt ~ " decrpyt err " ~ numberInt ~ " mic mismatch " ~
        numberInt ~ " mic miss " ~ numberInt)

    val clientRow3 = P(
      "tx retries " ~ numberInt ~ " data retries " ~
        numberInt ~ " rts retries " ~ numberInt)

    val clientRow4 = P("signal strength " ~ numberInt ~ " signal quality " ~ numberInt)

    val clientRecord = P(
      (clientRow1 ~/ "\n".rep ~/ clientRow2 ~/ "\n".rep ~/
        clientRow3 ~/ "\n".rep ~/ clientRow4 ~/ "\n".rep).
        map({ case (
          clientNumber, clientMAC, pakIn, bytesIn, pakOut, bytesOut,
          (dup, decrpytErr, micMismatch, micMiss),
          (txRetries, dataRetries, rtsRetries),
          (signalStrength, signalQuality))
        => ClientCWAPMetrics(
          clientNumber, clientMAC,
          pakIn, bytesIn,
          pakOut, bytesOut,
          dup, decrpytErr,
          micMismatch, micMiss,
          txRetries, dataRetries,
          rtsRetries, signalStrength,
          signalQuality)
        }))


    val interface = P(CharsWhile(_ != ':').! ~ ": -- Client Traffic --")

    val interfaceBlock = P(
      (interface ~/ "\n".rep ~/ "Clients:" ~/ "\n".rep ~/
        clientRecord.rep(sep = "\n".rep))
        .map({
          case (interface, clients) => ClientCWAPInterface(interface, clients)
        }))

    val ipAddress = P("IP Address : " ~/ (CharIn('0' to '9').rep(1).rep(exactly = 4, sep = ".") | "unknown").! ~/ "; ")
    val userAgent = P("User-Agent : " ~/ CharsWhile(_ != '\n').! ~/ "\n")
    val macAddress = P(
      "MAC Address : " ~/
        (CharsWhileIn(('0' to '9') ++ ('A' to 'F')).rep(exactly = 6, sep = ":") | "null").! ~/ "; ")

    val association = P(
      (ipAddress ~/ macAddress ~/ userAgent)
        .map({ case (ipAddress, macAddress, userAgent) =>
          ClientCWAPAssociation(ipAddress, macAddress, userAgent)
        }))

    val cwapClientFileBody = P(
      !eof ~ (interfaceBlock.rep(sep = "\n".rep).? ~/
        ((!association ~ CharsWhile(_ != '\n')).rep(sep = "\n".rep) | "\n".rep) ~/
        "\n".rep ~/ association.rep.?))



}