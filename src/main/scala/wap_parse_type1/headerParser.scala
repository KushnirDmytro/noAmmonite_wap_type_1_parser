package wap_parse_type1


import fastparse.all._

import parserUtils._

object headerParser {

  val dumm = 1


  val newLineSpaceTab = P(CharsWhileIn(" \t\n") | "")

  // Header log file metrics
  val summaryInterface = P((
    newLineSpaceTab ~

      // row 1
      CharsWhile(_ != ' ').! ~ space ~ "is" ~ space ~
      CharsWhile(_ != ',').! ~ ", line protocol " ~
      CharsWhile(_ != '\n').! ~ newLineSpaceTab ~

      // row 2
      "Hardware is " ~ CharsWhile(_ != ',').! ~
      ", address is " ~ CharsWhile(_ != ' ').! ~
      " (bia " ~ CharsWhile(_ != ')').! ~ ")" ~ newLineSpaceTab ~

      // row 3
      "MTU " ~ number ~ " bytes, BW " ~ number ~
      " Kbit, DLY " ~ number ~ " usec," ~ newLineSpaceTab ~

      "reliability " ~ CharsWhile(_ != ',').! ~
      ", txload " ~ CharsWhile(_ != ',').! ~
      ", rxload " ~ CharsWhile(_ != '\n').! ~ newLineSpaceTab ~

      "Encapsulation " ~ CharsWhile(_ != ',').! ~
      (", Vlan ID" ~ space ~ CharsWhile(_ != ',').!).? ~
      ", loopback " ~ CharsWhile(_ != '\n').! ~ newLineSpaceTab ~

      ("Full-duplex, " ~ number ~ "Mb/s, MII").? ~ newLineSpaceTab ~

      "ARP type: " ~ CharsWhile(_ != ',').! ~
      ", ARP Timeout " ~ CharsWhile(_ != '\n').! ~ newLineSpaceTab ~

      "Last input " ~ CharsWhile(_ != ',').! ~
      ", output " ~ CharsWhile(_ != ',').! ~
      ", output hang " ~ CharsWhile(_ != '\n').! ~ newLineSpaceTab ~

      // TODO check if this part is constant
      "Last clearing of \"show interface\" counters never" ~ newLineSpaceTab ~

      "Input queue: " ~ CharsWhile(_ != ' ').! ~
      " (size/max/drops/flushes); Total output drops: " ~
      number ~ newLineSpaceTab ~

      "Queueing strategy: " ~ CharsWhile(_ != '\n').! ~ newLineSpaceTab ~

      "Output queue: " ~ CharsWhile(_ != ' ').! ~ " (size/max)" ~ newLineSpaceTab ~

      "5 minute input rate " ~ number ~
      " bits/sec, " ~ number ~ " packets/sec" ~ newLineSpaceTab ~

      "5 minute output rate " ~ number ~
      " bits/sec, " ~ number ~ " packets/sec" ~ newLineSpaceTab ~

      number ~ " packets input, " ~ CharsWhile(_ != ' ').! ~ " bytes" ~ ", ".? ~
      (number ~ " no buffer").? ~ newLineSpaceTab ~

      "Received " ~ number ~ " broadcasts, " ~
      number ~ " runts, " ~ number ~ " giants, " ~
      number ~ " throttles" ~ newLineSpaceTab ~

      number ~ " input errors, " ~ number ~ " CRC, " ~
      number ~ " frame, " ~ number ~ " overrun, " ~
      number ~ " ignored" ~ newLineSpaceTab ~

      (number ~ " watchdog").? ~ newLineSpaceTab ~

      number ~ " input packets with dribble condition detected" ~ newLineSpaceTab ~

      number ~ " packets output, " ~ CharsWhile(_ != ' ').! ~ " bytes, " ~
      number ~ " underruns" ~ newLineSpaceTab ~

      number ~ " output errors, " ~ number ~ " collisions, " ~
      number ~ " interface resets" ~ newLineSpaceTab ~

      number ~ " babbles, " ~ number ~ " late collision, " ~
      number ~ " deferred" ~ newLineSpaceTab ~

      number ~ " lost carrier, " ~ number ~ " no carrier" ~ newLineSpaceTab ~

      number ~ " output buffer failures, " ~
      number ~ " output buffers swapped out" ~ newLineSpaceTab
    ).map({ case ((
    (interface, interfaceStatus, lineProtocolStatus, hardware,
    macAddress, macBiaAddress, mtu, bw, dly, reliability, txload,
    rxload, encapsulation, vlanId, loopback, fullDuplex, arpType, arpTimeout,
    lastInput, lastOutput, outputHang, inputQueue), totalOutputDrops,
    queueingStrategy, outputQueue, min5InputRateBytes, min5InputRatePackets,
    min5OutputRateBytes, min5OutputRatePackets, packetsInput, bytesInput,
    noBuffer, broadcasts, runts, giants, throttles, inputErrors,
    crc, frame, overrun, ignored, watchdog, inputPacketsDribble), packetsOutput,
  bytesOutput, underruns, outputErrors, collisions, interfaceResets,
  babbles, lateCollision, deferred, lostCarrier, noCarrier,
  outputBufferFailures, outputBufferSwappedOut) =>
    Seq(
      ("interface" -> interface),
      ("interfaceStatus" -> interfaceStatus),
      ("lineProtocolStatus" -> lineProtocolStatus),
      ("hardware" -> hardware),
      ("macAddress" -> macAddress),
      ("macBiaAddress" -> macBiaAddress),
      ("mtu" -> mtu),
      ("bw" -> bw),
      ("dly" -> dly),
      ("reliability" -> reliability),
      ("txload" -> txload),
      ("rxload" -> rxload),
      ("encapsulation" -> encapsulation),
      ("vlanId" -> vlanId.getOrElse("")),
      ("loopback" -> loopback),
      ("fullDuplex" -> fullDuplex.getOrElse("")),
      ("arpType" -> arpType),
      ("arpTimeout" -> arpTimeout),
      ("lastInput" -> lastInput),
      ("lastOutput" -> lastOutput),
      ("outputHang" -> outputHang),
      ("inputQueue" -> inputQueue),
      ("totalOutputDrops" -> totalOutputDrops),
      ("queueingStrategy" -> queueingStrategy),
      ("outputQueue" -> outputQueue),
      ("min5InputRateBytes" -> min5InputRateBytes),
      ("min5InputRatePackets" -> min5InputRatePackets),
      ("min5OutputRateBytes" -> min5OutputRateBytes),
      ("min5OutputRatePackets" -> min5OutputRatePackets),
      ("packetsInput" -> packetsInput),
      ("bytesInput" -> bytesInput),
      ("noBuffer" -> noBuffer.getOrElse("")),
      ("broadcasts" -> broadcasts),
      ("runts" -> runts),
      ("giants" -> giants),
      ("throttles" -> throttles),
      ("inputErrors" -> inputErrors),
      ("crc" -> crc),
      ("frame" -> frame),
      ("overrun" -> overrun),
      ("ignored" -> ignored),
      ("watchdog" -> watchdog.getOrElse("")),
      ("inputPacketsDribble" -> inputPacketsDribble),
      ("packetsOutput" -> packetsOutput),
      ("bytesOutput" -> bytesOutput),
      ("underruns" -> underruns),
      ("outputErrors" -> outputErrors),
      ("collisions" -> collisions),
      ("collisions" -> interfaceResets),
      ("babbles" -> babbles),
      ("lateCollision" -> lateCollision),
      ("deferred" -> deferred),
      ("lostCarrier" -> lostCarrier),
      ("noCarrier" -> noCarrier),
      ("outputBufferFailures" -> outputBufferFailures),
      ("outputBufferSwappedOut" -> outputBufferSwappedOut)
    ).toMap
  }))

}