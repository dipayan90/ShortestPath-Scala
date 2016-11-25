package com.persist.uw.examples

import java.util.concurrent
import java.util.concurrent.TimeUnit

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.concurrent.duration._

case class Station(name: String)

case class Connection(from: Station, to: Station, time: FiniteDuration)

case class Path(connections: Seq[Connection]) {
  val time: FiniteDuration = connections.map(_.time).fold(0 seconds) { case (t1, t2) => t1 + t2 }

  def stations: Seq[Station] = connections.map(_.from)
}

//google graph algo : pragle used in spark graphx

case class Info(path: Option[Path])

class Trains {

  def readFile() : List[Connection]  = {
    val connections = new ListBuffer[Connection]
    val lines =  scala.io.Source.fromFile("data/trains.txt").getLines()
    while (lines.hasNext){
      val conns = lines.next().split(",")
      val timeSplits = conns(2).split(":")
      val time: FiniteDuration  = Some( Duration(timeSplits(0).toInt,TimeUnit.HOURS)  +  Duration(timeSplits(1).toInt,TimeUnit.MINUTES) ).collect { case d: FiniteDuration => d}.get
      connections += Connection(Station(conns(0)),Station(conns(1)),time)
    }
    connections.toList
  }


  def toParis(): Seq[(Station, Info)] = ???
}

object Trains {

  def main(args: Array[String]): Unit = {
    val t = new Trains
    t.readFile().foreach(print)
    val state = t.toParis()
    println("")
    for ((station, info) <- state) {
      val (p, t) = info.path match {
        case Some(p) =>
          val h = p.time.toHours
          val m = (p.time - h.hours).toMinutes
          val ts = f"$h%d:$m%02d"
          (p.stations.reverse.:+(Station("Paris")).map(_.name).mkString("(", ",", ")"), ts)
        case None => ("no path", "")
      }
      println(s"${station.name} to Paris $t: $p")
    }
    println("")
  }
}
