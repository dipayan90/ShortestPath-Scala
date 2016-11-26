package com.persist.uw.examples

import java.util.concurrent.TimeUnit

import scala.collection.mutable.ListBuffer
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

  def getDirectConnections(station: Station) : List[Connection] = {
    connections.filter(_.from.equals(station))
  }

  def getDuration(start : Station , destination: Station) : Duration = {
    val conns = connections.filter(e => ( e.from.equals(start) && e.to.equals(destination)) || (e.from.equals(destination) && e.to.equals(start)))
    if(conns.nonEmpty){
      conns.head.time
    }else{
      Duration.Inf
    }
  }

  def findMinimumFromMap(trackerMap: Map[(Station,Station),(Duration,Station)], from: Station) : Station = {
    val res = trackerMap.toSeq.filter(_._1._1.equals(from)).min(Ordering.by[((Station,Station),(Duration,Station)),Duration](_._2._1))
    res._1._2
  }

  def toCover(stationsAlreadyCovered : List[Station]) : List[Station] = {
    def contains(station: Station,stationList : List[Station]) : Boolean = {
      for(stn <- stationList) {
        if(stn.equals(station)){
           return true
        }
      }
      false
    }
    stations.filter(e => !contains(e,stationsAlreadyCovered))
  }

  def findColumnarMinimum(to : Station,trackerMap: Map[(Station,Station),(Duration,Station)]) : (Duration, Station) = {
    if(trackerMap.isEmpty || !trackerMap.toSeq.exists(_._1._2.equals(to))){
      (Duration.Inf,to)
    }else{
      val minimumStation : ((Station, Station), (Duration, Station)) = trackerMap.toSeq.filter(_._1._2.equals(to)).min(Ordering.by[((Station,Station),(Duration,Station)),Duration](_._2._1))
      minimumStation._2
    }
  }

  val connections : List[Connection] = readFile()
  val stations:  List[Station] = ( connections.map(_.from).distinct ::: connections.map(_.to).distinct).distinct

  // start with paris and then create a matrix with the values
  // map of (start Station,end Station) - > (Weight,Via Station)
  def implementDjktrasAlgorithm() = {
    val baseStation = Station("Paris")
    var stationsAlreadyVisited = List.empty[Station]
    // initially add the base station to the list of stations already visited
    stationsAlreadyVisited =  baseStation :: stationsAlreadyVisited

    // main tracker to track the shortest paths
    var trackerMap = Map.empty[(Station,Station),(Duration,Station)]

    while(stations.length > stationsAlreadyVisited.length){
      for(stn <- toCover(stationsAlreadyVisited)){
        val duration = getDuration(stationsAlreadyVisited.head,stn)
        val columnMin = findColumnarMinimum(stn,trackerMap)
        if(columnMin._1 < duration){
          trackerMap = trackerMap + ((stationsAlreadyVisited.head,stn) -> columnMin)
        }else{
          trackerMap = trackerMap + ((stationsAlreadyVisited.head,stn) -> (duration,stationsAlreadyVisited.head))
        }
      }
      System.out.println("################")
      // print from axis
      System.out.println("Currently visiting :  "+stationsAlreadyVisited.head.name)
      System.out.println("**************Connections in this level *********")
      // print map now for the from axis
      trackerMap.filter(_._1._1.equals(stationsAlreadyVisited.head)).foreach(print)
      //add the next min to the from axis
      stationsAlreadyVisited = findMinimumFromMap(trackerMap,stationsAlreadyVisited.head) :: stationsAlreadyVisited
      //next minimum is
      System.out.println(" \n Next minimum is : "+ stationsAlreadyVisited.head.name)
    }


    // Print tracker map here and see the output

  }

  def toParis(): Seq[(Station, Info)] = ???
}

object Trains {

  def main(args: Array[String]): Unit = {
    val t = new Trains
    t.connections.foreach(print)
    t.implementDjktrasAlgorithm()
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
