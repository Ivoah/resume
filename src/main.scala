package net.ivoah.resume

import net.ivoah.vial.*
import org.rogach.scallop.*
import java.io.File

extension [T](i: Seq[T]) {
  def join(joiner: T): Seq[T] = i.flatMap(Seq(_, joiner)).dropRight(1)
}

@main
def main(args: String*): Unit = {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val host: ScallopOption[String] = opt[String](default = Some("127.0.0.1"))
    val port: ScallopOption[Int] = opt[Int](default = Some(2020))
    val socket: ScallopOption[String] = opt[String]()
    val verbose: ScallopOption[Boolean] = opt[Boolean]()
    val debug: ScallopOption[Boolean] = opt[Boolean]()

    conflicts(socket, List(host, port))
    verify()
  }

  val conf = Conf(args)
  implicit val logger: String => Unit = if (conf.verbose()) println else (msg: String) => ()
  
  val endpoints = Endpoints(conf.debug())
  val server = if (conf.socket.isDefined) {
    println(s"Using unix socket: ${conf.socket()}")
    Server(endpoints.router, conf.socket())
  } else {
    println(s"Using host/port: ${conf.host()}:${conf.port()}")
    Server(endpoints.router, (conf.host(), conf.port()))
  }
  server.serve()
}
