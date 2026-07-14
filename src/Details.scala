package net.ivoah.resume

import org.virtuslab.yaml.*

import scala.io.Source

private case class YamlDetails(
  name: String,
  links: Map[String, String],
  location: String,
  email: String,
  phone: String,
  summary: String,
  skills: Seq[Skill],
  experience: Seq[Experience],
  education: Seq[Education],
  projects: Seq[Project]
) derives YamlDecoder

case class Skill(name: String, items: Seq[String]) derives YamlDecoder
case class Experience(role: String, dates: String, employer: String, location: String, details: Seq[String]) derives YamlDecoder
case class Education(school: String, location: String, dates: String, degree: String, details: Seq[String]) derives YamlDecoder
case class Project(name: String, technology: String, github: String, details: Seq[String]) derives YamlDecoder

val Details = Source.fromResource("details.yaml").getLines().mkString("\n").as[YamlDetails] match {
  case Left(err) => throw err
  case Right(details) => details
}
