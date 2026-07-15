#!/usr/bin/env -S scala-cli shebang

//> using dep com.lihaoyi::scalatags:0.13.1
//> using dep org.virtuslab::scala-yaml:0.3.2
//> using dep org.commonmark:commonmark:0.29.0

import org.virtuslab.yaml.*
import java.nio.file.{Path, Files}
import scalatags.Text.all.*
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer

extension [T](i: Seq[T]) {
  def join(joiner: T): Seq[T] = i.flatMap(Seq(_, joiner)).dropRight(1)
}

object Markdown {
  private val parser = Parser.builder().build()
  private val htmlRenderer = HtmlRenderer.builder().build()

  def render(markdown: String): Frag = raw(htmlRenderer.render(parser.parse(markdown)))
}


private case class Details(
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

val _style = """
@import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:ital@0;1&display=swap');

*, *::before, *::after {
  box-sizing: border-box;
}

*:not(dialog) {
  margin: 0;
}

html {
  background: black;
}

body {
  width: 8.5in;
  height: 11in;
  margin: 16px auto;
  background: white;

  font-family: "Source Sans 3", sans-serif;
  padding: 16px;
  font-size: 10pt;
}

@media print {
  body {
    margin: 0;
  }
}

a {
  color: inherit;
  text-decoration: underline;
}

h1 {
  font-variant: small-caps;
}

h3 {
  color: blue;
  font-variant: small-caps;
}

.flex {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
}

.section {
  margin: 8px 0;
  > div {
    margin: 8px;
    > div {
      margin: 8px 0;
    }
  }
}
"""

val flex = div(cls:="flex")

def section(title: String)(content: Frag*) = div(cls:="section",
  h3(title),
  hr(),
  div(content)
)

def resume(details: Details): String = doctype("html")(html(
  head(
    tag("style")(raw(_style)),
    tag("title")(details.name)
  ),
  body(
    flex(
        h1(details.name),
        s"Location: ${details.location}"
    ),
    flex(
      span(details.links.toSeq.map {
        case (name, url) => a(href:=url, name)
      }.join(StringFrag(" | "))),
      span(
        s"Email: ", a(href:=s"mailto:${details.email}", details.email),
        " | ",
        s"Phone: ", a(href:=s"tel:${details.phone}", details.phone)
      )
    ),

    section("Full Stack Developer")(
      Markdown.render(details.summary)
    ),

    section("Technical Skills")(
      div(style:="display: grid; grid-template-columns: max-content 16px auto;",
        details.skills.flatMap(skill => Seq(
          strong(skill.name), span(style:="justify-self: center;", ":"), span(skill.items.join(", "))
        ))
      )
    ),

    section("Professional Experience")(
      for (experience <- details.experience) yield div(
        flex(strong(experience.employer), experience.location),
        flex(em(experience.role), experience.dates),
        ul(experience.details.map(d => li(Markdown.render(d))))
      )
    ),

    section("Education")(
      for (education <- details.education) yield div(
        flex(strong(education.school), education.location),
        flex(em(education.degree), em(education.dates)),
        ul(education.details.map(d => li(Markdown.render(d))))
      )
    ),

    section("Personal Projects")(
      for (project <- details.projects) yield div(
        flex(strong(project.name), em(project.technology), span("GitHub: ", a(href:=s"https://github.com/${project.github}", project.github))),
        ul(project.details.map(d => li(Markdown.render(d))))
      )
    )
  )
)).render

@main
def main(detailsPath: String): Unit = {
  Files.readString(Path.of(detailsPath)).as[Details] match {
    case Left(err) => throw err
    case Right(details) => println(resume(details))
  }
}
