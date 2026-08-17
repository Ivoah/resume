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

val flex = div(cls:="flex")

object Markdown {
  private val parser = Parser.builder().build()
  private val htmlRenderer = HtmlRenderer.builder().build()

  def render(markdown: String): Frag = raw(htmlRenderer.render(parser.parse(markdown)))
}

trait Renderable {
  def render: Frag
}

case class Header(
  name: String,
  links: Seq[String],
  location: String,
  email: String,
  phone: String
) extends Renderable derives YamlDecoder {
  def render: Frag = div(
    flex(
        h1(name),
        s"Location: ${location}"
    ),
    flex(
      span(links.toSeq.map(url => a(href:=url, url.stripPrefix("https://"))).join(StringFrag(" | "))),
      span(
        s"Email: ", a(href:=s"mailto:${email}", email),
        " | ",
        s"Phone: ", a(href:=s"tel:${phone}", phone)
      )
    )
  )
}

case class WithType(`type`: String) derives YamlDecoder

sealed trait Section(title: String, body: Frag*) extends Product with Serializable with Renderable {
  def render: Frag = div(cls:="section",
    h3(title),
    hr(),
    div(body)
  )
}

given YamlDecoder[Section] {
  def construct(node: Node)(implicit settings: LoadSettings): Either[ConstructError, Section] = {
    node.as[WithType]
    .map(_.`type`).flatMap {
      case "Summary" => node.as[Summary]
      case "Skills" => node.as[Skills]
      case "Professional Experience" => node.as[ProfessionalExperience]
      case "Education" => node.as[Education]
      case "Personal Projects" => node.as[PersonalProjects]
    }
    .left.map(_.asInstanceOf[ConstructError])
  }
}

case class Summary(content: String) extends Section("Summary", Markdown.render(content)) derives YamlDecoder

case class Skills(skills: Seq[Skills.Skill]) extends Section(
  "Technical Skills",
  div(style:="display: grid; grid-template-columns: max-content 16px auto;",
    skills.flatMap(skill => Seq(
      strong(skill.name), span(style:="justify-self: center;", ":"), span(skill.items.join(", "))
    ))
  )
) derives YamlDecoder
object Skills {
  case class Skill(name: String, items: Seq[String]) derives YamlDecoder
}

case class ProfessionalExperience(jobs: Seq[ProfessionalExperience.Job]) extends Section(
	"Professional Experience",
	for (job <- jobs) yield div(
		flex(strong(job.role), job.dates),
		flex(em(job.employer), job.location),
		ul(job.details.map(d => li(Markdown.render(d))))
	)
) derives YamlDecoder
object ProfessionalExperience {
	case class Job(role: String, dates: String, employer: String, location: String, details: Seq[String]) derives YamlDecoder
}

case class Education(schools: Seq[Education.School]) extends Section(
	"Education",
	for (school <- schools) yield div(
    flex(strong(school.degree), school.dates),
    flex(em(school.name), em(school.location)),
    ul(school.details.map(d => li(Markdown.render(d))))
  )
) derives YamlDecoder
object Education {
  case class School(name: String, location: String, dates: String, degree: String, details: Seq[String]) derives YamlDecoder
}

case class PersonalProjects(projects: Seq[PersonalProjects.Project]) extends Section(
	"Personal Projects",
	for (project <- projects) yield div(
    flex(strong(project.name), em(project.technology), span("GitHub: ", a(href:=s"https://github.com/${project.github}", project.github))),
    ul(project.details.map(d => li(Markdown.render(d))))
  )
) derives YamlDecoder
object PersonalProjects {
  case class Project(name: String, technology: String, github: String, details: Seq[String]) derives YamlDecoder
}

case class Page(sections: Seq[Section]) extends Renderable derives YamlDecoder {
  def render: Frag = sections.map(_.render)
}

case class Resume(
  header: Header,
  pages: Seq[Page]
) extends Renderable derives YamlDecoder {
  val _style = """
    @import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,200..900;1,200..900&display=swap');

    *, *::before, *::after {
      box-sizing: border-box;
    }

    *:not(dialog) {
      margin: 0;
    }

    html {
      background: #FCF5E5;
    }

    .page {
      width: 8.5in;
      height: 11in;
      margin: 16px auto;
      background: white;

      font-family: "Source Sans 3", sans-serif;
      padding: 16px;
      font-size: 10pt;

      @media print {
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

  def render: Frag = html(
    head(
      tag("style")(raw(_style)),
      tag("title")(header.name)
    ),
    body(
      for (page <- pages) yield div(cls:="page",
        header.render,
        page.render
      )
    )
  )
}

@main
def main(path: String): Unit = {
  Files.readString(Path.of(path)).as[Resume] match {
    case Left(err) => throw err
    case Right(resume) => println(resume.render)
  }
}
