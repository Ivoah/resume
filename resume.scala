#!/usr/bin/env -S scala-cli shebang

//> using scala 3.8.4
//> using jvm 21

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

case class Section(title: String, items: Seq[SectionItem]) extends Renderable derives YamlDecoder {
  def render: Frag = div(cls:="section",
    h3(title),
    hr(),
    div(items.map(_.render))
  )
}

sealed trait SectionItem extends Renderable

given YamlDecoder[SectionItem] {
  def construct(node: Node)(implicit settings: LoadSettings): Either[ConstructError, SectionItem] = {
    node
      .as[WithType].map(_.`type`).flatMap {
        case "Markdown" => node.as[Markdown]
        case "Skills" => node.as[Skills]
        case "Experience" => node.as[Experience]
        case "Project" => node.as[Project]
      }
      .left.map(_.asInstanceOf[ConstructError])
  }
}

case class Markdown(content: String) extends SectionItem derives YamlDecoder {
  def render: Frag = Markdown.render(content)
}

case class Skills(skills: Seq[Skills.Skill]) extends SectionItem derives YamlDecoder {
  def render: Frag = div(style:="display: grid; grid-template-columns: max-content 16px auto;",
    skills.flatMap(skill => Seq(
      strong(skill.name), span(style:="justify-self: center;", ":"), span(skill.items.join(", "))
    ))
  )
}
object Skills {
  case class Skill(name: String, items: Seq[String]) derives YamlDecoder
}

case class Experience(name: String, dates: String, at: String, location: String, details: Seq[String]) extends SectionItem derives YamlDecoder {
  def render: Frag = div(
    flex(strong(name), dates),
    flex(em(at), location),
    ul(details.map(d => li(Markdown.render(d))))
  )
}

case class Project(name: String, technology: String, github: String, details: Seq[String]) extends SectionItem derives YamlDecoder {
  def render: Frag = div(
    flex(strong(name), em(technology), span("GitHub: ", a(href:=s"https://github.com/${github}", github))),
    ul(details.map(d => li(Markdown.render(d))))
  )
}

case class Page(sections: Seq[Section]) extends Renderable derives YamlDecoder {
  def render: Frag = sections.map(_.render)
}

case class Resume(
  header: Header,
  pages: Seq[Page]
) extends Renderable derives YamlDecoder {
  def render: Frag = html(
    head(
      tag("style")(raw("""
        @import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,200..900;1,200..900&display=swap');

        *, *::before, *::after {box-sizing: border-box;}
        *:not(dialog) {margin: 0;}

        html {background: #FCF5E5; font-family: "Source Sans 3", sans-serif;}
        a {color: inherit; text-decoration: underline;}
        h1 {font-variant: small-caps;}
        h3 {color: blue; font-variant: small-caps;}

        .page {
          position: relative;
          width: 8.5in;
          height: 11in;
          margin: 16px auto;
          background: white;

          padding: 16px;
          font-size: 10pt;

          @media print {
            margin: 0;
          }
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

        .pageCount {position: absolute; bottom: 16px; width: 100%; text-align: center;}
      """)),
      tag("title")(header.name)
    ),
    body(
      for ((page, i) <- pages.zipWithIndex) yield div(cls:="page",
        header.render,
        page.render,
        if (pages.length > 1) div(cls:="pageCount", s"${i+1}/${pages.length}")
        else frag()
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
