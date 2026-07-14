package net.ivoah.resume

import scalatags.Text.all.*

import java.time.format.{DateTimeFormatter, FormatStyle}

object Templates {
  private val dateFormat: DateTimeFormatter = DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM)

  private def page(content: Frag*): String = doctype("html")(html(
    head(
      // link(rel:="icon", href:="/static/icon.svg"),
      link(rel:="stylesheet", href:="/static/style.css"),
      tag("title")(Details.name)
    ),
    body(content)
  )).render

  private def section(title: String)(content: Frag*) = div(cls:="section",
    h3(title),
    hr(),
    div(content)
  )

  private val flex = div(cls:="flex")

  def resume(): String = page(
    flex(
        h1(Details.name),
        s"Location: ${Details.location}"
    ),
    flex(
      span(Details.links.toSeq.map {
        case (name, url) => a(href:=url, name)
      }.join(StringFrag(" | "))),
      span(
        s"Email: ", a(href:=s"mailto:${Details.email}", Details.email),
        " | ",
        s"Phone: ", a(href:=s"tel:${Details.phone}", Details.phone)
      )
    ),

    section("Full Stack Developer")(
      Markdown.render(Details.summary)
    ),

    section("Technical Skills")(
      div(style:="display: grid; grid-template-columns: max-content 16px auto;",
        Details.skills.flatMap(skill => Seq(
          strong(skill.name), span(style:="justify-self: center;", ":"), span(skill.items.join(", "))
        ))
      )
    ),

    section("Professional Experience")(
      for (experience <- Details.experience) yield div(
        flex(strong(experience.role), experience.dates),
        flex(em(experience.employer), em(experience.location)),
        ul(experience.details.map(d => li(Markdown.render(d))))
      )
    ),

    section("Education")(
      for (education <- Details.education) yield div(
        flex(strong(education.school), education.location),
        flex(em(education.degree), em(education.dates)),
        ul(education.details.map(d => li(Markdown.render(d))))
      )
    ),

    section("Personal Projects")(
      for (project <- Details.projects) yield div(
        flex(strong(project.name), em(project.technology), span("GitHub: ", a(href:=s"https://github.com/${project.github}", project.github))),
        ul(project.details.map(d => li(Markdown.render(d))))
      )
    )
  )
}
