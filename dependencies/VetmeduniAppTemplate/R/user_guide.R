# Compile trusted application Markdown; never execute its code blocks.
user_guide_document <- function(file, ns = identity, asset_prefix = "guide-assets") {
  markdown <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  doc <- xml2::read_html(commonmark::markdown_html(markdown, extensions = TRUE))
  headings <- xml2::xml_find_all(doc, "//h2 | //h3")
  titles <- xml2::xml_text(headings)
  slugs <- gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(titles)))
  anchors <- make.unique(slugs, sep = "-")
  ids <- paste0(ns("section"), "-", anchors)
  xml2::xml_set_attr(headings, "id", ids)
  assets <- xml2::xml_find_all(doc, "//img[@src] | //a[@href]")
  for (node in assets) {
    attribute <- if (xml2::xml_name(node) == "img") "src" else "href"
    value <- xml2::xml_attr(node, attribute)
    if (grepl("^assets/", value)) {
      xml2::xml_set_attr(node, attribute, sub("^assets/", paste0(asset_prefix, "/"), value))
      if (attribute == "href" && length(xml2::xml_find_all(node, ".//img"))) {
        xml2::xml_set_attr(node, "target", "_blank")
        xml2::xml_set_attr(node, "rel", "noopener")
      }
    }
    if (attribute == "href" && startsWith(value, "#")) {
      target <- match(utils::URLdecode(sub("^#", "", value)), anchors)
      if (!is.na(target)) xml2::xml_set_attr(node, attribute, paste0("#", ids[target]))
    }
  }
  list(html = paste(as.character(xml2::xml_children(xml2::xml_find_first(doc, "//body"))), collapse = "\n"),
    contents = data.frame(title = titles, id = ids,
      level = as.integer(sub("h", "", xml2::xml_name(headings)))))
}

mod_user_guide_ui <- function(id, file = "docs/user-guide.md", title = "User guide",
                              description = "Contents", asset_prefix = "guide-assets") {
  ns <- shiny::NS(id)
  guide <- tryCatch(user_guide_document(file, ns, asset_prefix), error = function(e)
    list(html = paste0("<h1>", htmltools::htmlEscape(title),
      "</h1><p>The guide could not be loaded.</p>"),
      contents = data.frame(title = character(), id = character(), level = integer())))
  contents <- guide$contents
  app_layout(sidebar = app_sidebar(title = title, description = description, id = ns("sidebar"),
    shiny::tags$nav(class = "guide-toc", `aria-label` = paste(title, "contents"),
      shiny::tags$ul(lapply(seq_len(nrow(contents)), function(i)
        shiny::tags$li(class = if (contents$level[i] == 3L) "guide-toc-subsection" else NULL,
          shiny::tags$a(href = paste0("#", contents$id[i]), contents$title[i])))))),
    shiny::tags$article(id = ns("article"), class = "user-guide-content", shiny::HTML(guide$html)))
}
