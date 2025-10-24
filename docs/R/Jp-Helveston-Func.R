# From John Paul Helveston

# to use any of these functions in distill articles on this website, call `source(file.path("R", "functions.R"))` at the top of the .Rmd file

# copied from https://github.com/jhelvy/jhelvy.com
# for more details, see https://www.jhelvy.com/posts/2021-03-25-customizing-distill-with-htmltools-and-css/
make_icon <- function(icon) {
  return(htmltools::tag("i", list(class = icon)))
}

# copied from https://github.com/jhelvy/jhelvy.com
# for more details, see https://www.jhelvy.com/posts/2021-03-25-customizing-distill-with-htmltools-and-css/
make_icon_text <- function(icon, text) {
  return(htmltools::HTML(paste0(make_icon(icon), " ", text)))
}

# copied from https://github.com/jhelvy/jhelvy.com
# for more details, see https://www.jhelvy.com/posts/2021-03-25-customizing-distill-with-htmltools-and-css/
# Creates the html to make a button to an external link
icon_link <- function(icon, text, url, new_tab = TRUE) {
  target <- if (new_tab) ' target="_blank" rel="noopener"' else ""
  sprintf(
    '<a href="%s"%s aria-label="%s (%s)"><i class="%s" aria-hidden="true"></i> %s</a>',
    url, target, text, if (grepl("\\.pdf$", url)) "PDF" else "link", icon, text
  )
}