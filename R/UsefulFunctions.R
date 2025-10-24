####################################
# Some utilities 
####################################

#' Convert Latin characters with diacritics to HTML entities
#'
#' Replaces common French/German accented characters (e.g., "é", "ö", "ß")
#' with their corresponding HTML named entities (e.g., &eacute;, &ouml;, &szlig;).
#' Useful when generating raw HTML to ensure correct rendering across browsers/encodings.
#'
#' @param x Character vector. Can be length > 1.
#' @return Character vector of same length as `x` with replacements applied.
#'
#' @details
#' - This function targets specific non-ASCII characters; it does **not** escape
#'   reserved HTML symbols like `&`, `<`, `>`. Use `htmltools::htmlEscape()` upstream
#'   if you need general HTML escaping.
#' - Replacement is literal (no regex metacharacters involved).
#'
#' @examples
#' char2html("À bientôt, Grüße aus Köln & co.")
#' # " &Agrave; bient&ocirc;t, Gr&uuml;&szlig;e aus K&ouml;ln & co."

char2html <- function(x) {
  dictionary <- data.frame(
    symbol = c("À",	"à",	"Â",	"â",	"Ä",	"ä",	"Æ",	"æ",	"Ç",	"ç",	"È",	"è",	"É",	"é",	"Ê",	"ê",	"Ë",	"ë",	"Î",	"î",	"Ï",	"ï",	"ñ",	"Ö",	"ö",	"Ô",	"ô",	"Œ",	"œ",	"Ù",	"ù",	"Û",	"û",	"Ü",	"ü",	"ß"),
    html = c("&Agrave;",	"&agrave;",	"&Acirc;",	"&acirc;",	"&Auml;",	"&auml;",	"&AElig;",	"&aelig;",	"&Ccedil;",	"&ccedil;",	"&Egrave;",	"&egrave;",	"&Eacute;",	"&eacute;",	"&Ecirc;",	"&ecirc;",	"&Euml;",	"&euml;",	"&Icirc;",	"&icirc;",	"&Iuml;",	"&iuml;",	"&ntilde;",	"&Ouml;",	"&ouml;",	"&Ocirc;",	"&ocirc;",	"&OElig;",	"&oelig;",	"&Ugrave;",	"&ugrave;",	"&Ucirc;",	"&ucirc;",	"&Uuml;",	"&uuml;",	"&szlig;")
  )
  for (i in 1:dim(dictionary)[1]) {
    x <- gsub(dictionary$symbol[i], dictionary$html[i], x)
  }
  x
}

#' Remove rows from a data frame that match patterns in a given column
#'
#' Filters out any rows where column `from` matches **any** of the patterns in `toExclude`.
#' Useful for pruning publication lists (e.g., removing theses, preprints, or specific venues).
#'
#' @param toExclude Character vector of patterns to search for.
#' @param from      Name of the column (string) to search within `data`.
#' @param data      A data frame.
#' @param fixed     Logical; if `TRUE`, treat patterns as fixed strings (fast, safe).
#' @param ignore.case Logical; case-insensitive matching if `TRUE`.
#' @return A filtered data frame with matching rows removed.
#'
#' @examples
#' df <- data.frame(title = c("Preprint: X", "Journal: Y", "Thesis Z"))
#' toRemove(c("Preprint", "Thesis"), "title", df)
#' # -> keeps only "Journal: Y"

toRemove <- function(toExclude, from, data) {
  Res <- c()
  for (i in seq_along(toExclude)) {
    if(length(grep(toExclude[[i]], data[[from]])) == 0){
      next
    }else{
      Res <- c(Res, grep(toExclude[[i]], data[[from]]))
    }
  }
  if (length(which(duplicated(Res))) > 0) {
    Res <- Res[-which(duplicated(Res)),]
  }
  if(is.null(Res)){
    cleaned <- data
  }else{
    cleaned <- data[-Res, ]
  }
  return(cleaned)
}


#' Retrieve a named element from (possibly nested) lists
#'
#' Depth-first search through a list; returns the first element whose name matches `toFind`.
#' Works on simple lists, nested lists, and data frames (since dfs are lists of columns).
#'
#' @param myList  A list or data frame that may contain nested lists.
#' @param toFind  Name (string) of the element to retrieve.
#' @param default Value to return if `toFind` is not found (default `NULL`).
#' @return The matching element if found, otherwise `default`.
#'
#' @examples
#' x <- list(a = 1, b = list(c = 2, d = list(e = 3)))
#' listGet(x, "e")   # 3
#' listGet(x, "z", default = NA) # NA

listGet <- function(myList, toFind) {
  # Direct hit on current level
  if (toFind %in% names(myList) == TRUE) {
    return(myList[[toFind]])
  }
  # Recurse into any sub-lists
  else if (any(sapply(myList, class) == "list")) {
    for (i in myList) {
      found <- Recall(i, toFind)
      if (!is.null(found)) {
        return(found)
      } else {
        NULL
      }
    }
  }
}



safe_read_html <- function(url, timeout_sec = 6) {
  if (!nzchar(url) || is.na(url) || !grepl("^https?://", url)) return(NULL)
  ua <- "Mozilla/5.0 (compatible; R/4; +https://www.r-project.org/)"
  resp <- try(httr::GET(url,
                        httr::user_agent(ua),
                        httr::timeout(timeout_sec),
                        httr::add_headers(Accept="text/html,application/xhtml+xml")), silent = TRUE)
  if (inherits(resp, "try-error")) return(NULL)
  sc <- try(httr::status_code(resp), silent = TRUE)
  if (inherits(sc, "try-error") || sc < 200 || sc >= 400) return(NULL)
  txt <- try(httr::content(resp, as="text", encoding="UTF-8"), silent = TRUE)
  if (inherits(txt, "try-error") || !nzchar(txt)) return(NULL)
  doc <- try(xml2::read_html(txt), silent = TRUE)
  if (inherits(doc, "try-error")) return(NULL)
  doc
}
