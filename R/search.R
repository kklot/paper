#' Search an author by name
#' 
#' @param query author name as character string, space allow
#' @inheritParams an_author args
#' @param limit number of result to return if there are more than this limit
#' @value
#' - essential: tibble of essential information about authors
#' - paper: tibble of all authors and their papers
#' @export
search_author <- function(
    query = "adam smith ", 
    args = "name,affiliations,aliases,url,papers.title,papers.year,papers.url",
    limit = 10) 
{
    query <- gsub("\\s", "+", trimws(query))
    base_url <- get("base_url", envir = paper_env)
    url <- paste0(base_url, "author/search?query=", query, "&fields=", args, "&limit=", limit)
    a_list <- httr::GET(url, httr::user_agent("k-paper"))
    httr::stop_for_status(a_list, paste("Searching for author", query))
    ct <- httr::content(a_list)
    message("Found ", ct$total, " potential matches, listing here the ", limit, " authors")

    paper <- ct$data %>% purrr::map(~ purrr::map_dfr(.x$papers, paper::list_as_tibble))
    paper <- paper %>%
        rlang::set_names(ess$authorId) %>%
        dplyr::bind_rows(.id = "authorId")
    
    most_recent_paper <- paper %>%
        group_by(authorId) %>%
        dplyr::arrange(dplyr::desc(year), .by_group = TRUE) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::select(authorId, most_recent_paper = title)
    
    essential <- ct$data %>%
        purrr::map_dfr(function(x) {
            x$affiliations <- paste0(unlist(list()), ",")
            tibble::as_tibble(x[c("name", "authorId", "affiliations", "url")])
        }) %>%
        dplyr::left_join(most_recent_paper, by = "authorId")
    
    message("use view_web(this_object, authorID) to open the author url in your browser.")
    message("use get_paper(this_object, authorID) to get a table of publication")
    message("top ten rows")
    print(essential)

    o <- list(essential = essential, paper = paper)
    class(o) <- c("search_author", class(o))
    o
}

#' Open author website (if exists) in default browser
#'
#' @param search_author \code{\link[paper]{search_author}} output
#' @param id authorId identify from \code{\link[paper]{search_author}} output.
#' @export
view_web <- function(search_author, id) {
    if (!inherits(search_author, "search_author")) stop("Not a search_author()'s output")
    dplyr::filter(search_author$essential, authorId == id) %>%
        dplyr::pull("url") %>%
        browseURL()
}

#' Get papers from search_author results
#' 
#' @param search_author \code{\link[paper]{search_author}} output
#' @param id authorId identify from \code{\link[paper]{search_author}} output.
#' @export
get_paper <- function(search_author, id) {
    if (!inherits(search_author, 'search_author')) stop("Not a search_author()'s output")
    dplyr::filter(search_author$paper, id == id)
}