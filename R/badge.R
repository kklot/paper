paper_env <- new.env(parent = emptyenv())
assign('base_url', "https://api.semanticscholar.org/graph/v1/", envir = paper_env)

#' Update API url if needed
#'
#' In case API changes
#' @param new_url new API url, eg "https://api.semanticscholar.org/graph/v1/"
update_api <- function(new_url) 
{
    assign('base_url', new_url, envir = paper_env)
}

#' Get details of a single paper
#' 
#' @param id semanticscholar The following types of IDs are supported:
#' 
#'   * `<sha>` - a Semantic Scholar ID, e.g. `649def34f8be52c8b66281af98ae884c09aef38b`
#'   * `CorpusId:<id>` - Semantic Scholar numerical ID, e.g. `215416146`
#'   * `DOI:<doi>` - a [Digital Object Identifier](http://doi.org/), e.g. `DOI:10.18653/v1/N18-3011`
#'   * `ARXIV:<id>` - [arXiv.rg](https://arxiv.org/), e.g. `ARXIV:2106.15928`
#'   * `MAG:<id>` - Microsoft Academic Graph, e.g. `MAG:112218234`
#'   * `ACL:<id>` - Association for Computational Linguistics, e.g. `ACL:W12-3903`
#'   * `PMID:<id>` - PubMed/Medline, e.g. `PMID:19872477`
#'   * `PMCID:<id>` - PubMed Central, e.g. `PMCID:2323736`
#'   * `URL:<url>` - URL from one of the sites listed below, e.g. `URL:https://arxiv.org/abs/2106.15928v1`
#' 
#' URLs are recognized from the following sites:
#' 
#'   * [semanticscholar.org](https://www.semanticscholar.org/)
#'   * [arxiv.org](https://arxiv.org/)
#'   * [aclweb.org](https://www.aclweb.org/)
#'   * [acm.org](https://www.acm.org/)
#'   * [biorxiv.org](https://www.biorxiv.org/)
#' @param get what to get - hardcode now, a comma-separated list of the fields
#' to be returned. The following case-sensitive citation fields are recognized:
#' 
#' - `contexts`
#' - `intents`
#' - `isInfluential`
#' - `paperId` - Always included
#' - `externalIds`
#' - `url`
#' - `title` - Included if no fields are specified
#' - `abstract`
#' - `venue`
#' - `year`
#' - `referenceCount`
#' - `citationCount`
#' - `influentialCitationCount`
#' - `isOpenAccess`
#' - `fieldsOfStudy`
#' - `authors` - Up to 500 will be returned. Will include: `authorId` & `name`
#' @export
a_paper <- function(
    id = "649def34f8be52c8b66281af98ae884c09aef38b", 
    args = "citations/?fields=year,fieldsOfStudy,citationCount") 
{
    base_url <- get('base_url', envir = paper_env)
    url <- paste0(base_url, "paper/", id, "/", args)
    cited_this <- httr::GET(url, httr::user_agent("k-paper"))
    httr::stop_for_status(cited_this)
    o <- httr::content(cited_this, as = "parse")$data
    o <- purrr::map_dfr(o, function(x) {
        if (is.null(x[[1]]$fieldsOfStudy)) x[[1]]$fieldsOfStudy <- list(NA_character_)
        if (is.null(x[[1]]$citationCount)) x[[1]]$citationCount <- NA_real_
        tibble::as_tibble(x[[1]])
    })
    class(o) <- c("a_paper", class(o))
    o
}

#' Plot method for a_paper class
#' 
#' Produce a similar plot to PlumX metric, but the numbers of papers cited the
#' reference paper by fields.
#' 
#' - Size is the number of papers times the median citation counts of the
#'   respective paper
#' - Older median year of the papers give are more transparent.
#' 
#' @param a_paper an \code{\link[paper]{a_paper}} object
#' @param give_data give me data instead of the plot
#' @param txt_seed seed to label text use in `ggrepel`
#' @export
plot.a_paper <- function(a_paper, give_data = FALSE, txt_seed = 1) 
{
    if (!inherits(a_paper, "a_paper")) stop("Not a_paper object")

    plot_data <- a_paper %>%
        dplyr::mutate(
            fieldsOfStudy = unlist(fieldsOfStudy),
            fieldsOfStudy = dplyr::if_else(is.na(fieldsOfStudy), "Unknown", fieldsOfStudy)
        ) %>%
        dplyr::group_by(fieldsOfStudy) %>%
        dplyr::summarise(
            size = dplyr::n(),
            weight = median(citationCount, TRUE), 
            recency = median(year)
        ) %>%
        dplyr::mutate(fieldsOfStudy = as.factor(fieldsOfStudy), height = size)

    plot_gg <- ggplot2::ggplot(
        plot_data,
        aes(fieldsOfStudy, log1p(height), alpha = recency)
    ) +
    geom_col(width = .1, fill = "#D55E00") +
    geom_point(aes(size = height),
        shape = 21,
        color = "#D55E00",
        fill = "#E69F00"
    ) +
    ggrepel::geom_text_repel(aes(label = fieldsOfStudy), seed = txt_seed) +
    coord_polar() +
    theme_void() +
    scale_alpha(range = c(0.2, 1)) +
    theme(
        legend.position = "none",
        plot.background = element_rect(fill='white'),
        panel.spacing = grid::unit(c(0,0,0,0), "mm"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    )
    if (give_data)
        return(plot_data)
    plot_gg
}