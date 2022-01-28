paper_env <- new.env(parent = emptyenv())
assign('base_url', "https://api.semanticscholar.org/graph/v1/", envir = paper_env)

tidymess <- function(..., prefix = " ", initial = "") {
    message(strwrap(..., prefix = prefix, initial = initial))
}
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
    args = "year,fieldsOfStudy,citationCount,url") 
{
    base_url <- get('base_url', envir = paper_env)
    url <- paste0(base_url, "paper/", id, "/citations/?fields=", args)
    cited_this <- httr::GET(url, httr::user_agent("k-paper"))
    httr::stop_for_status(cited_this, paste('Getting paper ', id))
    o <- httr::content(cited_this, as = "parse")$data
    o <- purrr::map_dfr(o, function(x) {
        if (is.null(x[[1]]$paperId)) x[[1]]$paperId <- NA_character_
        if (is.null(x[[1]]$url)) x[[1]]$url <- NA_character_
        if (is.null(x[[1]]$fieldsOfStudy)) x[[1]]$fieldsOfStudy <- list(NA_character_)
        if (is.null(x[[1]]$citationCount)) x[[1]]$citationCount <- NA_real_
        if (is.null(x[[1]]$year)) x[[1]]$year <- NA_real_
        tibble::as_tibble(x[[1]])
    })
    class(o) <- c("a_paper", class(o))
    o
}

get_cited_details <- function(chunk) {
    purrr::map(chunk$paperId, paper::a_paper)
    purrr::map(chunk$paperId, paper::a_paper) %>%
        rlang::set_names(chunk$paperId) %>%
        dplyr::bind_rows(.id = "mypaperId")
}

#' Get details of an_author
#'
#' @param id authorId as return by \code{\link[paper]{search_author}}
#' @param args fields to get 
#' A comma-separated list of the fields to be returned.  
#' 
#' The following case-sensitive author fields are recognized:
#'
#' - `authorId` - Always included
#' - `externalIds`
#' - `url`
#' - `name` - Included if no fields are specified
#' - `aliases`
#' - `affiliations`
#' - `homepage`
#' - `paperCount`
#' - `citationCount`
#' - `hIndex`
#' - `papers.paperId` - Always included
#' - `papers.externalIds`
#' - `papers.url`
#' - `papers.title` - Included if no fields are specified
#' - `papers.abstract`
#' - `papers.venue`
#' - `papers.year`
#' - `papers.referenceCount`
#' - `papers.citationCount`
#' - `papers.influentialCitationCount`
#' - `papers.isOpenAccess`
#' - `papers.fieldsOfStudy`
#' - `papers.authors`\- Will include: `authorId` & `name`
#' 
#' @param level get the author paper's details only or also details of their
#' cited papers?
#' @export
an_author <- function(
    id = "1741101", 
    args = "paperCount,citationCount,hIndex,papers.year,papers.url,papers.citationCount,papers.fieldsOfStudy", 
    level = c('me_only', 'me_and_cited_me')) 
{
    base_url <- get('base_url', envir = paper_env)
    url <- paste0(base_url, "author/", id, "?fields=", args)
    my_pp <- httr::GET(url, httr::user_agent("k-paper"))
    httr::stop_for_status(my_pp, paste('Getting author ', id))
    ct <- httr::content(my_pp)
    o <- purrr::map_dfr(ct[["papers"]], function(x) {
        if (is.null(x$fieldsOfStudy)) x$fieldsOfStudy <- list(NA_character_)
        if (is.null(x$citationCount)) x$citationCount <- NA_real_
        if (is.null(x$year)) x$year <- NA_real_
        tibble::as_tibble(x)
    })

    attr(o, 'hIndex') <- ct$hIndex
    attr(o, 'paperCount') <- ct$paperCount
    attr(o, 'citationCount') <- ct$citationCount

    level <- match.arg(level)
    if (level == "me_only") {
        class(o) <- c("an_author", class(o))
        return(o)
    }

    oo <- dplyr::filter(o, citationCount != 0) %>%
        dplyr::distinct(paperId) # same paper different field classification
    n_request <- nrow(oo) + 1L

    if (n_request > 500) {
        msg <- paste(
            "Rate limit is 500 queries/5min, you will need to wait ~",
            n_request / 500 * 5,
            "minutes to query all of them. Do you want me to automatically wait and get all the data for you?"
        )
        to_wait <- askYesNo(msg, default = TRUE)
        if (to_wait) {
            tidymess("Cool, please leave this R session open and have a drink.
            Why not read about tmux in the mean time for a better terminal sessions management?
            https://tmuxcheatsheet.com")
            n_chunks <- n_request / 500
            chunk_id <- as.factor(sort(1L:nrow(oo) %% n_chunks))
            o <- lapply(split(oo, chunk_id), get_cited_details)
        } else {
            tidymess("Fail to retrieve details for each of the author's paper.
            You can try to retrieve individually with a_paper() function with the paperID from this output")
        }
    } else {
        o <-  get_cited_details(oo)
    }
    o
}

#' @export
summary.an_author <- function(an_author) {
    if (!inherits(an_author, "an_author")) stop("Not an_author object")
    message('hIndex       :', attr(an_author, 'hIndex'))
    message('paperCount   :', attr(an_author, 'paperCount'))
    message("citationCount:", attr(an_author, "citationCount"))
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


#' Plot method for an_author class
#' 
#' Produce a similar plot to PlumX metric, but the numbers of papers cited the
#' reference paper by fields.
#' 
#' - Size is the number of papers times the median citation counts of the
#'   respective paper
#' - Older median year of the papers give are more transparent.
#' 
#' @param an_author an \code{\link[paper]{an_author}} object
#' @param give_data give me data instead of the plot
#' @param txt_seed seed to label text use in `ggrepel`
#' @export
plot.an_author <- function(an_author, give_data = FALSE, txt_seed = 1) 
{
    if (!inherits(an_author, "an_author")) stop("Not an_author object")

    plot_data <- an_author %>%
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
    

    max_height <- max(plot_data$height)
    mid_x <- nlevels(plot_data$fieldsOfStudy) / 2
    
    plot_gg <- plot_data %>%
    dplyr::mutate(fieldsOfStudy = reorder(fieldsOfStudy, dplyr::desc(size))) %>%
    ggplot2::ggplot() +
    geom_col(aes(fieldsOfStudy, height, alpha = recency), fill = NA) +
    geom_text(aes(0, max_height, label = "H-index"), angle = 90, size = 5, color = "gray70", alpha = .5) +
    geom_text(aes(0, 0, label = attr(an_author, 'hIndex')), size = 50, fontface = 'bold', color = "gray92", alpha = .5) +
    geom_col(aes(fieldsOfStudy, height, alpha = recency), width = .2, fill = "#D55E00") +
    geom_point(aes(fieldsOfStudy, height, size = height), shape = 21, color = "#D55E00", fill = "#E69F00") + 
    ggrepel::geom_text_repel(aes(fieldsOfStudy, height, label = fieldsOfStudy), size = 3, color = 'grey50', seed = txt_seed) +
    coord_polar(start = 3 * pi /2, direction = 1) +
    theme_void() +
    scale_alpha(range = c(0.3, 1)) +
    theme(legend.position = "none") +
    scale_y_continuous(trans = 'log1p')
    if (give_data)
        return(plot_data)
    plot_gg
}