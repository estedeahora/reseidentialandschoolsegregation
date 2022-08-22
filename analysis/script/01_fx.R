
# wcomment: Agregar comentarios de word ----------------------------------------

wcomment <- function(cm, txt = "", author = "PS",
                     date = Sys.Date(), comN = commentN){
  cmt_str <- paste0('<span class="comment-text">[', cm,
                    ']{.comment-start id="', comN,
                    '" author="', author,
                    '" date="', date,
                    '"}', txt,
                    '[]{.comment-end id="',commentN,
                    '"}</span>')
  assign("commentN", commentN + 1, envir = .GlobalEnv)
  return(cmt_str)
}

# compose_eq: generar salida de ecuaciones en flextable ------------------------

compose_eq <- function(ft, indic = "Index"){
  ft |>
    flextable::compose(j = indic,
                       value = as_paragraph(
                         str_split(Index, pattern = "\\$" ,
                                   simplify = T)[, 1],
                         as_equation(
                           str_split(Index, pattern = "\\$",
                                     simplify = T)[, 2],
                           width = 2, height = .5),
                         str_split(Index, pattern = "\\$" ,
                                   simplify = T)[, 3]))
}

# MMutual: Calcula Index M -----------------------------------------------------

MMutual <- function(b, total = T, index = c("M", "H", "all"),
                    se = T, seed = 321, bootstrap = 100){
  if(!any(index %in% c("M", "H", "all"))){
    stop("'index' must be one of 'M', 'H' or 'all'")
  }else{
    index <- index[1]
  }
  set.seed(seed)
  res <- b |>
    mutate(id = 1:n()) |>
    pivot_longer(cols = -id, values_to = "n") |>
    mutual_total(group = "name", unit = "id", weight = "n",
                 se = se, n_bootstrap = bootstrap)

  if(index == "all"){
    return(res)
  }else{
    res <- round(res$est[res$stat == index], 4)
    names(res) <- index

    return(res)
  }

}

# SEG_summary: Calcula diferentes indices de segregación -----------------------

SEG_summary <- function(b, filtro0 = T, bootstrap, verbose = F, se = T){
  if(filtro0) {
    a <- apply(b, 1, sum)
    b <- b[a > 0 & !is.na(a), ]
  }

  IND <- c("Dissimilarity Primary and Higher education ($D_{pri;hig}$)",
           "Multi-group dissimilarity ($D^{*}$)",
           "Mutual Information Index ($M$)",
           "Segregation: Primary ($IS_{pri}$)",
           "Segregation: Secondary ($IS_{sec}$)",
           "Segregation: Higher education ($IS_{sup}$)",
           "Multi-group Gini Multi-group ($G^{*}$)",
           "Normalized Isolation: Primary ($ETA_{pri}^2$)",
           "Normalized Isolation: Secondary ($ETA_{sec}^2$)",
           "Normalized Isolation: Higher education ($ETA_{sup}^2$)",
           "Multi-group normalized exposure ($P^{*}$)")


  res <- data.frame(Index = IND,
                    Value = c(DIDuncan(b)[1, 3],
                              DMulti(b),
                              MMutual(b, index = "M", se = se,
                                      bootstrap = bootstrap),
                              ISDuncan(b),
                              GiniMulti(b),
                              Eta2(b),
                              PMulti(b)))

  n <- names(b)
  if(verbose){
    cat("Group names: ", paste(n, collapse = ", "), "\n")
  }
  attr(res, which = "group") <- n
  return(res)
}
