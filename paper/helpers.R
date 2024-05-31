
"%||%" <- function (x, y) {
  if (is.null(x))
    y
  else x
}



is_word <- function() {
  format <- knitr::opts_knit$get("rmarkdown.pandoc.to") %||% "r"
  format == "docx"
}


format <- knitr::opts_knit$get("rmarkdown.pandoc.to") %||% "r"
chapter <- tools::file_path_sans_ext(knitr::current_input())
verbose <- interactive()
knitr::opts_chunk$set(
  echo = FALSE,
  dev = "png",
  dpi = 150,
  message = verbose,
  warning = verbose,
  cache = TRUE,
  fig.show = ifelse(is_word(), "hide", "asis"),
  cache.extra = 3453,
  out.extra = "",
  fig.width = 6,
  fig.height = 6/1.6,   # Golden ratio?
  fig.align = "center",
  fig.path = file.path("figures", chapter, ""),
  cache.path = file.path("cache", chapter, format, "")
)


knitr::opts_hooks$set(label = function(options) {
  # Tira un error si el chunk no tiene label
  default_label <- knitr::opts_knit$get("unnamed.chunk.label")
  if (grepl(default_label, options$label)) {
    stop("Name your chunks!")
  }
  
  # Setea el default del caption a label-cap
  if (is.null(options[["fig.cap"]])) {
    options[["fig.cap"]] <- paste0("(ref:", options[["label"]], "-cap)")
  }
  
  options
})


get_caption <- function() {
  knitr::opts_current$get("fig.cap")
}

# ggplot2::theme_set(eliotesis::theme_tesis(base_size = 10) +
#                      ggplot2::theme(legend.box.spacing = grid::unit(-.5, "lines")))


LonLabel <- purrr::partial(metR::LonLabel, west = "°O")
lab_lev <- eliotesis::AddSuffix(" hPa")
lab_cplx <- c("Real" = "0º",
              "Imaginario" = "90º")

lab_sam <-  c(full = "SAM",
              asym = "A-SAM",
              sym  = "S-SAM")

factor_sam <- function(term) {
  factor(term, levels = names(lab_sam), ordered = TRUE)
}


lab_season <- c(DJF = "DEF",
                MAM = "MAM",
                JJA = "JJA",
                SON = "SON")


axis_labs_smol <- function() ggplot2::theme(axis.text = ggplot2::element_text(size = 6))


r2 <- expression(`$r^2$` = paste("", "r", phantom()^{
  paste("2")
}, "", ""))

main_period <- c("1979-01-01", "2020-12-01")
periodo <- paste0(data.table::year(main_period), collapse = "--")

texto_pval <- function(pval = 0.01, ajustado = TRUE) {
  ajuste <- if (ajustado) " ajustado por FDR"
  paste0("Áreas con puntos tienen p-valor menor que ", pval, ajuste, ".")
  
}


todo <- function(text) {
  if (knitr::is_html_output()) {
    text <- paste0("<span style='background-color:#FF8000;'>", text, "</p>")
  } else if (knitr::is_latex_output()) {
    text <- paste0(" \\todo[inline]{", text, "}")
  } else {
    text <- paste0("**TODO: ", text, "**")
  }
  
  return(text)
}


pval_contours <- function(pval = 0.01, de = NULL, adjust = TRUE) {
  if (adjust) {
    adjust <- " ajustado por FDR"
  } else {
    adjust <- NULL
  }
  
  
  paste0("Áreas con puntos marcan regiones donde el p-valor", de, " es menor que ", pval, adjust)
}

ZeroBreaks <- metR::AnchorBreaks(0, NULL, 0)


color_real <- "#998EC3"
color_imaginario <- "#E66101"
scale_color_fase <- function(name = NULL, ...) {
  scale_color_manual(name = name, ..., values = c(Real = color_real, Imaginario = color_imaginario),
                     labels = lab_cplx)
}

scale_fill_fase <- function(name = NULL, ...) {
  scale_fill_manual(name = name, ..., values = c(Real = color_real, Imaginario = color_imaginario),
                    labels = lab_cplx)
}
