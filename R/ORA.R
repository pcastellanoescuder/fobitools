#' ORA
#'
#' @description This function performs an ORA (Over Representation Analysis) using FOBI (Food-Biomarker Ontology) given a list of metabolites. FOBIEnrich allows users to explore over represented food groups by set of metabolites.
#'
#' @param metabolites Vector with metabolite names.
#' @param method Method for multiple test adjustment.
#'
#' @return A data frame with results.
#'
#' @importFrom magrittr %>%
#' @importFrom ontologyIndex get_ontology
#' @importFrom clisymbols symbol
#' @importFrom crayon red
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom purrr map
#' @importFrom tibble add_column
#' @importFrom tidyr crossing
#' @import dplyr
#'
#' @export
#' @author Pol Castellano-Escuder
ORA <- function(metabolites, method = c("bonferroni", "fdr")){

  if (is.null(metabolites)) {
    stop(crayon::red(clisymbols::symbol$cross, "Input must be a vector with metabolite names!"))
  }
  if (!is.vector(metabolites)) {
    stop(crayon::red(clisymbols::symbol$cross, "Input must be a vector with metabolite names!"))
  }
  if (!(method %in% c("bonferroni", "fdr"))) {
    stop(crayon::red(clisymbols::symbol$cross, "Incorrect value for method argument!"))
  }
  if (missing(method)) {
    method <- "bonferroni"
    warning("method argument is empty! Bonferroni method will be used!")
  }

  ####

  path <- "ontology/FOBI_old.obo"

  ontology <- get_ontology(path, extract_tags = "everything")

  ####

  BiomarkerOf <- ontology$`FOBI:00422` %>%
    map(as_tibble) %>%
    bind_rows(.id = "metabolite_ID") %>%
    add_column(type = "BiomarkerOf", metabolite = NA, food = NA)

  names <- ontology$name %>%
    map(as_tibble) %>%
    bind_rows(.id = "ID") %>%
    rename(value = value)

  for (i in 1:nrow(BiomarkerOf)){

    BiomarkerOf$metabolite[i] <- names$value[names$ID == BiomarkerOf$metabolite_ID[i]]
    BiomarkerOf$food[i] <- names$value[names$ID == BiomarkerOf$value[i]]

  }

  TABLE <- aggregate(BiomarkerOf$metabolite, list(BiomarkerOf$food), function(x) paste0(unique(x)))

  sig_metabolites <- metabolites[metabolites %in% unlist(TABLE$x)]
  out <- metabolites[!(metabolites %in% unlist(TABLE$x))]

  ####

  combinations <- crossing(out, possibilities = unlist(TABLE$x[!(unlist(TABLE$x) %in% sig_metabolites)]))

  str_distances <- combinations %>%
    rowwise() %>%
    mutate(Lev_dist = adist(x = out, y = possibilities, ignore.case=TRUE))

  str_distances_sel <- str_distances[str_distances$Lev_dist <= 1 ,]
  str_distances_sel <- str_distances_sel[!duplicated(str_distances_sel$out) ,]

  sig_metabolites <-c(sig_metabolites, str_distances_sel$possibilities)

  ## CHANGED

  changed_old <- str_distances_sel$out
  changed <- str_distances_sel$possibilities

  ## OUT

  out <- out[!(out %in% str_distances_sel$out)]

  ####

  results <- list()

  for (i in 1:nrow(TABLE)){

    # Significative metabolites in food group
    idx <- sig_metabolites %in% unlist(TABLE$x[i])
    d <- length(sig_metabolites[idx])

    # Significative metabolites out of food group
    idx2 <- !(sig_metabolites %in% unlist(TABLE$x[i]))
    b <- length(sig_metabolites[idx2])

    # Non significative metabolites out of food group
    idx3 <- !(unlist(TABLE$x) %in% sig_metabolites) & !(unlist(TABLE$x) %in% unlist(TABLE$x[i]))
    a <- length(unlist(TABLE$x)[idx3])

    # Non significative metabolites in food group
    idx4 <- !(unlist(TABLE$x[i]) %in% sig_metabolites)
    c <- length(unlist(TABLE$x[i])[idx4])

    ####

    x <- matrix(cbind(a, c, b, d), nrow = 2, ncol = 2)
    results[[i]] <- fisher.test(x, alternative = "greater")$p.value

  }

  pval <- unlist(results)

  results <- data.frame(Food_Group = TABLE$Group.1,
                        Pval = as.numeric(pval),
                        adjPval = as.numeric(p.adjust(pval, method = method)))
  results <- results[order(results$Pval) ,]

  if(length(out) > 0){

    message(crayon::red(clisymbols::symbol$cross, paste0(length(out), " metabolites were removed from the analysis.",
                                                         " [", paste0(out, collapse = ", "), "]")))

  } else{

    message(crayon::green(clisymbols::symbol$tick, "All metabolites analyzed!"))
  }

  if(length(changed) > 0){

    message(crayon::blue(clisymbols::symbol$warning, paste0(length(changed),
                                                            " metabolites not found in FOBI and they have been replaced for similar name metabolites.",
                                                            "\n", paste0(" [", changed_old, " to ", changed, "]", collapse = "\n"))))

  }

  return(results)

}

