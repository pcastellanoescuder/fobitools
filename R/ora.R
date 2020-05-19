
#' Over Representation Analysis and FOBI
#'
#' @description This function performs a traditional Over Representation Analysis by hypergeometric test: classes are treated as sets of individual metabolites and all metabolites are treated as equally informative. `fobitools::ora` uses FOBI (Food-Biomarker Ontology) to extract the biological information and it's an adapted version of `sigora::ora` function.
#'
#' @param metabolites Character vector with metabolite names. Other IDs are accepted: FOBI, HMDB, KEGG, PubChemCID, InChIKey, InChICode and ChemSpider.
#' @param fobi_sets Sets desired to test for the over representation in FOBI. Options are: "foods" (default) and "chemicals".
#' @param method Method for multiple testing adjustment. Options are: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
#' @param stable_version Logical. If it's set to TRUE (default), the function will use an stable version of FOBI. If not, the function will use a developing version of FOBI from GitHub.
#'
#' @export
#'
#' @return A data frame with results.
#' @references Foroushani AB, Brinkman FS and Lynn DJ (2013). Pathway-GPS and SIGORA:identifying relevant pathways based on the over-representation of their gene-pair signatures. PeerJ, 1, pp. e229.
#' @author Pol Castellano-Escuder
#'
#' @importFrom magrittr %>%
#' @importFrom clisymbols symbol
#' @importFrom crayon red green blue
#' @importFrom tidyr drop_na separate_rows
#' @importFrom dplyr mutate select rename filter left_join
#' @importFrom stringr str_remove_all regex str_replace_all
#' @importFrom sigora makeGPS
ora <- function(metabolites,
                fobi_sets = "foods",
                method = "bonferroni",
                stable_version = TRUE){

  if (is.null(metabolites)) {
    stop(crayon::red(clisymbols::symbol$cross, "Input must be a vector with metabolite names!"))
  }
  if (!is.vector(metabolites)) {
    stop(crayon::red(clisymbols::symbol$cross, "Input must be a vector with metabolite names!"))
  }
  if (!(method %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"))) {
    stop(crayon::red(clisymbols::symbol$cross, "Incorrect value for method argument!"))
  }
  if (missing(method)) {
    method <- "bonferroni"
    warning("method argument is empty! Bonferroni method will be used!")
  }
  if (!(fobi_sets %in% c("foods", "chemicals"))) {
    stop(crayon::red(clisymbols::symbol$cross, "Incorrect value for fobi_sets argument!"))
  }
  if (missing(fobi_sets)) {
    fobi_sets <- "foods"
    warning("fobi_sets argument is empty! Food groups will be used as sets!")
  }

  ####

  if(!isTRUE(stable_version)){

    fobi <- parse_fobi()
    
    GPSrepo_foods <- fobi %>%
      select(FOBI, name, BiomarkerOf) %>%
      mutate(BiomarkerOf = as.character(BiomarkerOf)) %>%
      filter(!BiomarkerOf == "NULL") %>%
      separate_rows(BiomarkerOf, sep = '", "') %>%
      mutate(BiomarkerOf = str_remove(BiomarkerOf, pattern = regex('[c("]')),
             BiomarkerOf = str_remove(BiomarkerOf, pattern = regex('[")]')),
             BiomarkerOf = str_trim(BiomarkerOf),
             BiomarkerOf = str_squish(BiomarkerOf))

    ##
    
    GPSrepo_chemicals <- fobi %>%
      filter(!BiomarkerOf == "NULL") %>%
      select(is_a_code, is_a_name, name) %>%
      mutate_all(as.character)

    ##
    
    idmap <- fobi %>%
      filter(!BiomarkerOf == "NULL") %>%
      select(name, FOBI, HMDB, KEGG, PubChemCID, InChIKey, InChICode, ChemSpider) %>%
      mutate_all(as.character) %>%
      mutate(HMDB = str_replace_all(HMDB, regex("( ).*"), regex("\\1")),
             KEGG = str_replace_all(KEGG, regex("( ).*"), regex("\\1")),
             PubChemCID = str_replace_all(PubChemCID, regex("( ).*"), regex("\\1")),
             ChemSpider = str_replace_all(ChemSpider, regex("( ).*"), regex("\\1"))) %>%
      rename(metaboliteNames = name) %>%
      mutate_all(~ ifelse(. == "NULL", NA, .))
    
    ##

    GPSrepo_foods <- sigora::makeGPS(GPSrepo_foods)
    GPSrepo_chemicals <- sigora::makeGPS(GPSrepo_chemicals)

  }

  else {

    data("fobi")
    data("idmap")
    data("GPSrepo_foods")
    data("GPSrepo_chemicals")

  }

  if(fobi_sets == "foods"){
    GPSrepo <- GPSrepo_foods
  } else{
    GPSrepo <- GPSrepo_chemicals
  }

  #### modification of sigora::ora

  fr <- GPSrepo$origRepo[[3]]
  sp1 <- GPSrepo$pathwaydescriptions

  if (length(intersect(metabolites, GPSrepo$origRepo[[2]])) == 0) {
    t1 <- which.max(sapply(idmap, function(x) length(intersect(x, GPSrepo$origRepo[[2]]))))
    t2 <- which.max(sapply(idmap, function(x) length(intersect(x, metabolites))))
    new_idmap <- idmap[, c(t2, t1)]
    colnames(new_idmap) <- c("match", "metaboliteName")
    metabolites <- new_idmap[which(new_idmap$match %in% metabolites), 2]
    message(crayon::blue(paste0(clisymbols::symbol$bullet, " Mapped identifiers from ", colnames(idmap)[t2], " to ", colnames(idmap)[t1])))
  }

  g1m <- match(metabolites$metaboliteName, GPSrepo$origRepo[[2]])
  frO <- fr[fr[, 2] %in% g1m, ]
  npwys <- table(as.character(GPSrepo$origRepo[[1]][(frO[,1])]))
  nn <- cbind(sp1[match(names(npwys), sp1$pwys), ], as.numeric(as.vector(npwys)))
  PPwys <- table(as.character(GPSrepo$origRepo[[1]][(fr[, 1])]))
  ps <- phyper(npwys - 1, PPwys[match(names(npwys), names(PPwys))],
               length(GPSrepo$origRepo[[2]]) - PPwys[match(names(npwys), names(PPwys))],
               length(intersect(metabolites$metaboliteName, GPSrepo$origRepo[[2]])), lower.tail = F)
  ps <- round(ps, 4)

  result <- data.frame(nn, PPwys[match(names(npwys), names(PPwys))], as.numeric(ps),
                       as.numeric(p.adjust(ps, method = method)))[, -4]
  colnames(result) <- c("classId", "description", "success", "classSize", "pvalue", "pvalueAdj")
  result <- result[with(result, order(pvalue)), ]
  rownames(result) <- c(1:nrow(result))

  return(result)

}

