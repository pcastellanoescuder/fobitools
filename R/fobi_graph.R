
#' Generate FOBI Graphs
#'
#' @description to do
#'
#' @param terms to do
#' @param get to do
#' @param layout to do
#' @param labels to do
#' @param labelsize to do
#' @param legend to do
#' @param legendSize to do
#' @param legendPos to do
#' @param curved to do
#' @param pointSize to do
#'
#' @export
#'
#' @return A ggraph object.
#' @references Pol Castellano-Escuder, Raúl González-Domínguez, David S Wishart, Cristina Andrés-Lacueva, Alex Sánchez-Pla, FOBI: an ontology to represent food intake data and associate it with metabolomic data, Database, Volume 2020, 2020, baaa033, https://doi.org/10.1093/databa/baaa033.
#' @author Pol Castellano-Escuder
#'
#' @examples
#' 
#' terms <- c("CHEBI:16164", "CHEBI:16243", "FOODON:00001139", "FOODON:00003274", "FOODON:00003275")
#' 
#' fobi_graph(terms, get = "anc")
#' fobi_graph(terms, get = "anc", labels = TRUE)
#' fobi_graph(terms, get = "anc", labels = TRUE, legend = TRUE)
#' fobi_graph(terms = "FOODON:00002473", labels = TRUE)
#' 
#' # Plot whole FOBI
#' fobi_graph(terms = c("FOBI:01501", "FOBI:0001"), get = "des", layout = "lgl", pointSize = 2)
#' 
#' @import ggraph
#' @import ggplot2
#' @import tidygraph
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select rename
fobi_graph <- function(terms = NULL,
                       get = NULL,
                       layout = "sugiyama",
                       labels = FALSE,
                       labelsize = 3,
                       legend = FALSE,
                       legendSize = 10,
                       legendPos = "bottom",
                       curved = FALSE,
                       pointSize = 3){
  
  if (is.null(terms)) {
    stop("No terms are provided")
  }
  if (!is.null(get)){
    if (!(get %in% c("anc", "des"))) {
      stop("Incorrect value for get argument. Options are 'anc' (for ancestors) and 'des' (for descendants)")
    }
  }
  if (!(layout %in% c("sugiyama", "lgl"))) {
    stop("Incorrect value for layout argument. Options are 'sugiyama' (default) and 'lgl'")
  }
  if (!(legendPos %in% c("bottom", "top"))) {
    stop("Incorrect value for legendPos argument. Options are 'bottom' (default) and 'top'")
  }
  
  fobi_foods <- parse_fobi(terms = "FOBI:0001", get = "des")
  
  fobiGraph <- parse_fobi(terms = terms, get = get) %>%
    filter(!is.na(is_a_code))
  
  contains <- fobiGraph %>%
    mutate(Property = ifelse(!is.na(Contains), "Contains", NA)) %>%
    filter(!is.na(Property)) %>%
    select(name, Contains, Property) %>%
    rename(from = 1, to = 2, Property = 3)
  
  biomarkerof <- fobiGraph %>%
    mutate(Property = ifelse(!is.na(BiomarkerOf), "BiomarkerOf", NA)) %>%
    filter(!is.na(Property)) %>%
    select(name, BiomarkerOf, Property) %>%
    rename(from = 1, to = 2, Property = 3)
  
  is_a <- fobiGraph %>%
    select(name, is_a_name) %>%
    mutate(Property = "is_a") %>%
    filter(!duplicated(name)) %>%
    rename(from = 1, to = 2, Property = 3)
  
  ##
  
  graph_table <- rbind(is_a, biomarkerof, contains) %>%
    as_tbl_graph() %>%
    mutate(subOntology = ifelse(name %in% fobi_foods$name, "Food", "Biomarker"))
  
  cols_nodes <- c("Biomarker" = "#440154FF", 
                  "Food" = "#FDE725FF") # viridis palette
  
  cols_edges <- c("is_a" = "#287C8EFF", 
                  "BiomarkerOf" = "#75D054FF", 
                  "Contains" = "#E3E418FF") # viridis palette
  
  ggraph(graph_table, layout = layout) +
    {if(!curved) geom_edge_link(aes(color = Property), end_cap = circle(2.5, "mm"), 
                                arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
                                show.legend = legend)} +
    {if(curved) geom_edge_arc(aes(color = Property), end_cap = circle(2.5, "mm"),
                              arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
                              strength = 0.1, show.legend = legend)} +
    geom_node_point(aes(color = as.factor(subOntology), shape = as.factor(subOntology)), size = pointSize, show.legend = legend) +
    {if(labels)geom_node_text(aes(label = name), color = "black", size = labelsize, repel = TRUE, show.legend = FALSE)} +
    theme_graph(foreground = "white", fg_text_colour = "white") + 
    theme(legend.title = element_blank(),
          legend.text = element_text(size = legendSize),
          legend.position = legendPos) +
    scale_color_manual(values = cols_nodes) +
    scale_shape_manual(values = c("Biomarker" = 16, "Food" = 15)) +
    scale_edge_color_manual(values = cols_edges)
  
  }

