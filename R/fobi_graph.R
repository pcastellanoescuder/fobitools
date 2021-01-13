
#' Generate FOBI Graphs
#'
#' @description This function allows users to create networks based on FOBI relationships.
#'
#' @param terms A character vector with FOBI term IDs.
#' @param get A character string indicating desired relationships between provided terms. Options are 'anc' (for ancestors) and 'des' (for descendants). Default is NULL and only information of single input terms will be provided.
#' @param property A character vector indicating which properties should be plotted. Options are 'is_a', 'BiomarkerOf', and 'Contains'. By default all of them are included.
#' @param layout A character string indicating the type of layout to create. Options are 'sugiyama' (default) and 'lgl'.
#' @param labels Logical indicating if node names should be plotted or not.
#' @param labelsize Numeric value indicating the size of labels.
#' @param legend Logical indicating if legend should be plotted or not.
#' @param legendSize Numeric value indicating the size of legend.
#' @param legendPos A character string indicating the legend position (if legend parameter is set to TRUE). Options are 'bottom' (default) and 'top'.
#' @param curved Logical indicating if the shape of the edges shape should be curved or not.
#' @param pointSize Numeric value indicating the size of graph points.
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
#' fobi_graph(terms, get = "anc", legend = TRUE)
#' fobi_graph(terms = "FOODON:00002473", property = c("is_a", "BiomarkerOf"), curved = TRUE)
#' 
#' @import ggraph
#' @import ggplot2
#' @import tidygraph
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select rename
fobi_graph <- function(terms = NULL,
                       get = NULL,
                       property = c("is_a", "BiomarkerOf", "Contains"),
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
  if (!all(property %in% c("is_a", "BiomarkerOf", "Contains"))) {
    stop("Incorrect value for property argument. Options are 'is_a', 'BiomarkerOf', and 'Contains'")
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
  
  if(sum(nrow(is_a), nrow(biomarkerof), nrow(contains)) < 1) {
    stop("No terms with these characteristics")
  }
  
  ##
  
  graph_table <- rbind(is_a, biomarkerof, contains) %>%
    filter(Property %in% property) %>%
    as_tbl_graph() %>%
    mutate(subOntology = ifelse(name %in% fobi_foods$name, "Food", "Biomarker"),
           subOntology = ifelse(name == "Foods", "Food", subOntology))
  
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
    geom_node_point(aes(color = subOntology, shape = subOntology), size = pointSize, show.legend = FALSE) +
    {if(labels)geom_node_text(aes(label = name), color = "black", size = labelsize, repel = TRUE, show.legend = FALSE)} +
    scale_color_manual(values = cols_nodes, guide = "none") +
    scale_shape_manual(values = c("Biomarker" = 16, "Food" = 15), guide = "none") +
    scale_edge_color_manual(values = cols_edges) +
    theme_graph(foreground = "white", fg_text_colour = "white") + 
    theme(legend.title = element_blank(),
          legend.text = element_text(size = legendSize),
          legend.position = legendPos)

  }

