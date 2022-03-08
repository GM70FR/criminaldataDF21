#' 17 November Greece Bombing
#' @description The dataset refers to the 17 November Revolutionary Organisation, 
#' a Marxist urban guerrilla organization operating in Greece. 
#' The data refer to the specific temporal window which runs from 1975 to 2002. 
#' During these years the group has been responsible for several violent acts 
#' such as assassinations, kidnappings and symbolic attacks on government offices.
#' The following has been reconstructed:
#' \preformatted{
#' 1) 2-mode matrix, binary, 15x12 persons by events. Ties are participation in terrorist events
#' 2) 1-mode stacked matrices 18x18 persons by persons, binary
#'    Kinship
#'    1975-1984
#'    1985-1994
#'    1995-2002
#' The original file presents a distinction among several types of relationships:
#'    1. Acquaintances/Distant family ties (interactions limited to radical organisation activities),
#'    2. Friends/Moderately close family ties (interactions extend beyond radical organisations to include such categories as co-workers and roommates). Operational/Organisational leadership (i.e. JI leadership, formally or informally ranking members of burgeoning cells).Operational Ties (i.e. worked closely on a bombing together).
#'    3. Close Friends/Family, Tight-knit operational cliques (would die for each other)
#' If one of these three types of relationships was present, it has been coded with 1.
#' }
#' @source Reconstructed at Manchester (https://sites.google.com/site/ucinetsoftware/datasets/covert-networks). Freely available from http://doitapps.jjay.cuny.edu/jjatt/data.php
#' @format list of igraph objects
#' @usage data(Greece_bombing, package = "DF21")
#' @name Greece_bombing
#' @docType data
NULL


#' 9/11 Hijackers
#' @description Famous dataset of the terrorists involved in the 9/11 bombing 
#' of the World Trade Centres in 2011. Data was extracted from news reports and 
#' ties range from ‘at school with’ to ‘on same plane’.
#' 1-mode matrix 19 x 19 person by person of trusted prior contacts and 1-mode 
#' matrix 61 x 61 of other associates.
#' Ties are undirected and binary. Relations are a mix of prior-contacts 
#' like trained together, lived together, financial transactions, at school with, 
#' on same flight.
#' @source Reconstructed at Manchester (https://sites.google.com/site/ucinetsoftware/datasets/covert-networks).
#'
#' http://orgnet.com/tnet.html
#'
#' http://firstmonday.org/ojs/index.php/fm/article/view/941/863#fig4
#' @references Krebs, Valdis E. "Mapping networks of terrorist cells." *Connections* 24.3 (2002): 43-52.
#' @format igraph object
#' @usage data(Hijackers_911, package = "DF21")
#' @name Hijackers_911
#' @docType data
NULL
