% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{Caviar}
\alias{Caviar}
\title{Caviar (dataset)}
\format{
list of igraph objects
}
\source{
Available from Manchester (https://sites.google.com/site/ucinetsoftware/datasets/covert-networks), reconstructed from Morselli's book, Inside Criminal Networks http://www.springer.com/social+sciences/criminology/book/978-0-387-09525-7 Book pages from 173 to 186, Appendix. Converted to R format in the \code{networkdata} package from David Schoch.
}
\usage{
data(Caviar, package = "DF21")
}
\description{
Project Caviar was a unique investigation that targeted a network of hashish and cocaine importers operating out of Montreal. The network was targeted between 1994 and 1996 by a tandem investigation uniting the Montreal Police, the Royal Canadian Mounted Police, and other national and regional law-enforcement agencies from various countries (i.e., England, Spain, Italy, Brazil, Paraguay, and Colombia). The case is unique because it involved a specific investigative approach that will be referred to as a 'seize and wait' strategy. Unlike most law-enforcement strategies, the mandate set forward in the Project Caviar case was to seize identified drug consignments, but not to arrest any of the identified participants. This took place over a 2-year period. Thus, although 11 importation consignments were seized at different moments throughout this period, arrests only took place at the end of the investigation. What this case offers is a rare opportunity to study the evolution of a criminal network phenomenon as it was being disrupted by law-enforcement agents. The inherent investigative strategy permits an assessment of change in the network structure and an inside look into how network participants react and adapt to the growing constraints set upon them.
The principal data source was comprised of information submitted as evidence during the trials of 22 participants in the Caviar network. It included 4,279 paragraphs of information (over 1,000 pages) revealing electronically intercepted telephone conversations between network participants. These transcripts were used to create the overall matrix of the drug-trafficking operation's communication system throughout the course of the investigation. Individuals falling in the surveillance net were not all participants in the trafficking operation. An initial extraction of all names appearing in the surveillance data led to the identification of 318 individuals. From this pool, 208 individuals were not implicated in the trafficking operations. Most were simply named during the many transcripts of conversations, but never detected. Others who were detected had no clear participatory role within the network (e.g., family members or legitimate entrepreneurs). The final network was thus composed of 110 participants.
NETWORK  11 1-mode matrices person by person, representing the 11 phases of the investigation. Ties are directed and valued. Number of nodes = 1) 15x15, 2) 24x24, 3) 33x33, 4) 33x33, 5) 32x32, 6) 27x27, 7) 34x34, 8) 42x42, 9) 34x34, 10) 42x42, 11) 41x41
1-mode matrix 110 x 110 person by person of the complete network.
Ties are communication exchanges between criminals. Values represent level of communication activity. Data comes from police wiretapping.
}
\references{
Morselli, C., 2009. Inside criminal networks. New York: Springer.
}
