# commute-analyzer-sg-public-bustransport
Data Source: Land Transport Authority (LTA) Geospatial visual analysis of Singapore bus commuter flows across planning areas and subzones using Intramax modelling.

**server.R** - complete application code developed on RShiny.

**ui.R** - application interface code developed on RShiny.

**Intramax_v2.R** - utilized open source Intramax code by Martin Charlton available on his GitHub.
The output of the Intramax analysis is a list of allocation of areas to clusters and the values of the maximized objective function 
at the last iteration of the clustering process. Modified the code to extract the following information at each stage of the 
clustering process:
(i) List of allocation of areas to clusters.
(ii) List containing the percentage intrazonal interaction.
(iii) List containing identity of 2 areas that merged.

**Project_Poster.jpg** - provides details on commute analyzer application and the visualizations designed.
