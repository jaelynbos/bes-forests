Guide to BGS 2020 code

All the data necessary to run the code is available within the 
folder, though obviously the filepaths will need to be changed. 

Each script lists, at the top, the other scripts on which it depends,
as well as the required packages. Running all the scripts requires the 
following packages:

dplyr
ggplot2
labdsv
rpart
rpart.plot
stringr
tidyr
TITAN2
vegan
viridis

For most analyses, scripts should be run in the following order:

1. "USFS codes.R". This script is a function to replace latin names 
and incorrect USFS codes with correct USFS codes.

2. "Create GC community data.R" and "Create OS community data.R" create
matrixes of community data for ground cover and overstory species. Sites
are rows and taxa are columns. 

3. "Taxa and environmental data.R" matches the community matrices created
above with site attributes.

Most other scripts can then be run. Below is a brief description of each
one's function:

Clusters.R creates clusters and dendrograms for ground cover and overstory
sites, as well as identifying indicator taxa.
Pie charts.R creates pie charts of species for each cluster. 
Rpart.R creates CARTs for cluster assignment.

Richness and shannon ground cover R. runs an nCPA and creates boxplots 
by MSPA class for species richness and shannon diversity with ground cover 
taxa. 

Richness Shannon stem density overstory.R runs an nCPA and creates boxplots 
by MSPA class for species richness, shannon diversity, and stem density with
 overstory taxa.

Schumaker score boxplot.R makes boxplot of Schumaker scores by MSPA class.

Penetrometer boxplots and nCPA.R makes boxplots of penetrometer readings
by MSPA class as well as nCPA along a distance to edge gradient.

Organic matter.R makes boxplots of organic matter depth by MSPA class.

Species accumulation curves.R plots rarefaction curves by taxa group.

Ground cover TITAN.R and Overstory TITAN.R run TITAN for ground cover and 
overstory taxa, respectively.

Ground Cover NMDS.R and Overstory NMDS.R runs all of the NMDS analyses
and makes all the plots shown in the google doc. 

betadispr.R calculated dispersion metrices for various clusters. 









