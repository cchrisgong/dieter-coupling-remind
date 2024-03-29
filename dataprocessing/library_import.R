library.path <- .libPaths()
print(library.path)

library(stringr)
# library(xlsx)
library(purrr)
library(reshape2)
library(quitte)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(readxl)
library(ggthemes)
library(gridExtra)
library(cowplot)
library(lubridate)
library(RColorBrewer)
library(zoo)

library(tibble)
library(dygraphs)
library(grid)
library(plyr)
library(ggpubr)
library(gridExtra)
library(cowplot)
library(lubridate)
library(RColorBrewer)
library("car")
library(openxlsx)

library(gdxrrw)

packages <- c("gdxrrw", "reshape2", "quitte", "ggplot2", "dplyr", "tidyr", "grid", "readxl",
              "ggthemes", "gridExtra", "cowplot", "lubridate", "RColorBrewer", "openxlsx", "zoo" )

#updating packages
for (i in packages) {
  
  if(!require(i, character.only=TRUE)){
    install.packages(i, character.only=TRUE)
    library(i,character.only=TRUE)
  }
}
