library.path <- .libPaths()
print(library.path)

# # import library
# summary(cars)
# library(tidyr)
# library(dplyr)
# library(ggplot2)
# library(tibble)
# library(data.table)
# library(reshape2)
# library(gdx)
# # library(xlsx)
# library(zoo)
# library(stringr)
# library(quitte)
# library(dygraphs)
# library(plyr)
# library(ggpubr)
# theme_set(theme_pubr())
# library("car")

library(stringr)
library(purrr)
library(gdxrrw)
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
library(openxlsx)
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

packages <- c("gdxrrw", "reshape2", "quitte", "ggplot2", "dplyr", "tidyr", "grid", "readxl",
              "ggthemes", "gridExtra", "cowplot", "lubridate", "RColorBrewer", "openxlsx", "zoo" )

#updating packages
for (i in packages) {
  
  if(!require(i, character.only=TRUE)){
    install.packages(i, character.only=TRUE)
    library(i,character.only=TRUE)
  }
}
