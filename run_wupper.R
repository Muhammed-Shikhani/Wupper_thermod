
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('/home/robert/Projects/Wupper_thermod')

source('helper.R')

library(gotmtools)
library(LakeEnsemblR)
library(pracma)
library(tidyverse)
library(deSolve)

### GETTING CONFIGURATION INPUT FROM LER YAML FILE
config_file <- 'LakeEnsemblR.yaml'
folder = '.'
parameters <- configure_from_ler(config_file <- config_file, folder = folder)

# load in the boundary data
bound <- read_delim(paste0(folder,'/meteo.txt'), delim = '\t')
bound <- bound[, -c(1)]
colnames(bound) <- c('Day','Jsw','Tair','Dew','vW')

# function to calculate wind shear stress (and transforming wind speed from km/h to m/s)
bound$Uw <- 19.0 + 0.95 * (bound$vW * 1000/3600)^2
bound$vW <- bound$vW * 1000/3600

boundary <- bound

# simulation maximum length
times <- seq(from = 1, to = max(as.numeric(boundary$Day) - min(as.numeric(boundary$Day))), by = 1)

# initial water temperatures
yini <- c(3,3)

parameters[19] = 2

# run the model
out <- run_model(bc = boundary, params = parameters, ini = yini, times = times, ice = F)

# visualize the results
result <- data.frame('Time' = out[,1],
                     'WT_epi' = out[,2], 'WT_hyp' = out[,3])
ggplot(result) +
  geom_line(aes(x=Time, y=WT_epi, col='Surface Mixed Layer')) +
  geom_line(aes(x=(Time), y=WT_hyp, col='Bottom Layer')) +
  labs(x = 'Simulated Time', y = 'WT in deg C')  +
  theme_bw()+
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="bottom")
