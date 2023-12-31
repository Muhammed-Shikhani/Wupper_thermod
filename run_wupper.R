
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('/home/robert/Projects/Wupper_thermod')

source('helper.R')

library(gotmtools)
library(LakeEnsemblR)
library(pracma)
library(tidyverse)
library(deSolve)
library(LakeMetabolizer)

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

# calibrate the entrainment coefficient over the thermocline
parameters[19] = 2.0

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

# add water quality parameters
wq_parameters <- append(parameters, c(0.001 / 1000, 
                                      2, 1500 * 1e4, 100)) # Fnep, Fsed, Ased, diffred

# initial water temperatures and do concentrations
yini <- c(3,3, 10 * 1000/1e6  * wq_parameters[1], 10 * 1000/1e6  * wq_parameters[2]) 

if (file.exists('output.txt')){
  file.remove('output.txt')
}

ice_on = TRUE # ice "simulation" on or off?

# calibrate parameters
wq_parameters[19] = 2.5

boundary_cal = boundary %>%
  mutate(Jsw = 1.3 * Jsw,
         Tair = 1.3 * Tair)

# run the oxygen model
out <- run_oxygen_model(bc = boundary_cal, params = wq_parameters, ini = yini, times = times, ice = ice_on)

# visualize the results
result <- data.frame('Time' = out[,1],
                     'WT_epi' = out[,2], 'WT_hyp' = out[,3],
                     'DO_epi' = out[,4], 'DO_hyp' = out[,5])

ggplot(result) +
  geom_line(aes(x=Time, y=WT_epi , col='Surface Mixed Layer')) +
  geom_line(aes(x=(Time), y=WT_hyp , col='Bottom Layer')) +
  labs(x = 'Simulated Time', y = 'WT in deg C')  +
  theme_bw()+
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="bottom")

ggplot(result) +
  geom_line(aes(x=Time, y=DO_epi / 1000 /  wq_parameters[1] * 1e6, col='Surface Mixed Layer')) +
  geom_line(aes(x=(Time), y=DO_hyp / 1000 /  wq_parameters[2] * 1e6, col='Bottom Layer')) +
  labs(x = 'Simulated Time', y = 'DO in g/m3')  +
  theme_bw()+
  guides(col=guide_legend(title="Layer")) +
  theme(legend.position="bottom")
