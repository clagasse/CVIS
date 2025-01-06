###############################################################################
#
# 2d_fw_FAZ_analysis.R
#
#  Analysis of climate exposure for freshwater spawning, incubation and rearing 
#   for each FAZ:

#   1) subset accessible streams within the boundary
#   2) summarize tscapes august mean temperature
#


################################################################################
### FAZ Summaries and Statistics
################################################################################

tscapes_FAZ_summary <- as.tibble(tscapes_acc) %>%
  group_by(FAZ) %>%
  summarize(Tw8_0_00_0 = mean(Tw8_0_00_0, na.rm=T),
            Tw8_9_45_3 = mean(Tw8_9_45_3, na.rm=T),
            mean_diff_temp  = mean(diff_temp, na.rm=T),
            total_length = sum(length_metres, na.rm=T),
            diff_weighted = sum(diff_temp * length_metres, na.rm =T) / total_length)

for(i in 1:nrow(FAZ_Fr)) {
  FAZ_i <- FAZ_Fr[i,]
  PCIC_month_FAZ <- PCIC_month[FAZ_i]
  
  PCIC_data <- PCIC_month_FAZ %>% 
    split("time") 
  
  %>%
    as.data.frame() 
  
  
  
  %>%
    mutate(spp = cu_run.s$Species[i], cu = cu_run.s$cuname[i], cuid = cu_run.s$cuid[i]) %>%
    relocate(spp, cu, cuid)
  
}

###############################################################################
## Plotting

ggplot(tscapes_LFR) +
  geom_point(aes(x=Tw8_0_00_0, y = Tw8_9_45_3, colour = STREAM_ORDER)) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2)

ggplot(tscapes) +
  geom_point(aes(x=STREAM_ORDER, y = diff_temp, colour = FAZ)) 

ggplot(tscapes_FWA) +
  geom_boxplot(aes(x=STREAM_ORDER_FACTOR, y = length_metre)) 

ggplot(tscapes_FWA) +
  geom_point(aes(x=diff_temp, y = diff_weighted)) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2)

plot(tscapes[15:16])

PCIC.FAZ <- PCIC[FAZ_pick]


PCIC.FAZ.aug <- filter(PCIC.FAZ, month(time) == 8)

#calculate monthly mean for each PCIC grid cell
PCIC.FAZ.aug.mean.hist <- PCIC.FAZ.aug %>%
  filter(year(time) == 1985) %>%
  st_apply(1:2, mean)
  
PCIC.FAZ.aug.mean.mid <- PCIC.FAZ.aug %>%
  filter(year(time) == 2055) %>%
  st_apply(1:2, mean)  

mean(PCIC.FAZ.aug.mean.mid$tw_day, na.rm = T)
mean(PCIC.FAZ.aug.mean.hist$tw_day, na.rm = T)

mean(tscapes_FAZ$Tw8_0_00_0, na.rm = T)
mean(tscapes_FAZ$Tw8_9_45_3, na.rm = T)



