### 

## Indicator Plots



#### Compare PCIC and Thermalscapes

ggplot(all_CU_stats) +
  geom_histogram(aes(x=augT_PCIC_hist_mean - w_Tw8_0_00_0))
  labs(title = "Gridded August mean temp 1970-2000 vs Thermalscapes 1981-2000")

ggplot(all_CU_stats) +
  geom_point(aes(x=augT_PCIC_proj_mean, y=w_Tw8_9_45_3, colour = Species_simple), size = 2) +
  geom_text(aes(x=augT_PCIC_proj_mean, y=w_Tw8_9_45_3,
                label = cuname, colour = Species_simple), size = 2.5, vjust = -1.) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  scale_x_continuous(limits=c(12,22)) +
  scale_y_continuous(limits=c(12,22)) +
  labs(title = "Gridded August mean temp 2041-2070 vs Thermalscapes 2041-2060")

ggplot(all_CU_stats) +
  geom_histogram(aes(x=augT_PCIC_diff - w_Tw_diff)) +
  labs(title = "Difference between August mean temp from historic to mid-century")

ggplot(all_CU_stats) +
  geom_point(aes(x=augT_PCIC_z, y=Tw_z_score, colour = Species_simple), size = 2) +
  geom_text(aes(x=augT_PCIC_z, y=Tw_z_score,
                label = cuname, colour = Species_simple), size = 2.5, vjust = -1.) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  labs(title = "Difference between August mean temp from historic to mid-century")


#### PCIC Migration Indicators

ggplot(all_CU_stats) +
  geom_histogram(aes(x=Tmigr_hist_mean)) 
