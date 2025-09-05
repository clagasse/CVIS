################################################################################
#
# 2e_FW_CU_plots.R
#
###############################################################################

## CU boundary diagnostics

# 1. Accessible vs non-accessible reaches by number and length
# 2. FWA stream orders distribution
# 2. Lakes and dams within CU boundaries
# 4. NuSEDS sites 
# 5. Thermalscapes temperatures
  # - violin diagram
  # proportion above each 1 degree threshold
  # map of mean August temperatures
  # temperature gauge locations?
# 6. Reach level PCIC flows
  # MAD histogram for streams
  # mean discharge by month - historical and projected
# 7. Cumulative threat scores - violin diagram
# 8. ENM favourability - violin diagram



i <- 1

## Subset CU boundary data
cuid_i <- cu_run$cuid[i]
cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid_i, ]
sp_pick <- cu_run$spp[cu_run$cuid == cuid_i] #species abbr
path_CU <- path_list[[i]]

FWA_cu <- st_crop(FWA_Fr_high, cu_boundary_i)

#subset nuseds observations
nuseds_CU <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_run$FULL_CU_IN[i],]

amod_CU <- fw_amod[stream_cu_picks[,i],] #%>%

# get species-specific ENM reaches
reaches_ENM_sp <- get(paste0("reaches_ENM_", sp_pick))
pick_st <- lengths(st_intersects(reaches_ENM_sp, cu_boundary_i)) > 0
reaches_ENM_cu <- reaches_ENM_sp[pick_st,]

cu_boundary_table <- all_CU_stats[i,]

cu_boundary_table <- cu_boundary_table %>%
  mutate(length_sum = length_sum/1000) %>%
  mutate(CU_area_km2 = (st_area(cu_boundary_i) %>% units::set_units("km^2"))) %>%
  select(CU_area_km2, n_streams, length_sum, ThiPI_9_45_3, Tw_rate, MADprop_8_diff,
         MADprop_win_diff, peakQday_diff, CT_anad_mean) %>%
  rename("CU Boundary Area (km2)" = CU_area_km2,
        "Number of stream segments" = n_streams,
         "Total stream length (km)" = length_sum,
         "7DEC High T 2041-2060" = ThiPI_9_45_3,
         "Temperature Rate of Change (deg C/10y)" = Tw_rate,
         "Change in August flow as %MAD" = MADprop_8_diff,
         "Change in winter flow as %MAD" = MADprop_win_diff,
         "Change in peak flow day"       = peakQday_diff,
         "Cumulative threat score"       = CT_anad_mean
  )


amod_CU_long <- pivot_longer(amod_CU, 
                             cols = c("FW_SPN_EXP_rateT_9", "FW_SPN_EXP_projT_9", "FW_SPN_EXP_winQ", "FW_SPN_EXP_augQ"),
                             names_to = "indicator", values_to = "value")


#Ridgeline plot of indicator distributions
ggplot(amod_CU_long ) +
  geom_density_ridges_gradient(aes(x = value, y = indicator, fill = ..x..), scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


## CU accessibility and FWA diagnostics

cu_acc_p <- ggplot() +
  geom_sf(data = st_zm(FWA_cu), color = "grey", alpha = 0.6) + 
  geom_sf(data = fw_indies_CU, aes(color = model_access_salmon)) +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  geom_sf(data = nuseds_CU, aes(color = SPECIES), alpha = 0.6) +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("CU ", cu_run$cuname[i]), colour = "BC FishPass Accessibility") +
  theme_minimal()


print(cu_acc_p)



ts_hist_p <- ggplot() +
  geom_sf(data = tscapes_CU, aes(color = Tw8_0_00_0)) +
  scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                        limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("August T, Thermalscapes, 1981-2000")) +
  theme_minimal()

ts_proj_p <- ggplot() +
  #geom_sf(data = st_zm(FWA_cu), color = "grey", alpha = 0.4) + 
  #geom_stars(data = PCIC_Aug_CU_proj["tw_month"]) +
  #scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) + 
  geom_sf(data = tscapes_CU, aes(color = Tw8_9_45_3)) +
  scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                        limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("August T, Thermalscapes, 2041-2060")) +
  theme_minimal()

tPCIC_hist_p <- ggplot() +
  geom_stars(data = PCIC_Aug_CU_hist["tw_month"]) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                       limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  #geom_sf(data = st_zm(FWA_cu), color = "black", alpha = 0.6) + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) + 
  labs(subtitle = "1970-2000 August stream temperature")

tPCIC_proj_p <- ggplot() +
  geom_stars(data = PCIC_Aug_CU_proj["tw_month"]) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                       limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  #geom_sf(data = st_zm(FWA_cu), color = "black", alpha = 0.6) + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) + 
  labs(subtitle = "2041-2070 August stream temperature")

max_hist_flow <- max(PCIC_Aug_CU_hist$flow_month, na.rm = T)
max_proj_flow <- max(PCIC_Aug_CU_proj$flow_month, na.rm = T)
max_plot_flow <- max(max_hist_flow, max_proj_flow)

qPCIC_hist_p <- ggplot() + 
  geom_stars(data = PCIC_Aug_CU_hist["flow_month"]) +
  scale_fill_viridis(limits = c(0, max_plot_flow)) +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  #geom_sf(data = st_zm(FWA_cu), color = "black", alpha = 0.6) + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) + 
  labs(subtitle = "August flow 1970-2000")

qPCIC_proj_p <- ggplot() + 
  geom_stars(data = PCIC_Aug_CU_proj["flow_month"]) +
  scale_fill_viridis(limits = c(0, max_plot_flow)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  #geom_sf(data = st_zm(FWA_cu), color = "black", alpha = 0.6) + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) + 
  labs(subtitle = "August flow 2041-2070")


### ENM favourability within CU boundary
ENM_hist_p <- ggplot() +
  geom_sf(data = reaches_ENM_cu, aes(color = Fav_f.0_00_1)) + 
  geom_sf(data = cu_pick, color = "black", alpha = 0.2) +
  scale_color_viridis()

ENM_proj_p <- ggplot() +
  geom_sf(data = reaches_ENM_cu, aes(color = Fav_f.9_45_3)) + 
  geom_sf(data = cu_pick, color = "black", alpha = 0.2) +
  scale_color_viridis()



### plotting of hydrological regime
hydro_day_plot <- ggplot(data = PCIC_day_summary) +
  geom_point(aes(x = day, y = flow_day, color = year)) +
  labs(subtitle = "PCIC mean flow within CU boundary accessible cells")

tw_day_plot <- ggplot(data = PCIC_day_summary, aes(x = day, y = tw_day, colour = year)) +
  geom_point() + 
  scale_colour_viridis(option = "plasma") +
  labs(subtitle = "PCIC mean temperature within CU boundary accessible cells")

#printcu_acc_p, nrow = 1, ncol = 1)
#grid.arrange(ts_hist_p, ts_proj_p, nrow = 1, ncol = 2)
print(cu_acc_p)
print(ts_hist_p)
print(ts_proj_p)
print(tPCIC_hist_p | tPCIC_proj_p)
print(qPCIC_hist_p | qPCIC_proj_p)
print(hydro_day_plot / tw_day_plot)
#grid.arrange(tPCIC_hist_p, tPCIC_proj_p, nrow =1, ncol =2)
#grid.arrange(qPCIC_hist_p, qPCIC_proj_p, nrow =1, ncol =2)
#grid.arrange(hydro_day_plot, tw_day_plot, nrow = 2, ncol = 1)





cat("####", "CU Migration Path Outputs \n")

### Upstream Migration Plots
timing_s <- cu_timing$rt_start[cu_timing$cuid == cuid[i]]
timing_e <- cu_timing$rt_end[cu_timing$cuid == cuid[i]]

PCIC_migr_hist_CU <- st_crop(PCIC_day, path_CU) %>%
  filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == 1985) %>%
  aggregate(by = "year", FUN = mean)

PCIC_migr_proj_CU <- PCIC_day[path_CU] %>%
  filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == 2055) %>%
  aggregate(by = "year", FUN = mean)

migr_hist_T_plot <- ggplot() +
  geom_stars(data = PCIC_migr_hist_CU["tw_day"]) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                       limits = c(5,25)) + 
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) + 
  labs(subtitle = paste("PCIC temp 1970-2000" ,"days",timing_s, "-", timing_e))

migr_proj_T_plot <- ggplot() +
  geom_stars(data = PCIC_migr_proj_CU["tw_day"]) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                       limits = c(5,25)) + 
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) + 
  labs(subtitle = paste("PCIC temp 2041-2070" ,"days",timing_s, "-", timing_e))

max_hist_flow <- max(PCIC_migr_hist_CU$flow_day, na.rm = T)
max_proj_flow <- max(PCIC_migr_hist_CU$flow_day, na.rm = T)
max_plot_flow <- max(max_hist_flow, max_proj_flow)

migr_hist_Q_plot <- ggplot() +
  geom_stars(data = PCIC_migr_hist_CU["flow_day"]) +
  scale_fill_viridis(limits = c(0, max_plot_flow)) + 
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) + 
  labs(subtitle = paste("PCIC flow 1970-2000:" ,"days",timing_s, "-", timing_e))

migr_proj_Q_plot <- ggplot() +
  geom_stars(data = PCIC_migr_proj_CU["flow_day"]) +
  scale_fill_viridis(limits = c(0, max_plot_flow)) + 
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) + 
  labs(subtitle = paste("PCIC flow 2041-2070:" ,"days",timing_s, "-", timing_e))


grid.arrange(migr_hist_T_plot, migr_proj_T_plot, nrow =1, ncol =2)
grid.arrange(migr_hist_Q_plot, migr_proj_Q_plot, nrow =1, ncol =2)

