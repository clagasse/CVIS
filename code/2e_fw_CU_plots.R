################################################################################
#
# 2e_fw_CU_plots.R
#

###############################################################################
## Plotting for CU outputs

# Subset CU boundary
cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid[i], ]
FWA_cu <- st_crop(FWA_Fr_high, cu_boundary_i)

path_CU <- path_list[[i]]


tscapes_CU <- tscapes_acc[cu_tscapes[,i],] %>%
  filter(!is.na(Tw8_0_00_0))

PCIC_Aug_CU_hist <- st_crop(PCIC_month, tscapes_CU) %>%
  filter(month(time) == 8, year(time) == 1985) 
PCIC_Aug_CU_proj <- st_crop(PCIC_month, tscapes_CU) %>%
  filter(month(time) == 8, year(time) == 2055) 

PCIC_CU_diff <- PCIC_Aug_CU_proj %>%
  st_set_dimensions(3, values = as.POSIXct("1985-08-15", "UTC"), names = "time")

PCIC_CU_diff <- PCIC_CU_diff - PCIC_Aug_CU_hist
PCIC_CU_prop <- PCIC_CU_diff / PCIC_Aug_CU_hist

PCIC_day_CU <-  st_crop(PCIC_day, cu_boundary_i) %>% 
  as_tibble()

PCIC_day_summary <- PCIC_day_CU %>%
  group_by(time) %>%
  dplyr::summarize(tw_day = mean(tw_day, na.rm=T),
                   flow_day = mean(flow_day, na.rm = T)) %>%
  mutate(year = year(time), 
         month = month(time),
         day = yday(time))

cu_acc_p <- ggplot() +
  geom_sf(data = st_zm(FWA_cu), color = "grey", alpha = 0.4) + 
  geom_sf(data = tscapes_CU, aes(color = model_access_salmon)) +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = chin_sp, color = "darkgreen", alpha = 0.8, show.legend = "point") + 
  geom_sf(data = coho_sp, color = "darkblue", alpha = 0.8, show.legend = "point") +
  geom_sf(data = sockeye_sp, color = "darkred", alpha = 0.8, show.legend = "point") + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("CU ", cu_run$cuname[i]))

ts_hist_p <- ggplot() +
  geom_sf(data = tscapes_CU, aes(color = Tw8_0_00_0)) +
  scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                        limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("August stream T, 1981-2000"))

ts_proj_p <- ggplot() +
  #geom_sf(data = st_zm(FWA_cu), color = "grey", alpha = 0.4) + 
  #geom_stars(data = PCIC_Aug_CU_proj["tw_month"]) +
  #scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) + 
  geom_sf(data = tscapes_CU, aes(color = Tw8_9_45_3)) +
  scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                        limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("August stream T, 2041-2060 -", cu_boundary$cuname[i]))

tPCIC_hist_p <- ggplot() +
  geom_stars(data = PCIC_Aug_CU_hist["tw_month"]) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                       limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  #geom_sf(data = st_zm(FWA_cu), color = "black", alpha = 0.6) + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) + 
  labs(subtitle = "1970-2000 August stream temperature")

tPCIC_proj_p <- ggplot() +
  geom_stars(data = PCIC_Aug_CU_proj["tw_month"]) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                       limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
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
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  #geom_sf(data = st_zm(FWA_cu), color = "black", alpha = 0.6) + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) + 
  labs(subtitle = "August flow 1970-2000")

qPCIC_proj_p <- ggplot() + 
  geom_stars(data = PCIC_Aug_CU_proj["flow_month"]) +
  scale_fill_viridis(limits = c(0, max_plot_flow)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  #geom_sf(data = st_zm(FWA_cu), color = "black", alpha = 0.6) + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) + 
  labs(subtitle = "August flow 2041-2070")


### plotting of hydrological regime
hydro_day_plot <- ggplot(data = PCIC_day_summary) +
  geom_point(aes(x = day, y = flow_day, color = year)) +
  labs(subtitle = "PCIC mean flow within CU boundary accessible cells")

tw_day_plot <- ggplot(data = PCIC_day_summary, aes(x = day, y = tw_day, colour = year)) +
  geom_point() + 
  scale_colour_viridis(option = "plasma") +
  labs(subtitle = "PCIC mean temperature within CU boundary accessible cells")

grid.arrange(cu_acc_p, nrow = 1, ncol = 1)
grid.arrange(ts_hist_p, ts_proj_p, nrow = 1, ncol = 2)
grid.arrange(tPCIC_hist_p, tPCIC_proj_p, nrow =1, ncol =2)
grid.arrange(qPCIC_hist_p, qPCIC_proj_p, nrow =1, ncol =2)
grid.arrange(hydro_day_plot, tw_day_plot, nrow = 2, ncol = 1)


### Upstream Migration Plots
timing_s <- cu_timing$rt_start[cu_timing$cuid == cuid_i]
timing_e <- cu_timing$rt_end[cu_timing$cuid == cuid_i]

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

migr_hist_Q_plot <- ggplot() +
  geom_stars(data = PCIC_migr_hist_CU["flow_day"]) +
  scale_fill_viridis(limits = c(0, 2000)) +
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) + 
  labs(subtitle = paste("PCIC flow 1970-2000" ,"days",timing_s, "-", timing_e))

migr_proj_Q_plot <- ggplot() +
  geom_stars(data = PCIC_migr_proj_CU["flow_day"]) +
  scale_fill_viridis(limits = c(0, 2000)) +
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) + 
  labs(subtitle = paste("PCIC flow 2041-2070" ,"days",timing_s, "-", timing_e))


grid.arrange(migr_hist_T_plot, migr_proj_T_plot, nrow =1, ncol =2)
grid.arrange(migr_hist_Q_plot, migr_proj_Q_plot, nrow =1, ncol =2)

