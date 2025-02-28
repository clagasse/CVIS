
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

fw_acc_indies <- fw_acc_indies %>%
  mutate(rateT_spn = (Tw8_9_45_3 - Tw8_0_00_0) / 6)

do_FAZ <- F

i <- 11

nuseds_Ck <- nuseds_Fr %>%
  filter(SPECIES == "Chinook")

cu_boundary_show <- cu_boundary %>%
  filter(CUID %in% cuid) %>%
  left_join(select(CVIS_CU, -cuname), by = c("CUID" = "cuid")) %>%
  left_join(select(cu_run, cuid, Peak_Spawn_To_Ocean_Entry_Days), by = c("CUID" = "cuid"))

cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid[i], ]
FWA_cu <- st_crop(FWA_Fr_high, cu_boundary_i)
# Subset CU migration path
path_CU <- path_list[[i]]

#subset nuseds observations
nuseds_CU <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_run$FULL_CU_IN[i],]

fw_indies_CU <- fw_acc_indies[stream_cu_picks[,i],] #%>%



ggplot() +
  geom_sf(data = st_zm(FWA_cu), color = "grey", alpha = 0.5) + 
  geom_sf(data = fw_indies_CU, aes(color = model_access_salmon)) +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  geom_sf(data = nuseds_CU, color = "darkgreen", alpha = 0.8) +
  # {switch(p_sp_pick, geom_sf(data = chin_sp, color = "darkgreen", alpha = 0.6),
  #         geom_sf(data = coho_sp, color = "darkblue", alpha = 0.6),
  #         geom_sf(data = sockeye_sp, color = "darkred", alpha = 0.6))} +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("CU ", cu_run$cuname[i])) +
  theme_minimal()


ggplot() +
  #geom_sf(data = st_zm(FWA_cu), color = "grey", alpha = 0.4) + 
  #geom_stars(data = PCIC_Aug_CU_proj["tw_month"]) +
  #scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) + 
  geom_sf(data = fw_indies_CU, aes(color = Tw8_9_45_3), linewidth = 1.) +
  scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                        limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("August T, Thermalscapes, 2041-2060")) +
  theme_minimal()


fw_indies_CU <- fw_indies_CU %>%
  mutate(augQ_spn = MADprop_8_hist - MADprop_8_proj)

ggplot() +
  geom_sf(data = fw_indies_CU, aes(color = augQ_spn), linewidth = 1.) +
  scale_color_viridis(limits = c(-10,10)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("Change in August flows as %MAD")) +
  theme_minimal()


ggplot() +
  geom_stars(data = PCIC_Aug_CU_proj["tw_month"]) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                       limits = c(5,25)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  #geom_sf(data = st_zm(FWA_cu), color = "black", alpha = 0.6) + 
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) + 
  labs(subtitle = "2041-2070 August stream temperature")


ggplot() +
  # geom_stars(data = PCIC_migr_proj_CU["tw_day"]) +
  # scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
  #                      limits = c(5,25)) + 
  geom_sf(data = nuseds_CU, color = "darkgreen", alpha = 0.8) +
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) #+
  #geom_sf(data = bc_coast, color = "black", alpha = 0.3) 
  #labs(subtitle = paste("PCIC temp 2041-2070" ,"days",timing_s, "-", timing_e))

ggplot() +
  geom_stars(data = PCIC_migr_proj_CU["tw_day"]) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                       limits = c(5,25)) + 
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) + 
  labs(subtitle = paste("PCIC temp 2041-2070" ,"days",timing_s, "-", timing_e))



ggplot() +
  geom_stars(data = PCIC_migr_hist_CU["flow_day"]) +
  scale_fill_viridis(limits = c(0, max_plot_flow)) + 
  theme_minimal() +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.2) +
  geom_sf(data = st_zm(path_CU)) + 
  labs(subtitle = paste("PCIC flow 1970-2000:" ,"days",timing_s, "-", timing_e))





ggplot() +
  geom_sf(data = fw_indies_CU, aes(color = rateT_spn), linewidth = 1.) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"), limits = c(0.3,0.6)) + 
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
  coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
           ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("Change in August flows as %MAD")) +
  theme_minimal()


CVIS_CU <- CVIS_CU %>%
  mutate(Q_mig = (Q_mig - Qmigr_hist_mean) / Qmigr_hist_mean)

ggplot(CVIS_CU) +
  geom_point(aes(x=T_mig, y = Q_mig, colour = Species_simple)) +
  geom_text(aes(x=T_mig, y= Q_mig,
                label = cuname, colour = Species_simple), size = 2.5, vjust = -1.) +
  labs(title = "Average projected migration temperature (T_mig) vs %change in flow (Q_mig)",
       colour = "Species") 
  

ggplot() +
  geom_sf(data = cu_boundary_show, aes(fill = rateT_spn), alpha = 0.3) +
  #geom_sf_text(data = cu_boundary_show, aes(label = cuname)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) + 
  #geom_sf(data = nuseds_Ck, colour = "darkgreen", alpha = 0.6) +
  geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +
  
  # {switch(p_sp_pick, geom_sf(data = chin_sp, color = "darkgreen", alpha = 0.6),
  #         geom_sf(data = coho_sp, color = "darkblue", alpha = 0.6),
  #         geom_sf(data = sockeye_sp, color = "darkred", alpha = 0.6))} +
  # coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
  #          ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("Decadal rate of August mean T change")) +
  theme_minimal()

ggplot() +
  geom_sf(data = cu_boundary_show, aes(fill = projT_spn), alpha = 0.3) +
  #geom_sf_text(data = cu_boundary_show, aes(label = cuname)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) + 
  #geom_sf(data = nuseds_Ck, colour = "darkgreen", alpha = 0.6) +
  geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +
  
  # {switch(p_sp_pick, geom_sf(data = chin_sp, color = "darkgreen", alpha = 0.6),
  #         geom_sf(data = coho_sp, color = "darkblue", alpha = 0.6),
  #         geom_sf(data = sockeye_sp, color = "darkred", alpha = 0.6))} +
  # coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
  #          ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("7DEC temperature 85%PI 2041-2060")) +
  theme_minimal()
#geom_sf(aes(x=projT_spn, colour = Species_simple))
#labs(title = "August mean T 1970-2000 (PCIC) vs 1981-2000 (Thermalscapes)")


ggplot() +
  geom_sf(data = cu_boundary_show, aes(fill = winQ_spn), alpha = 0.3) +
  #geom_sf_text(data = cu_boundary_show, aes(label = cuname)) +
  scale_fill_viridis() + 
  #geom_sf(data = nuseds_Ck, colour = "darkgreen", alpha = 0.6) +
  geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +

  # {switch(p_sp_pick, geom_sf(data = chin_sp, color = "darkgreen", alpha = 0.6),
  #         geom_sf(data = coho_sp, color = "darkblue", alpha = 0.6),
  #         geom_sf(data = sockeye_sp, color = "darkred", alpha = 0.6))} +
  # coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
  #          ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("Difference in winter flows as %MAD historic")) +
  theme_minimal()

ggplot() +
  geom_sf(data = cu_boundary_show, aes(fill = augQ_spn), alpha = 0.3) +
  #geom_sf_text(data = cu_boundary_show, aes(label = cuname)) +
  scale_fill_viridis(option = "rocket") + 
  #geom_sf(data = nuseds_Ck, colour = "darkgreen", alpha = 0.6) +
  geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +
  
  # {switch(p_sp_pick, geom_sf(data = chin_sp, color = "darkgreen", alpha = 0.6),
  #         geom_sf(data = coho_sp, color = "darkblue", alpha = 0.6),
  #         geom_sf(data = sockeye_sp, color = "darkred", alpha = 0.6))} +
  # coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
  #          ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
  labs(subtitle = paste("Difference in August flows as %MAD historic")) +
  theme_minimal()

ggplot() +
  geom_sf(data = cu_boundary_show, aes(fill = peakQday_spn), alpha = 0.3) +
  scale_fill_viridis(option = "magma") + 
  geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +
  labs(subtitle = paste("Change in peak flow day")) +
  theme_minimal()


ggplot() +
  geom_sf(data = cu_boundary_show, aes(fill = anadCT_spn), alpha = 0.3) +
  scale_fill_viridis(option = "magma") + 
  geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +
  labs(subtitle = paste("Average stream cumulative stressor score")) +
  theme_minimal()

ggplot() +
  geom_sf(data = cu_boundary_show, aes(fill = Peak_Spawn_To_Ocean_Entry_Days), alpha = 0.3) +
  scale_fill_viridis() + 
  geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +
  labs(subtitle = paste("Number of Days from Peak Spawn to Ocean Entry"),
       fill = "# days") +
  theme_minimal()

ggplot(all_CU_stats) +
  geom_point(aes(x=Tw8_9_45_3, y=Tav_9_45_3, colour = Species_simple), size = 2) +
  geom_text(aes(x=Tw8_9_45_3, y=Tav_9_45_3,
                label = cuname, colour = Species_simple), size = 2.5, vjust = -1.) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  xlim(12,19) +
  ylim(12,19) +
  labs(title = "Mean August T vs 7DECM")

ggplot(all_CU_stats) +
  geom_point(aes(y=ThiPI_9_45_3, x=Tw8_9_45_3, colour = Species_simple), size = 2) +
  geom_text(aes(y=ThiPI_9_45_3, x=Tw8_9_45_3,
                label = cuname, colour = Species_simple), size = 2.5, vjust = -1.) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  xlim(12,24) +
  ylim(12,24) +
  labs(title = "Mean August T vs 7DECM 85% PI")

ggplot(all_CU_stats) +
  geom_point(aes(y=Risk24_prop, x=Risk20_prop, colour = Species_simple), size = 2) +
  geom_text(aes(y=Risk24_prop, x=Risk20_prop,
                label = cuname, colour = Species_simple), size = 2.5, vjust = -1.) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  labs(title = "7DECM 50% PI vs 85% PI")




era_mod <- read_csv(file.path(salmon_dat, "Era_coefs_Jan152025.csv"))

ggplot(filter(era_mod, varnam = "SST", era = "Late")) +
  geom_point(aes(x=mean, y = coef2, colour = Species_simple), size = 2) +
  geom_text(aes(x=coef, y= coef2,
                label = cuname, colour = Species_simple), size = 2.5, vjust = -1.) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  labs(title = "SST vs SST2")



ggplot(data = PCIC_day_summary, aes(x = as.Date("2019-12-31") + yday(time), y = tw_mean, ymin = tw_q10, ymax = tw_q90, colour = year)) +
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1) +
  geom_vline(xintercept = as.Date("2019-12-31") + timing_s, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-12-31") + timing_e, linetype = "dashed") +
  geom_text(aes(x = as.Date("2019-12-31") + timing_e, y = max(tw_mean), label = "Spawning period"), size = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_discrete(labels=c('1971-2000','2041-2070')) +
  labs(subtitle = "PCIC daily temperature within CU boundary accessible cells (80% range)", colour = "Year range",
       x = "Date", y = "Daily Temperature")


