
### Downstream analysis

## functions for determining downstream paths from within CU boundaries or any other stream in the FWA



#ggplot() +
#  geom_histogram(data = streams, aes(x= stream_order, color = ))



# 
# 
# 
# 
# for(n in 1:st_level) {
#   
#   #get FWA code for current FWA level of iteration
#   c_cut <- (n-1) * 7
#   if(n == st_level) c_cut <- (n-1) * 7
#   s_pick <- str_sub(s_code, 1, st - c_cut)
#   s_pick <- str_c(s_pick, "-000000")
#   
#   #get candidate streams with lower FWA code
#   ind <- str_starts(FWA_high$FWA_WATERS, s_pick)
#   temp <- FWA_high[ind,] %>%
#     filter(STREAM_ORD >= max(sub_FWA$STREAM_ORD), STREAM_MAG >= max(sub_FWA$STREAM_MAG))
#   
#   #get streams with lower FWA code that intersect with migration reaches
#   FWA_int <- st_intersects(temp, sub_FWA, sparse = FALSE)
#   FWA_int <- temp[which(apply(FWA_int, 1, sum) > 0),]
#   # #subset streams with correct FWA code
#   # ind <- str_starts(FWA_int$FWA_WATERS, s_pick)
#   # FWA_int <- FWA_int[ind,]
#   
#   #get downstream range from lowest intersecting reach
#   dd <- min(FWA_int$DOWNSTREAM)
#   
#   #take candidate streams with lower downstream range
#   low_stream <- filter(temp, DOWNSTREAM <= dd)
#   #if(n==1) low_stream <- pick_FWA
#   #convert to points
#   #test <- st_coordinates(low_stream)
#   #test <- st_multipoint(test[,1:3])
#   
#   #FWA_int <- filter(FWA_int, DOWNSTREAM == min(DOWNSTREAM))
#   
#   #take all streams with DOWNSTREAM distance below intersect
#   temp <- filter(temp, DOWNSTREAM <= dd)
#   sub_FWA <- bind_rows(sub_FWA, low_stream)
#   
#   # if(sub_FWA$DOWNSTREAM > 0)
#   # {
#   # 
#   # }
#   
# }
