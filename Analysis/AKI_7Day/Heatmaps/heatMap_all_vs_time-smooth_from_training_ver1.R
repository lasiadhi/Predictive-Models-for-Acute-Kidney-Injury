library(ggplot2)



# add a lab item column 
plot_data_dia$item <- rep('Diastolic Blood Pressure (mmHG)', dim(plot_data_dia)[1])
plot_data_hr$item <- rep('Heart Rate (Beats per Minute)', dim(plot_data_hr)[1])
#plot_data_mac$item <- rep('Minimum Alveolar Concentration', dim(plot_data_mac)[1])
plot_data_map$item <- rep('Mean Arterial Blood Pressure (mmHG)', dim(plot_data_map)[1])
plot_data_sys$item <- rep('Systolic Blood Pressure (mmHG)', dim(plot_data_sys)[1])


# rename column
names(plot_data_dia)[1] <- "value"
names(plot_data_hr)[1] <- "value"
#names(plot_data_mac)[1] <- "value"
names(plot_data_map)[1] <- "value"
names(plot_data_sys)[1] <- "value"


# append all plot_data
#plot_data <- rbind(plot_data_dia[c(1,2,6,7)], plot_data_hr[c(1,2,6,7)], plot_data_mac[c(1,2,6,7)], plot_data_map[c(1,2,6,7)], plot_data_sys[c(1,2,6,7)])
plot_data <- rbind(plot_data_dia[c(1,2,6,7)], plot_data_hr[c(1,2,6,7)], plot_data_map[c(1,2,6,7)], plot_data_sys[c(1,2,6,7)])


# plot data
plot_all <- ggplot(plot_data, aes(value,time,z=risk)) + 
  geom_tile(aes(fill=risk)) + 
  scale_fill_gradientn(colours=c("green","red","red2"), na.value="transparent") +
  labs(title="AKI-7Day Risk over the Vitals and Time", y="Time (mins)") + 
  geom_raster(aes(fill=risk), interpolate=TRUE) +
  guides(fill=guide_colorbar(barheight = 15)) + 
  facet_wrap(~item, ncol=2, scales="free")
print(plot_all)


# save plot 
ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_all.png", plot=plot_all)
ggsave(filename="Meixian/IDEALIST_IntraOp/Heatmaps/aki7day_all.eps", plot=plot_all, dpi=300)

