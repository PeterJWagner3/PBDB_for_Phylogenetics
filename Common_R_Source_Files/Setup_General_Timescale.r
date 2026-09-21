# Load Time Scale ####
load(paste(data_for_R_folder,"Gradstein_2020_Augmented.RData",sep="")); # refined Gradstein 2012 timescale & biozonations
time_scale <- gradstein_2020_emended$time_scale;
time_scale$ma_ub[time_scale$interval=="Meghalayan"] <- 0;
time_scale$st[time_scale$interval=="Late Cretaceous"] <- "lK";
#earliest_interval <- "Maastrichtian";
#latest_interval <- "Recent";
#tier_one_rank="stage";
#tier_two_rank="epoch";
#onset=NULL;end=NULL;
#setup_general_time_scale <- function(earliest_interval,latest_interval,timescale,tier_one_rank="stage",tier_two_rank="period",scale="international",onset=NULL,end=NULL)	{
relv_time_scale <- time_scale[tolower(time_scale$scale) %in% tolower(scale) & time_scale$interval_sr %in% "",];
if (latest_interval=="Recent" && is.null(end))	end <- 0;

# Setup time scale ####
tier_one_scale <- relv_time_scale[tolower(relv_time_scale$chronostratigraphic_rank) %in% tolower(tier_one_rank),];
tier_two_scale <- relv_time_scale[tolower(relv_time_scale$chronostratigraphic_rank) %in% tolower(tier_two_rank),];
tier_one_scale <- tier_one_scale[order(-abs(tier_one_scale$ma_lb)),];
tier_two_scale <- tier_two_scale[order(-abs(tier_two_scale$ma_lb)),];
if (is.null(onset))
	onset <- timescale$ma_lb[match(earliest_interval,timescale$interval)]
if (is.null(end))
	end <- timescale$ma_ub[match(latest_interval,timescale$interval)]
tier_one_scale <- tier_one_scale[tier_one_scale$ma_lb>end & tier_one_scale$ma_ub<abs(onset),];
tier_two_scale <- tier_two_scale[tier_two_scale$ma_lb>end & tier_two_scale$ma_ub<abs(onset),];
tier_one_scale$ma_lb[tier_one_scale$ma_lb>abs(onset)] <- abs(onset);
tier_two_scale$ma_lb[tier_two_scale$ma_lb>abs(onset)] <- abs(onset);
tier_one_scale$ma_ub[tier_one_scale$ma_ub<abs(end)] <- abs(end);
tier_two_scale$ma_ub[tier_two_scale$ma_ub<abs(end)] <- abs(end);

tier_one_scale$span <- abs(tier_one_scale$ma_lb-tier_one_scale$ma_ub);
strat_names_minor <- tier_one_scale$st;
strat_names_minor[(xsize*tier_one_scale$span/abs(onset-end))<0.1] <- "";
tier_two_scale$span <- abs(tier_two_scale$ma_lb-tier_two_scale$ma_ub);
strat_names_major <- tier_two_scale$interval;
strat_names_major[(xsize*tier_two_scale$span/abs(onset-end))<0.5] <- tier_two_scale$st[(xsize*tier_two_scale$span/abs(onset-end))<0.5];
strat_names_major[(xsize*tier_two_scale$span/abs(onset-end))<0.1] <- "";
# Make sure that ages on time scale are negatives
tier_one_scale$ma_lb <- -abs(tier_one_scale$ma_lb);
tier_one_scale$ma_ub <- -abs(tier_one_scale$ma_ub);
tier_two_scale$ma_lb <- -abs(tier_two_scale$ma_lb);
tier_two_scale$ma_ub <- -abs(tier_two_scale$ma_ub);

# Add names/symbols for plotting
# Get the colors for time units
strat_colors_minor <- tier_one_scale$color;
strat_colors_major <- tier_two_scale$color;

# get the time scale that will be plotted
time_scale_to_plot_minor <- unique(c(tier_one_scale$ma_lb,tier_one_scale$ma_ub[nrow(tier_one_scale)]));
time_scale_to_plot_major <- unique(c(tier_two_scale$ma_lb,tier_two_scale$ma_ub[nrow(tier_two_scale)]));
time_scale_to_plot_minor[time_scale_to_plot_minor < -abs(onset)] <- -abs(onset);
time_scale_to_plot_major[time_scale_to_plot_major < -abs(onset)] <- -abs(onset);
time_scale_to_plot_minor[time_scale_to_plot_minor > -abs(end)] <- -abs(end);
time_scale_to_plot_major[time_scale_to_plot_major > -abs(end)] <- -abs(end);
strat_font_colors_minor <- tier_one_scale$font_color;
names(strat_font_colors_minor) <- tier_one_scale$interval;
strat_font_colors_major <- tier_two_scale$font_color;
names(strat_font_colors_major) <- tier_two_scale$interval;

# Now, set the onset and end of the x-axis
onset <- -abs(onset);
end <- -abs(end);
#print(paste(onset,end));
# Finally, set up breaks for x-axis
#yearbreaks <- c(5,25,50);					# set breaks for x-axis (minor, medium & major)
#yearbreaks <- as.numeric(set_axis_breaks_new(end-onset));
#print("62");
# now, set up the y-axis: this will reflect your data

use_strat_labels <- T;						# if T, then strat_names_minor will be plotted on X-axis inside boxes
alt_back <- F;								# if T, then the background will alternat shades between major intervals
plot_title <- "";							# Name of the plot; enter "" for nothing
ordinate <- "";								# Label of Y-axis
hues <- TRUE;								# If T, then IGN stratigraphic colors will be used
colored <- "base";							# Where IGN stratigraphic colors should go
#mny <- 0;
yearbreaks <- sort(as.numeric(set_axis_breaks_new(max(abs(time_scale_to_plot_minor))-min(abs(time_scale_to_plot_minor)))));
yearbreaks <- sort(yearbreaks);

myr_size <- 1;
names(strat_font_colors_minor) <- names(strat_colors_minor) <- strat_names_minor;
names(strat_font_colors_major) <- names(strat_colors_major) <- strat_names_major;

strat_label_size_minor <- 1/2;
strat_label_size_major <- 2/3;
{}
