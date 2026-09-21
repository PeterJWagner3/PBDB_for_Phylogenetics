# set up program ####
common_r_sources <- '~/Documents/R_Projects/Common_R_Source_Files/';
data_for_R_folder <- '~/Documents/R_Projects/Data_for_R/';
source(paste(common_r_sources,"Chronos.r",sep=""));  #
source(paste(common_r_sources,"Data_Downloading_v4.r",sep=""));  #
source(paste(common_r_sources,"General_Plot_Templates.r",sep=""));  #
source(paste(common_r_sources,"Occurrence_Data_Routines.r",sep=""));  #

load(paste(data_for_R_folder,"Paleobiology_Database.RData",sep="")); # Paleobiology Database
load(paste(data_for_R_folder,"Gradstein_2020_Augmented.RData",sep="")); # Paleobiology Database

pbdb_sites <- pbdb_data_list$pbdb_sites_refined;
pbdb_finds <- pbdb_data_list$pbdb_finds;
pbdb_taxonomy <- pbdb_data_list$pbdb_taxonomy;
pbdb_opinions <- pbdb_data_list$pbdb_opinions;

time_scale <- gradstein_2020_emended$time_scale;

study_taxon <- "Ammonitida";
study_worker <- "P. Wagner";
study_time <- "Cretaceous";
study_onset <- time_scale$ma_lb[time_scale$interval %in% study_time];
study_end <- time_scale$ma_ub[time_scale$interval %in% study_time];

study_taxon_rank <- pbdb_taxonomy$accepted_rank[match(study_taxon,pbdb_taxonomy$taxon_name)]
study_sites <- pbdb_sites[pbdb_sites$ma_lb>study_end & pbdb_sites$ma_ub<study_onset,];
study_finds <- pbdb_finds[pbdb_finds[,study_taxon_rank] %in% study_taxon,];
study_sites <- study_sites[study_sites$collection_no %in% study_finds$collection_no,];
study_finds <- study_finds[study_finds$collection_no %in% study_sites$collection_no,];
study_sites <- study_sites[study_sites$enterer %in% "P. Wagner" | study_sites$modifier %in% "P. Wagner",];
study_finds <- study_finds[study_finds$enterer %in% "P. Wagner" | study_finds$modifier %in% "P. Wagner",];
nrow(study_sites)
nrow(study_finds)

study_taxonomy <- pbdb_taxonomy[pbdb_taxonomy$family_no %in% study_finds$family_no,];
study_opinions <- pbdb_opinions[pbdb_opinions$parent_no %in% study_taxonomy$taxon_no,];
study_taxonomy <- study_taxonomy[study_taxonomy$enterer %in% study_worker,];
study_opinions <- study_opinions[study_opinions$enterer %in% study_worker,];
study_taxonomy <- study_taxonomy[study_taxonomy$created>="2018-01-01 00:00:00",];
study_opinions <- study_opinions[study_opinions$created>="2018-01-01 00:00:00",];
nrow(study_taxonomy);
nrow(study_opinions);

time_scale <- time_scale[time_scale$ma_lb<=540,];
stage_scale <- time_scale[time_scale$chronostratigraphic_rank %in% "Stage" & time_scale$scale %in% "International",];
stage_scale <- stage_scale[stage_scale$interval_sr=="",];
stage_scale <- stage_scale[order(-stage_scale$ma_lb),];
period_scale <- time_scale[time_scale$chronostratigraphic_rank %in% "Period" & time_scale$scale %in% "International",];
period_scale <- period_scale[period_scale$interval_sr=="",];
period_scale <- period_scale[order(-period_scale$ma_lb),];
stage_slices <- time_scale[time_scale$scale %in% "Stage Slice",];
stage_slices <- stage_slices[order(-stage_slices$ma_lb),];
max_dur <- max(stage_slices$ma_lb-stage_slices$ma_ub);
stage_slices <- adjacent_stage_slice_lumping(time_scale_to_condense=stage_slices,too_short=2,max_length=max_dur);
nslices <- nrow(stage_slices);

pbdb_finds <- pbdb_data_list$pbdb_finds;
pbdb_sites <- pbdb_data_list$pbdb_sites_refined;
pbdb_taxonomy <- pbdb_data_list$pbdb_taxonomy;
age <- pbdb_sites$ma_lb;
pbdb_sites$interval_lb <- pbapply::pbsapply(age,rebin_collection_with_time_scale,"onset",stage_slices);
age <- pbdb_sites$ma_ub;
pbdb_sites$interval_ub <- pbapply::pbsapply(age,rebin_collection_with_time_scale,"end",stage_slices);

# Set aside Data for this Group ####
study_taxon <- "Blastozoa";
study_taxon_no <-unique(pbdb_taxonomy$accepted_no[pbdb_taxonomy$taxon_name %in% study_taxon]);
study_taxon_rank <- unique(pbdb_taxonomy$accepted_rank[pbdb_taxonomy$taxon_name %in% study_taxon])
if (study_taxon_rank %in% standard_pbdb_taxon_ranks)	{
	study_taxon_rank_no <- paste(study_taxon_rank,"_no",sep="");
	study_taxonomy <- pbdb_taxonomy[pbdb_taxonomy[,study_taxon_rank_no]==study_taxon_no,];
	study_genera <- data.frame(daughter=study_taxonomy$taxon_name[study_taxonomy$taxon_rank %in% "genus"],
							   daughter_rank=rep("genus",sum(study_taxonomy$taxon_rank %in% "genus")),
							   taxon_no=study_taxonomy$taxon_no[study_taxonomy$taxon_rank %in% "genus"]);
	} else	{
	study_taxon_daughters <- accersi_daughter_taxa_from_taxon_no(parent_taxon_no=study_taxon_no,pbdb_taxonomy=pbdb_taxonomy);
#	study_genera <- accersi_daughter_genera_from_taxon_no(pbdb_taxonomy$accepted_no[match(study_taxon,pbdb_taxonomy$taxon_name)],pbdb_taxonomy);
	study_genera <- data.frame(daughter=as.character(),daughter_rank=as.character(),taxon_no=as.numeric());
	for (st in 1:nrow(study_taxon_daughters))	{
		daughter_rank <- study_taxon_daughters$daughter_rank[st];
		daughter_rank_no <- paste(daughter_rank,"_no",sep="");
		dummy <- data.frame(daughter=pbdb_taxonomy$taxon_name[pbdb_taxonomy[,daughter_rank_no]==study_taxon_daughters$taxon_no[st]],
							daugher_rank=pbdb_taxonomy$taxon_rank[pbdb_taxonomy[,daughter_rank_no]==study_taxon_daughters$taxon_no[st]],
							taxon_no=pbdb_taxonomy$taxon_no[pbdb_taxonomy[,daughter_rank_no]==study_taxon_daughters$taxon_no[st]]);
		dummy <- dummy[dummy$daugher_rank %in% "genus",];
		study_genera <- rbind(study_genera,dummy);
		}
	}
#which(study_genera=="Minervaecystis",arr.ind = TRUE)
study_taxonomy <- pbdb_taxonomy[pbdb_taxonomy$accepted_no %in% study_genera$taxon_no,];
study_taxonomy <- study_taxonomy[study_taxonomy$phylum %in% pbdb_taxonomy$phylum[match(study_taxon,pbdb_taxonomy$accepted_name)],];
study_finds <- pbdb_finds[pbdb_finds$genus_no %in% study_genera$taxon_no,];
study_finds <- study_finds[study_finds$phylum %in% pbdb_taxonomy$phylum[match(study_taxon,pbdb_taxonomy$accepted_name)],];
study_finds <- study_finds[study_finds$identified_rank %in% c("species","subspecies"),];
crinoid_finds <- study_finds[study_finds$class %in% "Crinoidea",];
blastozoid_finds <- study_finds[!study_finds$class %in% "Crinoidea",];

study_sites <- pbdb_sites[pbdb_sites$collection_no %in% study_finds$collection_no,];
if (is.null(stage_slices$bin_first))	{
	stage_slices$bin_first <- 1:nrow(stage_slices);
	stage_slices$bin_last <- 1:nrow(stage_slices);
	}
crinoid_sites <- study_sites[study_sites$collection_no %in% crinoid_finds$collection_no,];
blastozoid_sites <- study_sites[study_sites$collection_no %in% blastozoid_finds$collection_no,];
bsites <- nrow(blastozoid_sites);
crinoid_sites_pal <- crinoid_sites[crinoid_sites$ma_lb>time_scale$ma_lb[time_scale$interval=="Triassic"],];
cpsites <- nrow(crinoid_sites_pal);
shared_sites <- blastozoid_sites[blastozoid_sites$collection_no %in% crinoid_sites_pal$collection_no,];
shsites <- nrow(shared_sites);

unique_enterers <- unique(sort(study_finds$enterer));
enterer_stats <- data.frame(enterer=unique_enterers,entries=hist(match(study_finds$enterer,unique_enterers),breaks=0:length(unique_enterers),plot=FALSE)$counts);
enterer_stats <- enterer_stats[order(-enterer_stats$entries),]
enterer_stats[1:10,]

# summarize information per stratigraphic unit
finds_per_stage_slice <- tally_occurrences_per_subinterval(taxon_collections=study_sites,taxon_finds=study_finds,hierarchical_chronostrat = stage_slices);
sites_per_stage_slice <- tally_collections_occupied_by_subinterval(taxon_collections=study_sites,hierarchical_chronostrat = stage_slices);
rocks_per_stage_slice <- tally_rock_units_occupied_by_subinterval(taxon_collections=study_sites,hierarchical_chronostrat = stage_slices);

# Setup Plots ####
standard_time_scale <- period_scale;
strat_unit <- period_scale$interval;
# Make sure that ages on time scale are negatives
#standard_time_scale$ma_ub[standard_time_scale$ma_ub == 143.7] <- 143.1;
#standard_time_scale$ma_lb[standard_time_scale$ma_lb == 143.7] <- 143.1;
standard_time_scale$ma_lb <- -abs(standard_time_scale$ma_lb);
standard_time_scale$ma_ub <- -abs(standard_time_scale$ma_ub);
# Make sure that time scale is correct order, with oldest first
#standard_time_scale <- standard_time_scale[abs(standard_time_scale$ma_ub)< max(study_sites$ma_lb),];

standard_time_scale <- standard_time_scale[order(standard_time_scale$ma_lb),];
# Add names/symbols for plotting
standard_time_scale$strat_names <- standard_time_scale$st;
strat_names <- standard_time_scale$strat_names;
strat_names[strat_names=="Q"] <- "";

time_scale <- gradstein_2020_emended$time_scale[gradstein_2020_emended$time_scale$chronostratigraphic_rank %in% "Era" & gradstein_2020_emended$time_scale$scale %in% "International",];
time_scale <- time_scale[time_scale$interval_sr=="",];
time_scale$ma_lb <- -time_scale$ma_lb; time_scale$ma_ub <- -time_scale$ma_ub;
time_scale <- time_scale[order(time_scale$ma_lb),];
time_scale <- time_scale[time_scale$ma_ub>min(standard_time_scale$ma_lb),];
time_scale <- time_scale[time_scale$ma_lb<max(standard_time_scale$ma_ub),];
time_scale <- time_scale[time_scale$interval_sr=="",];
time_scale$ma_lb[time_scale$interval=="Phanerozoic"] <- -538.8;

# Add names/symbols for plotting
strat_names_minor <- standard_time_scale$st;
strat_names_major <- time_scale$interval;
strat_names_minor[strat_names_minor=="Q"] <- strat_names_minor[strat_names_minor=="Had"] <- "";
strat_names_major[strat_names_major %in% "Neoproterozoic"] <- "Neopr";
# Get the colors for time units
strat_colors_minor <- standard_time_scale$color;
strat_colors_major <- time_scale$color;
# Set the oldest and youngest intervals by names on whatever chronostratigraphic scale you used in the analysis
oldest_interval <- standard_time_scale$interval[1];
youngest_interval <- standard_time_scale$interval[nrow(standard_time_scale)];
# get the time scale that will be plotted
time_scale_to_plot_minor <- unique(c(standard_time_scale$ma_lb,standard_time_scale$ma_ub));
time_scale_to_plot_major <- unique(c(time_scale$ma_lb,time_scale$ma_ub));
time_scale_to_plot_minor[time_scale_to_plot_minor <= -max(study_sites$ma_lb)] <- -max(study_sites$ma_lb);
time_scale_to_plot_major[time_scale_to_plot_major <= -max(study_sites$ma_lb)] <- -max(study_sites$ma_lb);

# Now, set the onset and end of the x-axis
onset <- min(time_scale_to_plot_minor);
end <- max(time_scale_to_plot_minor);
# Finally, set up breaks for x-axis
#yearbreaks <- c(5,25,50);					# set breaks for x-axis (minor, medium & major)
yearbreaks <- sort(as.numeric(set_axis_breaks(max_no=end,min_no=onset)));
# now, set up the y-axis: this will reflect your data

use_strat_labels <- T;						# if T, then strat_names_minor will be plotted on X-axis inside boxes
alt_back <- F;								# if T, then the background will alternat shades between major intervals
plot_title <- "";							# Name of the plot; enter "" for nothing
hues <- "T";								# If T, then IGN stratigraphic colors will be used
colored <- "base";							# Where IGN stratigraphic colors should go
xsize <- 7;
ysize <- xsize*(4.285714285/6);

myr_size <- 1;
strat_label_size_minor <- 10;
strat_label_size_major <- 0.9;

# Plot collections per Stage Slice ####
ordinate <- "  Sites per Stage Slice";								# Label of Y-axis
mxy <- ceiling(max(sites_per_stage_slice)/10)*10;
mny <- 0;									# set maximum y value
ybreaks <- as.numeric(set_axis_breaks_new(mxy));
plot_title <- "";
Phanerozoic_Timescale_Plot_Hierarchical(onset,end,time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/100),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,plot_title,ordinate,abscissa="Ma",yearbreaks,xsize=xsize,ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,xlab_size=1.5,myr_size=myr_size,font=franky,ylab_size = 1.25,ylab_off = 3.5);
specified_axis(axe=2,max_val=mxy,min_val=mny,maj_break=ybreaks[1],med_break=ybreaks[2],min_break=ybreaks[3],linewd=4/3,orient=2,print_label=TRUE,font="Franklin Gothic Medium");
ns <- 0;
while (ns < nslices)	{
	ns <- ns+1;
	if (sites_per_stage_slice[ns]>0)
		rect(-stage_slices$ma_lb[ns],0,-stage_slices$ma_ub[ns],min(mxy,sites_per_stage_slice[ns]),col=stage_slices$color[ns],lwd=0.25);
	}
for (ns in 1:nslices)	if (sites_per_stage_slice[ns]>0)	rect(-stage_slices$ma_lb[ns],0,-stage_slices$ma_ub[ns],min(mxy,sites_per_stage_slice[ns]),col=stage_slices$color[ns],lwd=0.25);

# Plot Rock Units per Stage Slice ####
ordinate <- "  Rock Units per Stage Slice";								# Label of Y-axis
mxy <- ceiling(max(rocks_per_stage_slice)/5)*5;
mny <- 0;									# set maximum y value
ybreaks <- as.numeric(set_axis_breaks_new(mxy));
plot_title <- "";
Phanerozoic_Timescale_Plot_Hierarchical(onset,end,time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/100),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,plot_title,ordinate,abscissa="Ma",yearbreaks,xsize=xsize,ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,xlab_size=1.5,myr_size=myr_size,font=franky,ylab_size = 1.25,ylab_off = 3.5);
specified_axis(axe=2,max_val=mxy,min_val=mny,maj_break=ybreaks[1],med_break=ybreaks[2],min_break=ybreaks[3],linewd=4/3,orient=2,print_label=TRUE,font="Franklin Gothic Medium");
for (ns in 1:nslices)	if (sites_per_stage_slice[ns]>0)	rect(-stage_slices$ma_lb[ns],0,-stage_slices$ma_ub[ns],min(mxy,rocks_per_stage_slice[ns]),col=stage_slices$color[ns],lwd=0.25);


{}


# Fixing data entry errors ####
study_sites[study_sites$ma_lb<50,]
