#### Setup Program ####
# Setup Your Analysis
data_for_R_folder <- "~/Documents/R_Projects/Data_for_R/";
pbdb_directory <- "~/Documents/R_Projects/PaleoDB_Stuff/";
common_source_folder <- "~/Documents/R_Projects/Common_R_Source_Files/";
setwd(pbdb_directory);

gap <- INAP <- -22;
UNKNOWN <- missing <- -11;
polymorphs <- TRUE;	# if false, then these are converted to unknowns

#devtools::install_github("kassambara/r2excel");
#library(xlsx);
source('~/Documents/R_Projects/Common_R_Source_Files/Chronos.r'); 		#
source('~/Documents/R_Projects/Common_R_Source_Files/Data_Downloading_v4.r');	#
source('~/Documents/R_Projects/Common_R_Source_Files/General_Plot_Templates.r');	#
source('~/Documents/R_Projects/Common_R_Source_Files/Historical_Diversity_Metrics.r');	#
source('~/Documents/R_Projects/Common_R_Source_Files/Nexus_File_Routines.r'); 		#
source('~/Documents/R_Projects/Common_R_Source_Files/Occurrence_Data_Routines.r'); 		#
source('~/Documents/R_Projects/Common_R_Source_Files/Sampling_and_Occupancy_Distributions.r'); 		#
source('~/Documents/R_Projects/Common_R_Source_Files/Stratigraphy.r'); 		#
source('~/Documents/R_Projects/Common_R_Source_Files/Wagner_kluges.r'); 		#
source('~/Documents/R_Projects/Common_R_Source_Files/Wagner_Stats_and_Probability_101.r'); 		#

franky <- "Franklin Gothic Medium";
load("~/Documents/R_Projects/Data_for_R/Paleobiology_Database.RData"); # PBDB Data
load("~/Documents/R_Projects/Data_for_R/Gradstein_2020_Augmented.RData"); # PBDB Data
load("~/Documents/R_Projects/Data_for_R/Rock_Unit_Database.RData"); # PBDB Data

time_scale <- gradstein_2020_emended$time_scale;
stage_slices <- time_scale[time_scale$scale %in% "Stage Slice",];
stage_scale <- time_scale[time_scale$chronostratigraphic_rank %in% "Stage" & time_scale$scale %in% "International",];
stage_scale <- stage_scale[stage_scale$interval_sr=="",];
finest_chronostrat <- time_scale[time_scale$scale %in% "Stage Slice",];
finest_chronostrat <- finest_chronostrat[order(-finest_chronostrat$ma_lb),];
finest_chronostrat <- lump_two_plus_intervals(finest_chronostrat,intervals_to_lump=c("Ka1","Ka2"),new_interval_name="Ka1-2"); 
finest_chronostrat <- lump_two_plus_intervals(finest_chronostrat,intervals_to_lump=c("Ka3","Ka4"),new_interval_name="Ka3-4"); 
finest_chronostrat <- adjacent_stage_slice_lumping(time_scale_to_condense=finest_chronostrat);

epoch_scale <- time_scale[time_scale$chronostratigraphic_rank %in% "Epoch" & time_scale$scale %in% "International",];
epoch_scale <- epoch_scale[epoch_scale$interval_sr=="",];
rock_to_zone_database <- rock_unit_data$rock_to_zone_database;
rock_unit_database <- rock_unit_data$rock_unit_database;

pbdb_sites <- pbdb_data_list$pbdb_sites_refined;
pbdb_finds <- pbdb_data_list$pbdb_finds;
pbdb_finds_old_ids <- pbdb_data_list$pbdb_finds_oldid;
pbdb_taxonomy <- pbdb_data_list$pbdb_taxonomy;
pbdb_opinions <- pbdb_data_list$pbdb_opinions;
pbdb_references <- pbdb_data_list$pbdb_references;

#pbdb_finds$identified_name[pbdb_finds$occurrence_comments %in% c("Balaenopteridae indet aka portisi MGPT 13803","Balaenopteridae indet aka portisi MRSN PU13808","Balaenopteridae indet aka portisi MCZ 17882","Balaenopteridae indet aka portisi SDSNH 21507","Balaenopteridae indet aka portisi SDSNH 65769 et 68698")] <- pbdb_finds$accepted_name[pbdb_finds$occurrence_comments %in% c("Balaenopteridae indet aka portisi MGPT 13803","Balaenopteridae indet aka portisi MRSN PU13808","Balaenopteridae indet aka portisi MCZ 17882","Balaenopteridae indet aka portisi SDSNH 21507","Balaenopteridae indet aka portisi SDSNH 65769 et 68698")] <- "Balaenoptera floridana";
#pbdb_finds$identified_no[pbdb_finds$occurrence_comments %in% c("Balaenopteridae indet aka portisi MGPT 13803","Balaenopteridae indet aka portisi MRSN PU13808","Balaenopteridae indet aka portisi MCZ 17882","Balaenopteridae indet aka portisi SDSNH 21507","Balaenopteridae indet aka portisi SDSNH 65769 et 68698")] <- pbdb_finds$accepted_no[pbdb_finds$occurrence_comments %in% c("Balaenopteridae indet aka portisi MGPT 13803","Balaenopteridae indet aka portisi MRSN PU13808","Balaenopteridae indet aka portisi MCZ 17882","Balaenopteridae indet aka portisi SDSNH 21507","Balaenopteridae indet aka portisi SDSNH 65769 et 68698")] <- pbdb_taxonomy$taxon_no[match("Balaenoptera floridana",pbdb_taxonomy$taxon_name)];

# Setup Specifics for This Analysis ####
##### Select Nexus File for analysis ####
nexus_file_name <- file.choose();
#nexus_file_name <- "/Users/peterjwagner/Documents/R_Projects/Rev_Bayes_Setup/Ordovician_Proetida.nex";
#nexus_file_name <- "/Users/peterjwagner/Documents/R_Projects/Code_for_Paper_Reviews/doi_10_5061_dryad_vq83bk3qf__v20210121/Cetacean Metatree Data/Metatree data/Safe/FULL.nex";
char_data <- accersi_data_from_nexus_file(nexus_file_name,polymorphs,UNKNOWN,INAP);
otus <- char_data$OTUs;
notu <- length(otus);
accepted_otus <- pbdb_taxonomy$accepted_name[match(otus,pbdb_taxonomy$taxon_name)];
unentered_otus <- char_data$OTUs[is.na(pbdb_taxonomy$accepted_name[match(otus,pbdb_taxonomy$taxon_name)])]

# look to see if there are apparently unentered otus because of subgenus issues;
uo <- 0;
while (uo < length(unentered_otus))	{
	uo <- uo+1;
	otu_no <- match(unentered_otus[uo],otus);
	if (is.species(unentered_otus[uo]) | is.subspecies(unentered_otus[uo]))	{
		subgenus_involved <- FALSE;
		otu_genus <- otu_parent_genus <- otu_subgenus <- divido_genus_names_from_species_names(unentered_otus[uo]);
		otu_epithet <- divido_species_epithets(unentered_otus[uo]);
		if (is.subgenus(otu_genus))	{
			subgenus_involved <- TRUE:
			otu_subgenus <- elevate_subgenus_to_genus(otu_subgenus);
			otu_parent_genus <- strsplit(otu_genus," ")[[1]][1];
			}
		if (subgenus_involved)	{
			combo1 <- paste(otu_parent_genus,otu_epithet);
			combo2 <- paste(otu_subgenus,otu_epithet);
			pbdb_taxonomy$taxon_name %in% c(combo1,combo2)
			} else	{
			genus_no <- pbdb_taxonomy$accepted_no[match(otu_genus,pbdb_taxonomy$taxon_name)]
			suspects <- pbdb_taxonomy[gsub(otu_genus,"",pbdb_taxonomy$taxon_name)!=pbdb_taxonomy$taxon_name,]
			suspects <- suspects[suspects$taxon_rank %in% c("species","subspecies"),];
			if (sum(gsub(otu_epithet,"",suspects$taxon_name)!=suspects$taxon_name)>0)	{
				accepted_otus[otu_no] <- suspects$accepted_name[gsub(otu_epithet,"",suspects$taxon_name)!=suspects$taxon_name];
				unentered_otus <- unentered_otus[!unentered_otus %in% unentered_otus[uo]];
				uo <- uo-1;
				}
			}
		}
	}

# if there are junior synonyms coded separately from their senior synonym
#	then restore their names to the accepted names on their finds.
#	Note: if the senior synonym is not coded separately, 
#		then just leave things be.
junior_otus <- otus[pbdb_taxonomy$difference[match(otus,pbdb_taxonomy$taxon_name)] %in% obsolete]
jo <- 0;
while (jo < length(junior_otus))	{
	jo <- jo+1;
	otu_no <- match(junior_otus[jo],otus);
	if (accepted_otus[otu_no] %in% accepted_otus[!(1:notu) %in% otu_no])	{
#	if (accepted_otus[match(junior_otus[jo],otus)] %in% otus)
		accepted_otus[match(junior_otus[jo],otus)] <- junior_otus[jo];
		pbdb_finds <- restore_junior_synonyms(junior_otus,pbdb_finds,pbdb_taxonomy);
		}
	}

unentered_otu_finds <- pbdb_finds[pbdb_finds$identified_name %in% unentered_otus | pbdb_finds$occurrence_comments %in% unentered_otus,];
accepted_otu_finds <- pbdb_finds[pbdb_finds$accepted_name %in% accepted_otus,];
findless_otus <- accepted_otus[!accepted_otus %in% accepted_otu_finds$accepted_name]

type_localities <- pbapply::pbsapply(otus,find_species_type_localities,pbdb_finds,pbdb_sites);
type_localities[type_localities==0] <- pbapply::pbsapply(accepted_otus[type_localities==0],find_species_type_localities,pbdb_finds,pbdb_sites);
write.csv(data.frame(species=otus[type_localities==0],type_localities=type_localities[type_localities==0]),"Missing_Type_Localities.csv",row.names = FALSE);

fossil_info <- data.frame(taxon=otus,nfinds=hist(match(accepted_otu_finds$accepted_name,accepted_otus),breaks=0:notu,plot=FALSE)$counts,
						  fa_lb=rep(0,notu),fa_ub=rep(0,notu),
						  la_lb=rep(0,notu),la_ub=rep(0,notu));
for (tx in 1:notu)	{
	taxon_sites <- pbdb_sites[pbdb_sites$collection_no %in% pbdb_finds$collection_no[pbdb_finds$accepted_name %in% accepted_otus[tx]],];
	if (fossil_info$nfinds[tx]>0)	{
		fossil_info$fa_lb[tx] <- round(max(taxon_sites$ma_lb),3);
		fossil_info$fa_ub[tx] <- round(max(taxon_sites$ma_ub),3);
		fossil_info$la_lb[tx] <- round(min(taxon_sites$ma_lb),3);
		fossil_info$la_ub[tx] <- round(min(taxon_sites$ma_ub),3);
		}
	}
exact_nexus_file_name <- strsplit(nexus_file_name,"/")[[1]][length(strsplit(nexus_file_name,"/")[[1]])];
fossil_info_file <- gsub(".nex","_Fossil_Info.tsv",exact_nexus_file_name);
rev_bayes_fossil_info <- data.frame(taxon=fossil_info$taxon,
									min_age=round(fossil_info$fa_ub-min(fossil_info$fa_ub),3),
									max_age=round(fossil_info$fa_lb-min(fossil_info$fa_ub),3));
rev_bayes_fossil_info$taxon <- sapply(rev_bayes_fossil_info$taxon,nexify_otu_name);
rev_bayes_rate_breaks_stages <- c("Floian","Darriwilian","Sandbian","Katian","Hirnantian","Silurian");
rev_bayes_rate_breaks <- sort(round(time_scale$ma_lb[match(rev_bayes_rate_breaks_stages,time_scale$interval)]-min(fossil_info$fa_ub),3));
#paste("timeline <- v(",paste(rev_bayes_rate_breaks,collapse = ","),");",sep="")
write.table(file=fossil_info_file,x=rev_bayes_fossil_info,sep="\t",row.names = FALSE,quote=FALSE);
write.csv(fossil_info,gsub(".nex","_Stratigraphic_Range_Summary.csv",exact_nexus_file_name),row.names = FALSE);

# Get presence/absence matrix of OTUs ####
study_onset <- max(fossil_info$fa_lb);
study_end <- min(fossil_info$la_ub);
study_sites <- pbdb_sites[pbdb_sites$ma_lb>study_end & pbdb_sites$ma_ub<study_onset,];
finest_chronostrat <- finest_chronostrat[finest_chronostrat$ma_lb>study_end & finest_chronostrat$ma_ub<study_onset,];
finest_chronostrat$initial_stage[gsub("Ka","",finest_chronostrat$initial_stage)!=finest_chronostrat$initial_stage] <- "Ka";
nbins <- nrow(finest_chronostrat);
finest_chronostrat$ma_md <- (finest_chronostrat$ma_lb+finest_chronostrat$ma_ub)/2;

study_sites$interval_lb <- pbapply::pbsapply(study_sites$ma_lb,rebin_collection_with_time_scale,"onset",finest_chronostrat);
study_sites$interval_ub <- pbapply::pbsapply(study_sites$ma_ub,rebin_collection_with_time_scale,"end",finest_chronostrat);
study_sites$bin_lb <- match(study_sites$interval_lb,finest_chronostrat$interval);
study_sites$bin_ub <- match(study_sites$interval_ub,finest_chronostrat$interval);
otu_pres_per_bin <- otu_sites_per_bin <- base::t(pbapply::pbsapply(accepted_otus,tally_collections_occupied_by_subinterval_sapply,all_finds=pbdb_finds,all_sites=pbdb_sites,hierarchical_chronostrat=finest_chronostrat));
otu_pres_per_bin[otu_pres_per_bin<0.5] <- 0;
otu_pres_per_bin[otu_pres_per_bin>=0.5] <- 1;

for (sp in 1:notu)	{
	if (rowSums(otu_pres_per_bin)[sp]>1)	{
		otu_pres_per_bin[sp,min((1:nbins)[otu_pres_per_bin[sp,]>0]):max((1:nbins)[otu_pres_per_bin[sp,]>0])] <- 1;
		}
	}

scale <- "International";
earliest_interval <- "Stage 10"; latest_interval <- "Gorstian";
tier_one_rank <- time_scale$chronostratigraphic_rank[match(earliest_interval,time_scale$interval)];
tier_two_rank <- time_scale$chronostratigraphic_rank[match("Ordovician",time_scale$interval)];
xsize <- 6.0; ysize <- 5*4.285714285/xsize;
onset <- time_scale$ma_lb[time_scale$interval %in% earliest_interval];
end <- time_scale$ma_ub[time_scale$interval %in% latest_interval];
source(paste(common_source_folder,"Setup_General_Timescale.r",sep=""));

ordinate <- "  Species Richness (OTUs Only)";
ylab_off <- 3.5;
standing_richness <- colSums(otu_pres_per_bin);
mxy <- max(colSums(otu_pres_per_bin));
mny <- 0;
ybreaks <- as.numeric(set_axis_breaks_new(mxy));
ybreaks[ybreaks<1] <- 1;
Phanerozoic_Timescale_Plot_Hierarchical(-abs(onset),-abs(end),time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/20),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,strat_font_colors_minor,strat_font_colors_major,plot_title,ordinate,abscissa="Ma",yearbreaks=yearbreaks,hierarchical_partition=TRUE,xsize=xsize,ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,xlab_size=1.5,myr_size=myr_size,font=franky,ylab_size = 1.25,ylab_off = ylab_off);
#specified_axis(axe=2,max_val=mxy,min_val=mny,maj_break=ybreaks[1],med_break = ybreaks[2],min_break=ybreaks[3],font=franky,orient=2)
specified_axis(axe=2,max_val=mxy,min_val=mny,maj_break=ybreaks[1],med_break=ybreaks[2],min_break=ybreaks[3],font=franky,orient=2);
# plot standing richness
lines(-finest_chronostrat$ma_md[match(names(standing_richness[standing_richness>0]),finest_chronostrat$interval)],standing_richness[standing_richness>0],lwd=2.5)
lines(-finest_chronostrat$ma_md[match(names(standing_richness[standing_richness>0]),finest_chronostrat$interval)],standing_richness[standing_richness>0],lwd=2.0,col="orange2");
rev_bayes_fas <- hist(match(sapply(fossil_info$fa_ub,rebin_collection_with_time_scale,"end",finest_chronostrat),finest_chronostrat$interval),breaks=0:nbins,plot=FALSE)$counts;
names(rev_bayes_fas) <- finest_chronostrat$interval;
points(-finest_chronostrat$ma_md[match(names(rev_bayes_fas[rev_bayes_fas>0]),finest_chronostrat$interval)],rev_bayes_fas[rev_bayes_fas>0],pch=24,bg="orange1");

otu_higher_taxa <- unique(pbdb_taxonomy[pbdb_taxonomy$taxon_name %in% accepted_otus,c("order","family")]);
otu_higher_taxa <- otu_higher_taxa[order(otu_higher_taxa$order,otu_higher_taxa$family),]
otu_higher_taxa$species_counts <- hist(match(pbdb_taxonomy[pbdb_taxonomy$taxon_name %in% accepted_otus,"family"],otu_higher_taxa$family),breaks=0:nrow(otu_higher_taxa),plot=FALSE)$counts;

study_interval_sites <- pbdb_sites[pbdb_sites$ma_lb>study_end & pbdb_sites$ma_ub<study_onset,];
study_interval_finds <- pbdb_finds[pbdb_finds$collection_no %in% study_interval_sites$collection_no & pbdb_finds$family %in% otu_higher_taxa$family & pbdb_finds$identified_rank %in% c("species","subspecies"),];
study_interval_finds <- study_interval_finds[!sapply(study_interval_finds$accepted_name,revelare_informal_taxa),];
study_interval_finds <- study_interval_finds[gsub("uncertain species","",study_interval_finds$flags)==study_interval_finds$flags,];
#accepted_otu_finds$occurrence_no[!accepted_otu_finds$occurrence_no %in% study_interval_finds$occurrence_no]
#study_interval_sites[study_interval_sites$collection_no==accepted_otu_finds$collection_no[464],]

study_interval_finds$identified_name <- gsub(" n. sp.","",study_interval_finds$identified_name);
study_interval_finds$identified_name <- gsub(" n. gen.","",study_interval_finds$identified_name);
study_interval_finds$identified_name <- gsub(" n. subgen.","",study_interval_finds$identified_name);
study_interval_finds$identified_name <- gsub("cf. ","",study_interval_finds$identified_name);
study_interval_finds$identified_name <- gsub(" sensu lato","",study_interval_finds$identified_name);
study_interval_finds$identified_name <- gsub("\\? ","",study_interval_finds$identified_name);
study_interval_finds$identified_name <- gsub("\"","",study_interval_finds$identified_name);
study_interval_finds$identified_name[study_interval_finds$identified_name %in% "Lepdioproetus (Dipharangus) xeo"] <- "Lepidoproetus (Dipharangus) xeo";
sfinds <- nrow(study_interval_finds);
study_interval_species_taxonomy <- unique(data.frame(species=study_interval_finds$accepted_name,ided_species=study_interval_finds$identified_name,
													 taxon_no=study_interval_finds$accepted_no,entered_rank=pbdb_taxonomy$accepted_rank[match(study_interval_finds$accepted_no,pbdb_taxonomy$taxon_no)],
													 family=study_interval_finds$family,order=study_interval_finds$order));
study_interval_species_taxonomy <- study_interval_species_taxonomy[order(study_interval_species_taxonomy$family,study_interval_species_taxonomy$species),]
unique_species_no <- unique(study_interval_species_taxonomy$taxon_no[study_interval_species_taxonomy$entered_rank %in% c("species","subspecies")]);
study_interval_species_taxonomy$ided_species[study_interval_species_taxonomy$taxon_no %in% unique_species_no] <- pbdb_taxonomy$accepted_name[match(study_interval_species_taxonomy$taxon_no[study_interval_species_taxonomy$taxon_no %in% unique_species_no],pbdb_taxonomy$taxon_no)];
study_interval_species_taxonomy$species[study_interval_species_taxonomy$taxon_no %in% unique_species_no] <- pbdb_taxonomy$accepted_name[match(study_interval_species_taxonomy$taxon_no[study_interval_species_taxonomy$taxon_no %in% unique_species_no],pbdb_taxonomy$taxon_no)];
study_interval_species_taxonomy <- unique(study_interval_species_taxonomy);

study_interval_species_taxonomy$nfinds <- rowMaxs(cbind((hist(match(study_interval_finds$accepted_name,study_interval_species_taxonomy$species),breaks=0:nrow(study_interval_species_taxonomy),plot=FALSE)$counts+hist(match(study_interval_finds$identified_name,study_interval_species_taxonomy$species),breaks=0:nrow(study_interval_species_taxonomy),plot=FALSE)$counts)));
study_interval_species_taxonomy$entered <- study_interval_species_taxonomy$ided_species %in% pbdb_taxonomy$taxon_name;
study_interval_species_taxonomy$entered[!study_interval_species_taxonomy$entered] <- study_interval_species_taxonomy$species[!study_interval_species_taxonomy$entered] %in% pbdb_taxonomy$taxon_name
st_species <- nrow(study_interval_species_taxonomy);
subgenus_assigned <- c();
for (sp in 1:st_species)	{
	if (study_interval_species_taxonomy$entered_rank[sp] %in% c("genus","subgenus"))	{
		if (is.subgenus(divido_genus_names_from_species_names(study_interval_species_taxonomy$species[sp])))	{
			assigned_genus <- divido_genus_names_from_species_names(study_interval_species_taxonomy$species[sp])
			epithet <- divido_species_epithets(study_interval_species_taxonomy$species[sp])
			genus1 <- strsplit(assigned_genus," ")[[1]][1]
			genus2 <- elevate_subgenus_to_genus(assigned_genus) 
			combo1 = paste(genus1,epithet);
			combo2 = paste(genus2,epithet);
#			if (length(which(study_interval_species_taxonomy %in% c(combo1,combo2),arr.ind=FALSE))>1)
				subgenus_assigned <- c(subgenus_assigned,sp);
			}
		}
	}

#study_interval_species_taxonomy$entered[study_interval_species_taxonomy$species %in% "Lepdioproetus (Dipharangus) xeo"]
#pbdb_taxonomy[pbdb_taxonomy$taxon_name %in% "Lepidoproetus (Dipharangus) xeo",]
for (sp in 1:nrow(study_interval_species_taxonomy))	{
#	study_interval_finds$collection_no[study_interval_finds$accepted_name %in% study_interval_species_taxonomy$species[sp]]
#	pbdb_finds$accepted_name[pbdb_finds$collection_no %in% 155682]
	study_interval_species_taxonomy$nfinds[sp] <- length(unique(c(pbdb_finds$collection_no[pbdb_finds$accepted_name %in% study_interval_species_taxonomy$species[sp]],pbdb_finds$collection_no[pbdb_finds$identified_name %in% study_interval_species_taxonomy$species[sp]],pbdb_finds$collection_no[pbdb_finds$accepted_name_orig %in% study_interval_species_taxonomy$species[sp]])));
	}
#pbdb_taxonomy[pbdb_taxonomy$taxon_name %in% "Otarion (Conoparia) clarimonda",]
unentered_study_interval_species_taxonomy <- study_interval_species_taxonomy[!study_interval_species_taxonomy$entered,];

write.csv(study_interval_species_taxonomy,"Study_Interval_Species_Entry_Summary.csv",row.names = FALSE);
writexl::write_xlsx("Study_Interval_Species_Entry_Summary.xlsx",x=study_interval_species_taxonomy)

unentered_study_interval_species_taxonomy <- unentered_study_interval_species_taxonomy[order(-unentered_study_interval_species_taxonomy$nfinds,unentered_study_interval_species_taxonomy$family),];
write.csv(unentered_study_interval_species_taxonomy,"Unentered_Species_Summaries.csv",row.names = FALSE);

# Get initial origination, extinction and sampling estimates ####
finest_chronostrat$bin_first <- 1:nrow(finest_chronostrat);
finest_chronostrat$bin_last <- 1:nrow(finest_chronostrat);

study_interval_sites$interval_lb <- pbapply::pbsapply(study_interval_sites$ma_lb,rebin_collection_with_time_scale,onset_or_end = "onset",finest_chronostrat);
study_interval_sites$interval_ub <- pbapply::pbsapply(study_interval_sites$ma_ub,rebin_collection_with_time_scale,onset_or_end = "end",finest_chronostrat);
study_interval_sites$bin_lb <- match(study_interval_sites$interval_lb,finest_chronostrat$interval);
study_interval_sites$bin_ub <- match(study_interval_sites$interval_ub,finest_chronostrat$interval);

study_species_sites_per_bin <- base::t(pbapply::pbsapply(study_interval_species_taxonomy$species,tally_collections_occupied_by_subinterval_sapply,all_finds=study_interval_finds,all_sites=study_interval_sites,hierarchical_chronostrat=finest_chronostrat));
study_interval_sites <- name_unnamed_rock_units(study_interval_sites,finest_chronostrat);
study_species_rocks_per_bin <- base::t(pbapply::pbsapply(study_interval_species_taxonomy$species,tally_rock_units_occupied_by_subinterval_sapply,all_finds=study_interval_finds,all_sites=study_interval_sites,hierarchical_chronostrat=finest_chronostrat));

ttl_sites_per_bin <- colSums(study_species_sites_per_bin);
ttl_rocks_per_bin <- colSums(study_species_rocks_per_bin);
nbins <- nrow(finest_chronostrat);
lognormal_sampling_intensity_rocks <- vector(length=nbins);
names(lognormal_sampling_intensity_rocks) <- finest_chronostrat$interval;
chao_sampling_rate_sites <- lognormal_sampling_rate_sites <- chao_sampling_rate_rocks <- lognormal_sampling_rate_rocks <- chao_sampling_intensity_sites <- lognormal_sampling_intensity_sites <- chao_sampling_intensity_rocks <- lognormal_sampling_intensity_rocks;
for (bn in 1:nbins)	{
	if (ttl_sites_per_bin[bn]>=10)	{
		finds <- sort(round(study_species_sites_per_bin[,bn][study_species_sites_per_bin[,bn]>=0.5],0),decreasing=TRUE);
		chao_sampling_intensity_sites[bn] <- length(finds)/chao2(finds,data_type = "occurrence");
		best_lgn_sites <- optimize_lognormal_occupancy(finds,ncoll=round(ttl_sites_per_bin[bn]));
		best_lognormal <- best_lgn_sites$scale*lognormal_distribution(mag=best_lgn_sites$mag_var,S=best_lgn_sites$richness)
#		best_lognormal_quants <- quantile(best_lognormal,prob=seq(1:7)/8)
#		x <- (0.25*(1-(1-best_lognormal_quants[names(best_lognormal_quants) %in% "87.5%"])^ttl_sites_per_bin[bn]))+(0.25*(1-(1-best_lognormal_quants[names(best_lognormal_quants) %in% "62.5%"])^ttl_sites_per_bin[bn]))+(0.25*(1-(1-best_lognormal_quants[names(best_lognormal_quants) %in% "37.5%"])^ttl_sites_per_bin[bn]))+(0.25*(1-(1-best_lognormal_quants[names(best_lognormal_quants) %in% "12.5%"])^ttl_sites_per_bin[bn]))
		lognormal_sampling_intensity_sites[bn] <- length(finds)/best_lgn_sites$richness;
		chao_sampling_rate_sites[bn] <- probability_to_Poisson_rate(chao_sampling_intensity_sites[bn])/finest_chronostrat$duration[bn];
		lognormal_sampling_intensity_sites[bn] <- probability_to_Poisson_rate(lognormal_sampling_intensity_sites[bn])/finest_chronostrat$duration[bn];
		}
	if (ttl_rocks_per_bin[bn]>5)	{
		finds <- sort(round(study_species_rocks_per_bin[,bn][study_species_rocks_per_bin[,bn]>=0.5],0),decreasing=TRUE);
		if (max(finds)==1)	finds[1] <- 2;
		chao_sampling_intensity_rocks[bn] <- length(finds)/chao2(finds,data_type = "occurrence");
		best_lgn_rocks <- optimize_lognormal_occupancy(finds,ncoll=round(ttl_rocks_per_bin[bn]));
		best_lognormal <- best_lgn_rocks$scale*lognormal_distribution(mag=best_lgn_rocks$mag_var,S=best_lgn_rocks$richness);
#		best_lognormal_quants <- quantile(best_lognormal,prob=seq(1:7)/8)
#		x <- (0.25*(1-(1-best_lognormal_quants[names(best_lognormal_quants) %in% "87.5%"])^ttl_rocks_per_bin[bn]))+(0.25*(1-(1-best_lognormal_quants[names(best_lognormal_quants) %in% "62.5%"])^ttl_rocks_per_bin[bn]))+(0.25*(1-(1-best_lognormal_quants[names(best_lognormal_quants) %in% "37.5%"])^ttl_rocks_per_bin[bn]))+(0.25*(1-(1-best_lognormal_quants[names(best_lognormal_quants) %in% "12.5%"])^ttl_rocks_per_bin[bn]))
		lognormal_sampling_intensity_rocks[bn] <- length(finds)/best_lgn_rocks$richness;
		chao_sampling_rate_rocks[bn] <- probability_to_Poisson_rate(chao_sampling_intensity_rocks[bn])/finest_chronostrat$duration[bn];
		lognormal_sampling_intensity_rocks[bn] <- probability_to_Poisson_rate(lognormal_sampling_intensity_rocks[bn])/finest_chronostrat$duration[bn];
		}
	}

rev_bayes_breaks_stages <- c(rev_bayes_breaks_stages,"Devonian");
stage_breaks <- c("Archean",rev_bayes_breaks_stages,"Holocene");
chao_sampling_rate_sites <- chao_sampling_rate_sites[chao_sampling_rate_sites>0]
chao_dates <- finest_chronostrat$ma_lb[match(names(chao_sampling_rate_sites),finest_chronostrat$interval)];
chao_spans <- finest_chronostrat$duration[match(names(chao_sampling_rate_sites),finest_chronostrat$interval)];
psis <- vector(length=1+length(rev_bayes_breaks_stages));
for (bn in 1:(length(stage_breaks)-1))	{
	aa <- time_scale$ma_lb[match(stage_breaks[bn],time_scale$interval)];
	zz <- time_scale$ma_lb[match(stage_breaks[bn+1],time_scale$interval)];
	psis[bn] <- sum(chao_sampling_rate_sites[chao_dates<=aa & chao_dates>zz]*chao_spans[chao_dates<=aa & chao_dates>zz])/sum(chao_spans[chao_dates<=aa & chao_dates>zz]);
	}
psis[length(psis):1];
paste(round(psis,2),collapse=",")

study_species_presence_per_bin <- study_species_sites_per_bin;
study_species_presence_per_bin[study_species_presence_per_bin>=0.5] <- 1;
study_species_presence_per_bin[study_species_presence_per_bin<0.5] <- 0;
for (sp in 1:nrow(study_species_presence_per_bin))	{
	if (sum(study_species_presence_per_bin[sp,])>1)	{
		aa <- min((1:nbins)[study_species_presence_per_bin[sp,]>0]);
		zz <- max((1:nbins)[study_species_presence_per_bin[sp,]>0]);
		study_species_presence_per_bin[sp,aa:zz] <- 1;
		}
	}

standing_richness_all_species <- colSums(study_species_presence_per_bin);
mxy <- ceiling(max(standing_richness_all_species)/5)*5;
ybreaks <- as.numeric(set_axis_breaks_new(mxy));
ybreaks <- c(10,5,1);
ordinate <- "    Species Richness";
Phanerozoic_Timescale_Plot_Hierarchical(-abs(onset),-abs(end),time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/20),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,strat_font_colors_minor,strat_font_colors_major,plot_title,ordinate,abscissa="Ma",yearbreaks=yearbreaks,hierarchical_partition=TRUE,xsize=xsize,ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,xlab_size=1.5,myr_size=myr_size,font=franky,ylab_size = 1.25,ylab_off = ylab_off);
#specified_axis(axe=2,max_val=mxy,min_val=mny,maj_break=ybreaks[1],med_break = ybreaks[2],min_break=ybreaks[3],font=franky,orient=2)
specified_axis(axe=2,max_val=mxy,min_val=mny,maj_break=ybreaks[1],med_break=ybreaks[2],min_break=ybreaks[3],font=franky,orient=2);
lines(-finest_chronostrat$ma_md[match(names(standing_richness_all_species[standing_richness_all_species>0]),finest_chronostrat$interval)],standing_richness_all_species[standing_richness_all_species>0],lwd=2.5)
lines(-finest_chronostrat$ma_md[match(names(standing_richness_all_species[standing_richness_all_species>0]),finest_chronostrat$interval)],standing_richness_all_species[standing_richness_all_species>0],lwd=2.0,col="orange3");

lines(-finest_chronostrat$ma_md[match(names(standing_richness[standing_richness>0]),finest_chronostrat$interval)],standing_richness[standing_richness>0],lwd=2.5)
lines(-finest_chronostrat$ma_md[match(names(standing_richness[standing_richness>0]),finest_chronostrat$interval)],standing_richness[standing_richness>0],lwd=2.0,col="orange2");

Sbt <- vector(length=nbins-1);
names(Sbt) <- paste(finest_chronostrat$interval[1:(nbins-1)],finest_chronostrat$interval[2:nbins],sep="/");
SbL <- SFt <- Sbt;
#sum(rowSums(study_species_presence_per_bin)>1)
for (sp in 1:nrow(study_species_presence_per_bin))	{
	if (sum(study_species_presence_per_bin[sp,])>1)	{
		aa <- min((1:nbins)[study_species_presence_per_bin[sp,]>0]);
		SbL[aa] <- SbL[aa]+1;
		zz <- max((1:nbins)[study_species_presence_per_bin[sp,]>0]);
		if (zz<=length(SFt))
			SFt[zz] <- SFt[zz]+1;
		if (zz>(aa+1))
			Sbt[(aa+1):(zz-1)] <- Sbt[(aa+1):(zz-1)]+1;
		}
	}

Sbt <- vector(length=nbins);
names(Sbt) <- finest_chronostrat$interval;
Sb <- St <- SFt <- SbL <- Sbt;
for (bn in 2:(nbins-1))	{
	bottoms_up <- study_species_presence_per_bin[rowSums(study_species_presence_per_bin[,(bn-1):bn])==2,(bn-1):(bn+1)]
	Sb[bn] <- nrow(bottoms_up);
	heads_down <- study_species_presence_per_bin[rowSums(study_species_presence_per_bin[,bn:(bn+1)])==2,(bn-1):(bn+1)]
	St[bn] <- nrow(heads_down);
	Sbt[bn] <- sum(rowSums(bottoms_up)==3);
	SFt[bn] <- St[bn]-Sbt[bn];
	SbL[bn] <- Sb[bn]-Sbt[bn];
	}

origination_rates <- extinction__rates <- vector(length=nbins);
names(origination_rates) <- names(extinction__rates) <- finest_chronostrat$interval;
for (bn in 2:(nbins-3))	{
	origination_rates[bn] <- log(St[bn]/max(1,Sbt[bn]));
	extinction__rates[bn] <- log(Sb[bn]/max(1,Sbt[bn]));
	}

origination_rates <- origination_rates[2:(nbins-3)];
extinction__rates <- extinction__rates[2:(nbins-3)];
hist(log(extinction__rates[is.finite(extinction__rates)]/origination_rates[is.finite(extinction__rates)]),breaks=seq(-1,1.5,by=0.25))


origination_rates <- log(St/Sbt);
origination_rates[St>0 & Sbt==0] <- log(St[St>0 & Sbt==0]);
exintction_rates <- log(Sb/Sbt);
exintction_rates[Sb>0 & Sbt==0] <- log(Sb[Sb>0 & Sbt==0]);
turnover_rates <- exintction_rates[!is.na(exintction_rates) & !is.na(origination_rates)]/origination_rates[!is.na(exintction_rates) & !is.na(origination_rates)]
log(turnover_rates)
hist(log(turnover_rates[turnover_rates>0]));
sum(exintction_rates[!is.na(exintction_rates) & !is.na(origination_rates)]>origination_rates[!is.na(exintction_rates) & !is.na(origination_rates)])
sum(exintction_rates[!is.na(exintction_rates) & !is.na(origination_rates)]<origination_rates[!is.na(exintction_rates) & !is.na(origination_rates)])

study_species_presence_per_bin[rowSums(study_species_presence_per_bin[,15:16])==2,14:16]
study_species_presence_per_bin[rowSums(study_species_presence_per_bin[,14:15])==2,14:16]

Sb <- Sbt+SbL;
St <- Sbt+SFt;
log(St/Sbt)

char_matrix <- char_data$Matrix;
nchars <- ncol(char_matrix);
scored <- vector(length=notu);
for (i in 1:notu)	scored[i] <- sum(!char_matrix[i,] %in% c(INAP,UNKNOWN));
sum(scored==0);
otu_finds <- vector(length=notu);
names(otu_finds) <- gsub(" ","_",accepted_otus);
pbdb_sites$ma_lb[pbdb_sites$collection_no==96250] <- 529.0;
pbdb_sites$ma_ub[pbdb_sites$collection_no==96250] <- 525.4;
onset <- 0; end <- MAXNO;
otu_boundaries <- otu_site_info <- list();
for (tx in 1:notu)	{
	if (accepted_otus[tx] %in% pbdb_taxonomy$taxon_name)	{
		taxon_no <- pbdb_taxonomy$accepted_no[match(accepted_otus[tx],pbdb_taxonomy$taxon_name)];
		taxon_finds <- pbdb_finds[pbdb_finds$accepted_no %in% taxon_no,];
		if (nrow(taxon_finds)==0)
			taxon_finds <- pbdb_finds[pbdb_finds$identified_name %in% taxon_no,];
		taxon_finds <- taxon_finds[!taxon_finds$identified_name %in% accepted_otus[!accepted_otus %in% accepted_otus[tx]],]
		if (nrow(taxon_finds)==0 & pbdb_taxonomy$is_extant[pbdb_taxonomy$taxon_no==taxon_no] %in% "extant")	{
			otu_finds[tx] <- -1;
			} else	{
			taxon_sites <- pbdb_sites[pbdb_sites$collection_no %in% taxon_finds$collection_no,];
			if (onset < max(taxon_sites$ma_lb))	onset <- max(taxon_sites$ma_lb);
			if (end > min(taxon_sites$ma_ub))	end <- min(taxon_sites$ma_ub);
			otu_finds[tx] <- nrow(taxon_sites);
			taxon_sites <- taxon_sites[order(-taxon_sites$ma_lb,taxon_sites$ma_ub),];
			boundaries_unq <- unique(sort(c(taxon_sites$ma_lb,taxon_sites$ma_ub),decreasing=TRUE))
			nbins <- length(boundaries_unq)-1;
			taxon_bin_info <- data.frame(ma_lb=boundaries_unq[1:nbins],
										 ma_ub=boundaries_unq[2:length(boundaries_unq)],
										 nfinds_min=rep(0,nbins),nfinds_max=rep(0,nbins));
			for (b in 1:nbins)	{
				taxon_bin_info$nfinds_min[b] <- sum(taxon_sites$ma_lb==boundaries_unq[b] & taxon_sites$ma_ub==boundaries_unq[b+1]);
				overlaps <- vector(length=nrow(taxon_sites));
				for (oc in 1:nrow(taxon_sites))
					overlaps[oc] <- do_two_ranges_overlap(lb_a=boundaries_unq[b],ub_a=boundaries_unq[b+1],
														  lb_b=taxon_sites$ma_lb[oc],ub_b=taxon_sites$ma_ub[oc])
				#cbind(taxon_sites[,c("collection_no","rock_unit_senior","ma_lb","ma_ub")],overlaps)
				taxon_bin_info$nfinds_max[b] <- sum(overlaps);
				}
			otu_site_info <- rlist::list.append(otu_site_info,taxon_sites[,c("collection_no","rock_unit_senior","ma_lb","ma_ub")]);
			otu_boundaries <- rlist::list.append(otu_boundaries,taxon_bin_info);
			}
		}
	}
names(otu_boundaries) <- names(otu_site_info) <- gsub(" ","_",accepted_otus);
#pbdb_sites[pbdb_sites$collection_no %in% c(208578,208579),c("ma_lb","ma_ub")]

# plot boundary crossers & density of finds within bins ####
minor_rank <- "Stage";
major_rank <- "Epoch";
minor_scale <- time_scale[time_scale$chronostratigraphic_rank %in% minor_rank,]
minor_scale <- minor_scale[minor_scale$ma_ub<onset & minor_scale$ma_lb>end & minor_scale$interval_sr %in% "" & minor_scale$scale %in% "International",];
major_scale <- time_scale[time_scale$chronostratigraphic_rank %in% major_rank,]
major_scale <- major_scale[major_scale$ma_ub<onset & major_scale$ma_lb>end & major_scale$interval_sr %in% "" & major_scale$scale %in% "International",];
stage_slices <- stage_slices[stage_slices$ma_ub<onset & stage_slices$ma_lb>end & stage_slices$interval_sr %in% "",];
stage_slices <- stage_slices[order(-stage_slices$ma_lb),];

time_scale_minor <- minor_scale[abs(minor_scale$ma_ub)<max(abs(minor_scale$ma_lb)),];
#time_scale_minor$ma_lb[time_scale_minor$ma_lb>onset] <- onset;
time_scale_major <- major_scale[abs(major_scale$ma_ub)<max(abs(minor_scale$ma_lb)),];
#time_scale_major$ma_lb[time_scale_major$ma_lb>onset] <- onset;

# Make sure that ages on time scale are negatives
minor_scale$ma_lb <- -abs(minor_scale$ma_lb);
minor_scale$ma_ub <- -abs(minor_scale$ma_ub);
major_scale$ma_lb <- -abs(major_scale$ma_lb);
major_scale$ma_ub <- -abs(major_scale$ma_ub);
time_scale_minor$ma_lb <- -abs(time_scale_minor$ma_lb);
time_scale_minor$ma_ub <- -abs(time_scale_minor$ma_ub);
time_scale_major$ma_lb <- -abs(time_scale_major$ma_lb);
time_scale_major$ma_ub <- -abs(time_scale_major$ma_ub);

# Make sure that time scale is correct order, with oldest first
minor_scale <- minor_scale[order(minor_scale$ma_lb),];
time_scale_minor <- time_scale_minor[order(time_scale_minor$ma_lb),];
time_scale_major <- time_scale_major[order(time_scale_major$ma_lb),];

i <- 1;
#for (i in 2:nrow(time_scale_minor))	{
while (i < nrow(time_scale_minor))	{
	i <- i+1;
	if (!time_scale_minor$ma_lb[i] %in% minor_scale$ma_lb)	{
		time_scale_minor$ma_ub[i-1] <- time_scale_minor$ma_ub[i];
		time_scale_minor$interval[i-1] <- time_scale_minor$st[i-1] <- "";
		time_scale_minor <- time_scale_minor[(1:nrow(time_scale_minor))[!(1:nrow(time_scale_minor)) %in% i],];
		i <- i-1;
		}
	}
minor_scale$span <- abs(minor_scale$ma_lb-minor_scale$ma_ub);
time_scale_minor$span <- abs(time_scale_minor$ma_lb-time_scale_minor$ma_ub);
time_scale_major$span <- abs(time_scale_major$ma_lb-time_scale_major$ma_ub);
time_scale_minor$prop_span <- time_scale_minor$span/(max(abs(time_scale_minor$ma_lb))-min(abs(time_scale_minor$ma_ub)));
time_scale_major$prop_span <- time_scale_major$span/(max(abs(time_scale_major$ma_lb))-min(abs(time_scale_major$ma_ub)));

# Add names/symbols for plotting
strat_names_minor <- time_scale_minor$strat_names <- time_scale_minor$interval;
for (i in 1:nrow(time_scale_minor))	{
	if (time_scale_minor$prop_span[i]<0.04)	{
		strat_names_minor[i] <- paste(strsplit(strat_names_minor[i],"")[[1]][1:3],collapse="")
		} else if (time_scale_minor$prop_span[i]<0.05)	{
		strat_names_minor[i] <- paste(strsplit(strat_names_minor[i],"")[[1]][1:4],collapse="")
		}
	}

strat_names_major <- time_scale_major$interval;
for (i in 1:nrow(time_scale_major))	{
	if (time_scale_major$prop_span[i]<0.04)	{
		strat_names_major[i] <- paste(strsplit(strat_names_major[i],"")[[1]][1:3],collapse="")
		} else if (time_scale_major$prop_span[i]<0.05)	{
		strat_names_major[i] <- paste(strsplit(strat_names_major[i],"")[[1]][1:4],collapse="")
		}
	}

# Get the colors for time units
strat_colors_minor <- time_scale_minor$color;
strat_colors_major <- time_scale_major$color;
strat_font_colors_minor <- time_scale_minor$font_color;
strat_font_colors_major <- time_scale_major$font_color;
names(strat_font_colors_minor) <- names(strat_colors_minor) <- time_scale_minor$interval;
names(strat_font_colors_major) <- names(strat_colors_major) <- time_scale_major$interval;
# Set the oldest and youngest intervals by names on whatever chronostratigraphic scale you used in the analysis
oldest_interval <- time_scale_minor$interval[1];
youngest_interval <- time_scale_minor$interval[nrow(time_scale_minor)];
# get the time scale that will be plotted
time_scale_to_plot_minor <- unique(c(time_scale_minor$ma_lb,time_scale_minor$ma_ub));
time_scale_to_plot_minor <- time_scale_to_plot_minor[time_scale_to_plot_minor!=-2.6];
time_scale_to_plot_major <- unique(c(-abs(time_scale_major$ma_lb),-abs(time_scale_major$ma_ub)));
time_scale_to_plot_major[1] <- time_scale_to_plot_minor[1];

# Now, set the onset and end of the x-axis
onset <- min(time_scale_to_plot_minor);
end <- max(time_scale_to_plot_minor);
# Finally, set up breaks for x-axis
#yearbreaks <- c(5,25,50);					# set breaks for x-axis (minor, medium & major)
yearbreaks <- sort(as.numeric(set_axis_breaks_new(max_no=end-onset)));
# now, set up the y-axis: this will reflect your data

use_strat_labels <- T;						# if T, then strat_names_minor will be plotted on X-axis inside boxes
alt_back <- F;								# if T, then the background will alternat shades between major intervals
plot_title <- "";							# Name of the plot; enter "" for nothing
hues <- "T";								# If T, then IGN stratigraphic colors will be used
colored <- "base";							# Where IGN stratigraphic colors should go
xsize <- 6;
ysize <- xsize*(4.285714285/6);
#ysize <- xsize*(4.285714285/6);

myr_size <- 1;
ordinate <- "";								# Label of Y-axis
#mxy <- ceiling(max(sites_per_stage_slice_marine)/25)*25;
mxy <- 2*notu;
mny <- 0;									# set maximum y value
ybreaks <- as.numeric(set_axis_breaks_new(mxy));
plot_title <- "";
names(strat_colors_major) <- strat_names_major;
strat_label_size_minor <- 0.6;
strat_label_size_major <- 0.9;
strat_names_minor[strat_names_minor %in% c("Guzhangian","Paibian")] <- c("Guzh.","Paib.")
strat_names_major[strat_names_major %in% c("Furongian")] <- c("Fur.")
end_extra <- abs(end-onset)/4;
#Phanerozoic_Timescale_Plot_Hierarchical(onset,end=end+end_extra,time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/100),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,plot_title,ordinate=ordinate,abscissa=abscissa,yearbreaks,xsize=xsize,ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,xlab_size=1.5,myr_size=myr_size,font=franky,ylab_size = 1.25,ylab_off = 3.5);
Phanerozoic_Timescale_Plot_Hierarchical(onset,end=end+end_extra,time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/7.5),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,strat_font_colors_minor,strat_font_colors_major,plot_title,ordinate,abscissa="Ma",yearbreaks,xsize=xsize*(1+end_extra/abs(end-onset)),ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,hierarchical_partition = TRUE,xlab_size=1.25,myr_size=myr_size,font=franky,ylab_size=1,timescale_pos=-1,xlab_off = 1.5,ylab_off = 3.5);

xspan <- end-onset;
myr_xsize <- xsize/xspan;
yspan <- mxy;
for (tx in 1:notu)	{
	this_base <- 2*(tx-2);
	this_taxon_sites <- otu_site_info[[tx]];
	these_slices <- stage_slices[stage_slices$ma_lb>min(this_taxon_sites$ma_ub) & stage_slices$ma_ub<max(this_taxon_sites$ma_lb),];
	taxon_boundaries <- data.frame(ma_lb=these_slices$ma_lb,ma_ub=these_slices$ma_ub,
										nfinds_min=rep(0,nrow(these_slices)),
										nfinds_max=rep(0,nrow(these_slices)));
	for (bn in 1:nrow(taxon_boundaries))	{
		taxon_boundaries$nfinds_min[bn] <- sum(this_taxon_sites$ma_lb<=taxon_boundaries$ma_lb[bn] & this_taxon_sites$ma_ub>=taxon_boundaries$ma_ub[bn])
		taxon_boundaries$nfinds_max[bn] <- sum(this_taxon_sites$ma_lb>taxon_boundaries$ma_ub[bn] & this_taxon_sites$ma_ub<taxon_boundaries$ma_lb[bn])
		}
#	taxon_boundaries <- otu_boundaries[[tx]];
	taxon_boundaries$ma_lb <- -abs(taxon_boundaries$ma_lb);
	taxon_boundaries$ma_ub <- -abs(taxon_boundaries$ma_ub);
	rect(min(taxon_boundaries$ma_lb),this_base,max(taxon_boundaries$ma_ub),1+this_base,col=makeTransparent("orange3",100),border=makeTransparent("orange3",100),lwd=0)
#	taxon_boundaries$ma_lb <- -abs(taxon_boundaries$ma_lb);
#	taxon_boundaries$ma_ub <- -abs(taxon_boundaries$ma_ub);
	rect(min(taxon_boundaries$ma_lb),this_base,max(taxon_boundaries$ma_ub),1+this_base,col=NULL,border="black",lwd=1)
	text(max(taxon_boundaries$ma_ub)-xspan/75,this_base+yspan/60,accepted_otus[tx],family=franky_italic,pos=4,cex=10/12)
	misfits <- bn <- 0;
	while (bn < nrow(taxon_boundaries))	{
		bn <- bn+1;
#	for (bn in 1:nrow(taxon_boundaries))	{
		mid <- (taxon_boundaries$ma_lb[bn]+taxon_boundaries$ma_ub[bn])/2;
		font_col <- "white";
		if (bn==1 | bn==nrow(taxon_boundaries))	{
			font_col <- "black";
			if (bn==2 & nrow(taxon_boundaries)==2)	{
				segments(taxon_boundaries$ma_lb[bn],this_base,taxon_boundaries$ma_lb[bn],this_base+1,lwd=2);
				segments(taxon_boundaries$ma_lb[bn],this_base,taxon_boundaries$ma_lb[bn],this_base+1,lwd=1,col="orange3");
				}
			} else	{
			rect(taxon_boundaries$ma_lb[bn],this_base,taxon_boundaries$ma_ub[bn],1+this_base,col="orange3",border="black",lwd=1);
			}
		if (taxon_boundaries$nfinds_min[bn]==taxon_boundaries$nfinds_max[bn])	{
		#	text(mid,this_base+0.5,taxon_boundaries$nfinds_min[bn],family=franky,col=font_col,cex=9/12);
			find_text <- as.character(taxon_boundaries$nfinds_min[bn]);
			} else	{
			if (log10(max(taxon_boundaries$nfinds_min[bn],1))<log10(taxon_boundaries$nfinds_max[bn]))	{
				add_this <- rep(" ",floor(log10(taxon_boundaries$nfinds_max[bn]))-max(0,log10(taxon_boundaries$nfinds_min[bn])))
				} else	{
				add_this <- "";
				}
			find_text <- paste(add_this,taxon_boundaries$nfinds_min[bn],"-",taxon_boundaries$nfinds_max[bn],sep="");
#			text(mid,this_base+0.5,poss_finds,family=franky,col=font_col,cex=9/12);
			}
		bin_span <- abs(taxon_boundaries$ma_lb[bn]-taxon_boundaries$ma_ub[bn]);
		bin_span_size <- bin_span*myr_xsize;
		find_txt_l <- length(strsplit(find_text,"")[[1]]);
		find_txt_size <- 0.5*find_txt_l*myr_xsize;
		if (find_txt_size<bin_span_size)	{
			text(mid,this_base+yspan/48,find_text,family=franky,col=font_col,cex=9/12);
			} else	{
			misfits <- misfits+1;
			if (misfits%%2==1)	{
				text(mid,this_base-yspan/60,find_text,family=franky,col="black",cex=6/12);
				} else	{
				text(mid,1+this_base+yspan/60,find_text,family=franky,col="black",cex=6/12);
				}
			}
		}
	}

# stuff ####
paste(pbdb_sites$collection_no[pbdb_sites$formation=="Urumaco" & pbdb_sites$member==""],collapse=",")
rock_unit_data$rock_to_zone_database$zone[rock_unit_data$rock_to_zone_database$rock_no_sr==32421]
# c'mon let's do it again... ####
stage_slices <- stage_slices[order(-stage_slices$ma_lb),];
time_scale_minor <- minor_scale[abs(minor_scale$ma_ub)<max(abs(stage_slices$ma_lb)),];
time_scale_major <- major_scale[abs(major_scale$ma_ub)<max(abs(stage_slices$ma_lb)),];

# Make sure that ages on time scale are negatives
stage_slices$ma_lb <- -abs(stage_slices$ma_lb);
stage_slices$ma_ub <- -abs(stage_slices$ma_ub);
time_scale_minor$ma_lb <- -abs(time_scale_minor$ma_lb);
time_scale_minor$ma_ub <- -abs(time_scale_minor$ma_ub);
time_scale_major$ma_lb <- -abs(time_scale_major$ma_lb);
time_scale_major$ma_ub <- -abs(time_scale_major$ma_ub);

# Make sure that time scale is correct order, with oldest first
stage_slices <- stage_slices[order(stage_slices$ma_lb),];
time_scale_minor <- time_scale_minor[order(time_scale_minor$ma_lb),];
time_scale_major <- time_scale_major[order(time_scale_major$ma_lb),];

i <- 1;
#for (i in 2:nrow(time_scale_minor))	{
while (i < nrow(time_scale_minor))	{
	i <- i+1;
	if (!time_scale_minor$ma_lb[i] %in% stage_slices$ma_lb)	{
		time_scale_minor$ma_ub[i-1] <- time_scale_minor$ma_ub[i];
		time_scale_minor$interval[i-1] <- time_scale_minor$st[i-1] <- "";
		time_scale_minor <- time_scale_minor[(1:nrow(time_scale_minor))[!(1:nrow(time_scale_minor)) %in% i],];
		i <- i-1;
		}
	}
stage_slices$span <- abs(stage_slices$ma_lb-stage_slices$ma_ub);
time_scale_minor$span <- abs(time_scale_minor$ma_lb-time_scale_minor$ma_ub);
time_scale_major$span <- abs(time_scale_major$ma_lb-time_scale_major$ma_ub);
time_scale_minor$prop_span <- time_scale_minor$span/(max(abs(time_scale_minor$ma_lb))-min(abs(time_scale_minor$ma_ub)));
time_scale_major$prop_span <- time_scale_major$span/(max(abs(time_scale_major$ma_lb))-min(abs(time_scale_major$ma_ub)));

# Add names/symbols for plotting
strat_names_minor <- time_scale_minor$strat_names <- time_scale_minor$st;
strat_names_major <- time_scale_major$interval;
for (i in 1:nrow(time_scale_major))	{
	if (time_scale_major$prop_span[i]<0.04)	{
		strat_names_major[i] <- paste(strsplit(strat_names_major[i],"")[[1]][1:3],collapse="")
		} else if (time_scale_major$prop_span[i]<0.05)	{
		strat_names_major[i] <- paste(strsplit(strat_names_major[i],"")[[1]][1:4],collapse="")
		}
	}

# Get the colors for time units
strat_colors_minor <- time_scale_minor$color;
strat_colors_major <- time_scale_major$color;
names(strat_colors_minor) <- time_scale_minor$interval;
names(strat_colors_major) <- time_scale_major$interval;
# Set the oldest and youngest intervals by names on whatever chronostratigraphic scale you used in the analysis
oldest_interval <- time_scale_minor$interval[1];
youngest_interval <- time_scale_minor$interval[nrow(time_scale_minor)];
# get the time scale that will be plotted
time_scale_to_plot_minor <- unique(c(time_scale_minor$ma_lb,time_scale_minor$ma_ub));
time_scale_to_plot_minor <- time_scale_to_plot_minor[time_scale_to_plot_minor!=-2.6];
time_scale_to_plot_major <- unique(c(-abs(time_scale_major$ma_lb),-abs(time_scale_major$ma_ub)));
time_scale_to_plot_major[1] <- time_scale_to_plot_minor[1];

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
xsize <- 6;
ysize <- xsize*(4.285714285/6);

myr_size <- 1;
strat_label_size_minor <- 1;
strat_label_size_major <- 0.9;

ordinate <- "  PBDB Sites per Stage Slice";								# Label of Y-axis
#mxy <- ceiling(max(sites_per_stage_slice_marine)/25)*25;
mny <- 0;									# set maximum y value
ybreaks <- as.numeric(set_axis_breaks_new(mxy));
plot_title <- "";
names(strat_colors_major) <- strat_names_major;
Phanerozoic_Timescale_Plot_Hierarchical(onset,end,time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/100),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,plot_title,ordinate=ordinate,abscissa="Ma",yearbreaks,xsize=xsize,ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,xlab_size=1.5,myr_size=myr_size,font=franky,ylab_size = 1.25,ylab_off = 3.5);
specified_axis(axe=2,max_val=mxy,min_val=0,maj_break=ybreaks[1],med_break=ybreaks[2],min_break=ybreaks[3],
			   orient=2,font=franky);

group_labels <- c("Marine Vertebates","Marine Tetrapods","Cetaceans");
group_shades <- c("gray75","gray50","gray25");
for (i in 1:length(sites_per_stage_slice_marine))	{
	rect(stage_slices$ma_lb[i],0,stage_slices$ma_ub[i],sites_per_stage_slice_marine[i],col=group_shades[1]);
	if (sites_per_stage_slice_controls[i]>0) rect(stage_slices$ma_lb[i],0,stage_slices$ma_ub[i],sites_per_stage_slice_controls[i],col=group_shades[2]);
	if (sites_per_stage_slice_cetacea[i]>0) rect(stage_slices$ma_lb[i],0,stage_slices$ma_ub[i],sites_per_stage_slice_cetacea[i],col=group_shades[3]);
	}

iy <- mxy;
dy <- 0.05*mxy;
dx <- abs(onset-end)/20;
for (i in 1:length(group_labels))	{
	rect(onset,iy,onset+dx,iy-dy,col=group_shades[i]);
	text(onset+dx,iy-(dy/2),group_labels[i],pos=4,family=franky);
	iy <- iy-1.5*dy;
	}

# illustrate range through taxa ####
#rowSums(otu_info[boundary_crossers,c("fa_finds","la_finds","rt_finds")])>otu_info$nfinds[boundary_crossers]
ttl_edge_finds <- sum(otu_info$fa_finds[otu_info$boundary_crosser])+sum(otu_info$la_finds[otu_info$boundary_crosser])+sum(otu_info$rt_finds[otu_info$boundary_crosser])
ttl_rthr_finds <- sum(otu_info$rt_finds[otu_info$boundary_crosser]);
ttl_bcrs_finds <- sum(otu_info$nfinds[otu_info$boundary_crosser]);
otu_info_bcers <- otu_info[otu_info$boundary_crosser,];
otu_info_bcers <- otu_info_bcers[order(-otu_info_bcers$fa_lb),];
otu_info_rthru <- otu_info_bcers[otu_info_bcers$rt_finds>0,];

onset <- time_scale_minor$ma_lb[time_scale_minor$interval=="Lutetian"]
time_scale_minor <- time_scale_minor[time_scale_minor$ma_ub>=-max(abs(otu_info_rthru$fa_lb)),];
time_scale_major <- time_scale_major[time_scale_major$ma_ub>=-max(abs(otu_info_rthru$fa_lb)),];
time_scale_major$ma_lb[1] <- time_scale_minor$ma_lb[1];

# Add names/symbols for plotting
strat_names_minor <- time_scale_minor$strat_names <- time_scale_minor$st;
strat_names_major <- time_scale_major$interval;
for (i in 1:nrow(time_scale_major))	{
	if (time_scale_major$prop_span[i]<0.04)	{
		strat_names_major[i] <- paste(strsplit(strat_names_major[i],"")[[1]][1:3],collapse="")
		} else if (time_scale_major$prop_span[i]<0.05)	{
		strat_names_major[i] <- paste(strsplit(strat_names_major[i],"")[[1]][1:4],collapse="")
		}
	}

# Get the colors for time units
strat_colors_minor <- time_scale_minor$color;
strat_colors_major <- time_scale_major$color;
names(strat_colors_minor) <- time_scale_minor$interval;
names(strat_colors_major) <- time_scale_major$interval;
# Set the oldest and youngest intervals by names on whatever chronostratigraphic scale you used in the analysis
oldest_interval <- time_scale_minor$interval[1];
youngest_interval <- time_scale_minor$interval[nrow(time_scale_minor)];
# get the time scale that will be plotted
time_scale_to_plot_minor <- unique(c(time_scale_minor$ma_lb,time_scale_minor$ma_ub));
time_scale_to_plot_minor <- time_scale_to_plot_minor[time_scale_to_plot_minor!=-2.6];
time_scale_to_plot_major <- unique(c(-abs(time_scale_major$ma_lb),-abs(time_scale_major$ma_ub)));
time_scale_to_plot_major[1] <- time_scale_to_plot_minor[1];

# Now, set the onset and end of the x-axis
onset <- min(time_scale_to_plot_minor);
end <- max(time_scale_to_plot_minor);
# Finally, set up breaks for x-axis
#yearbreaks <- c(5,25,50);					# set breaks for x-axis (minor, medium & major)
mxy <- nrow()
yearbreaks <- sort(as.numeric(set_axis_breaks(max_no=end,min_no=onset)));
# now, set up the y-axis: this will reflect your data

use_strat_labels <- T;						# if T, then strat_names_minor will be plotted on X-axis inside boxes
alt_back <- F;								# if T, then the background will alternat shades between major intervals
plot_title <- "";							# Name of the plot; enter "" for nothing
hues <- "T";								# If T, then IGN stratigraphic colors will be used
colored <- "base";							# Where IGN stratigraphic colors should go

myr_size <- 1;
strat_label_size_minor <- 1;
strat_label_size_major <- 0.9;

ordinate <- "";								# Label of Y-axis
mxy <- nrow(otu_info_rthru);
mny <- 1;									# set maximum y value
end_extra <- abs(end-onset)/4;
abscissa <- "Ma            ";
xsize <- 6;
ysize <- 5;
Phanerozoic_Timescale_Plot_Hierarchical(onset,end=end+end_extra,time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/100),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,plot_title,ordinate=ordinate,abscissa=abscissa,yearbreaks,xsize=xsize,ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,xlab_size=1.5,myr_size=myr_size,font=franky,ylab_size = 1.25,ylab_off = 3.5);
for (i in 1:nrow(otu_info_rthru))	{
	j <- match(otu_info_rthru$taxon[i],rownames(sites_per_stage_slice_otus));
	rect(-otu_info_rthru$fa_lb[i],i,-otu_info_rthru$la_ub[i],i+0.75,col=makeTransparent("forestgreen"));
	rect(-otu_info_rthru$fa_ub[i],i,-otu_info_rthru$la_lb[i],i+0.75,col="forestgreen");
	text(-otu_info_rthru$la_ub[i],i+1/3,otu_info_rthru$taxon[i],pos=4,family=franky,cex=0.75)
	text(-sum(otu_info_rthru[i,c("fa_lb","fa_ub")])/2,i+1/3,otu_info_rthru$fa_finds[i],family=franky,cex=0.5);
	text(-sum(otu_info_rthru[i,c("la_lb","la_ub")])/2,i+1/3,otu_info_rthru$la_finds[i],family=franky,cex=0.5);
#	text(-sum(otu_info_rthru[i,c("la_lb","fa_ub")])/2,i+1/3,otu_info_rthru$rt_finds[i],family=franky,cex=0.5,col="white");
	sites_per_stage_slice_otu <- sites_per_stage_slice_otus[j,];
	bins <- min((1:nintervals)[sites_per_stage_slice_otu>0]):max((1:nintervals)[sites_per_stage_slice_otu>0]);
	nbins < length(bins);
	for (nb in 2:(nbins-1))	{
		ss <- match(names(sites_per_stage_slice_otu)[bins[nb]],stage_slices$interval);
		if (nb>2)	segments(stage_slices$ma_lb[ss],i,stage_slices$ma_lb[ss],i+0.75,col="gray90");
		xx <- -abs(mean(as.numeric(stage_slices[ss,c("ma_lb","ma_ub")])));
		nf <- round(sites_per_stage_slice_otu[bins[nb]],0);
		text(xx,i+1/3,nf,family=franky,cex=0.5,col="white");
		}
	}

xsize <- 6;
i <- match("Schizodelphis sulcatus",otu_info$taxon);
nintervals <- ncol(sites_per_stage_slice_otus);
xx <- abs(stage_slices$ma_lb[min(match(colnames(sites_per_stage_slice_otus)[sites_per_stage_slice_otus[i,]>0],stage_slices$interval))]);
time_scale_minor <- time_scale_minor[abs(time_scale_minor$ma_ub)<=xx,];
time_scale_major <- time_scale_major[abs(time_scale_major$ma_ub)<=xx,];
time_scale_to_plot_minor <- unique(c(-abs(time_scale_minor$ma_lb),-abs(time_scale_minor$ma_ub)));
time_scale_to_plot_major <- unique(c(-abs(time_scale_major$ma_lb),-abs(time_scale_major$ma_ub)));
time_scale_to_plot_major[1] <- time_scale_to_plot_minor[1];
time_scale_to_plot_major[length(time_scale_to_plot_major)] <- 0.0;
strat_names_minor <- time_scale_minor$st;
strat_names_major <- time_scale_major$st;
strat_names_major[strat_names_major %in% "Oli"] <- "Oligocene";
strat_names_major[strat_names_major %in% "Mio"] <- "Miocene";
strat_colors_minor <- time_scale_minor$color;
strat_colors_major <- time_scale_major$color;
onset <- -max(abs(time_scale_to_plot_minor));
end <- -min(abs(time_scale_to_plot_minor));
strat_label_size_minor <- 0.75;
xsize <- 4.5;
ysize <- 1;
mxy <- 1;
mny <- 0;
abscissa <- "Ma";
Phanerozoic_Timescale_Plot_Hierarchical(onset,end=end,time_scale_to_plot_minor,time_scale_to_plot_major,mxy=mxy,mny=(mny-(mxy-mny)/100),use_strat_labels,strat_names_minor,strat_names_major,strat_colors_minor,strat_colors_major,plot_title,ordinate=ordinate,abscissa=abscissa,yearbreaks,xsize=xsize,ysize=ysize,hues=hues,colored=colored,alt_back=alt_back,strat_label_size_minor=strat_label_size_minor,strat_label_size_major=strat_label_size_major,xlab_size=1.5,myr_size=myr_size,font=franky,ylab_size = 1.25,ylab_off = 3.5);

otu_info$fa_lb[i] <- 21.3;
rect(-abs(otu_info$fa_lb[i]),0.5,-abs(otu_info$la_ub[i]),0.75,col=makeTransparent("forestgreen"));
rect(-abs(otu_info$fa_ub[i]),0.5,-abs(otu_info$la_lb[i]),0.75,col="forestgreen");
text(-otu_info$la_ub[i],5/8,otu_info$taxon[i],pos=4,family=franky,cex=0.9);
spc_range <- min((1:nintervals)[sites_per_stage_slice_otus[i,]>0]):max((1:nintervals)[sites_per_stage_slice_otus[i,]>0]);
spc_finds <- round(sites_per_stage_slice_otus[i,spc_range],0);
for (sr in 1:length(spc_range))	{
	if (sr==1)	{
		aa <- -abs(otu_info$fa_lb[i]);
		zz <- -abs(otu_info$fa_ub[i]);
		font_col <- "black";
		} else if (sr==length(spc_range))	{
		aa <- -abs(otu_info$la_lb[i]);
		zz <- -abs(otu_info$la_ub[i]);
		font_col <- "black";
		} else	{
		aa <- -abs(stage_slices$ma_lb[match(names(spc_finds)[sr],stage_slices$interval)]);
		zz <- -abs(stage_slices$ma_ub[match(names(spc_finds)[sr],stage_slices$interval)]);
		font_col <- "white";
		}
	text(mean(c(aa,zz)),5/8,spc_finds[sr],family=franky,col=font_col);
	if (sr>2 & sr<length(spc_range))
		segments(-abs(stage_slices$ma_lb[match(names(spc_finds)[sr],stage_slices$interval)]),0.51,
				 -abs(stage_slices$ma_lb[match(names(spc_finds)[sr],stage_slices$interval)]),0.74,col="white");
	}

spc_sites <- relv_sites[relv_sites$collection_no %in% pbdb_finds$collection_no[pbdb_finds$accepted_name %in% "Schizodelphis sulcatus"],]
spc_sites <- spc_sites[order(spc_sites$ma_lb,spc_sites$ma_ub),];

slice_scale <- stage_slices[stage_slices$interval %in% names(sites_per_stage_slice_marine),]
slice_scale <- slice_scale[order(-slice_scale$ma_lb),];
otu_info$fa_bin_lb <- match(sapply(otu_info$fa_lb,rebin_collection_with_time_scale,"onset",slice_scale),slice_scale$interval);
otu_info$fa_bin_ub <- match(sapply(otu_info$fa_ub,rebin_collection_with_time_scale,"onset",slice_scale),slice_scale$interval);
otu_info$la_bin_lb <- match(sapply(otu_info$la_lb,rebin_collection_with_time_scale,"onset",slice_scale),slice_scale$interval);
otu_info$la_bin_ub <- match(sapply(otu_info$la_ub,rebin_collection_with_time_scale,"onset",slice_scale),slice_scale$interval);
otu_info$poss_bin_span <- 1+otu_info$la_bin_ub-(otu_info$fa_bin_lb);
otu_info$poss_bin_span[otu_info$nfinds==0] <- 0;
otu_info$rt_bin_span <- otu_info$la_bin_lb-(otu_info$fa_bin_ub+1);
otu_info$rt_bin_span[otu_info$rt_bin_span<0] <- 0;
sum(otu_info$rt_bin_span)
bt <- sum(otu_info$rt_bin_span);
bL <- sum(otu_info$fa_bin_ub<otu_info$la_bin_lb & !otu_info$extant);
Ft <- sum(otu_info$fa_bin_ub<otu_info$la_bin_lb);
FL <- sum(otu_info$fa_bin_lb==otu_info$la_bin_lb & otu_info$fa_bin_ub==otu_info$la_bin_ub);
mxy <- ceiling(FL/100)*100;
specify_basic_plot(mxx=4,mnx=0,mxy=mxy,mny=0,
				   abscissa = "",ordinate="Lineage Segments",
				   xsize=3,ysize=3,font=franky,
				   ylab_off = 3,cexlab = 1.25);
specified_axis_w_labels(axe=1,max_val=4,min_val=0,maj_break=1,med_break=0,min_break=0,
						axis_labels=c("FL","bL","Ft","bt"),label_pos = "mid",font=franky);
ybreaks <- as.numeric(set_axis_breaks_new(mxy));
specified_axis(axe=2,max_val=mxy,min_val=0,maj_break=ybreaks[1],med_break=ybreaks[2],min_break=ybreaks[3],orient=2,font=franky);
rect(0,0,1,FL,col="gray90");
rect(1,0,2,bL,col="gray75");
rect(2,0,3,Ft,col="gray75");
rect(3,0,4,bt,col="gray50");

# boundaries ####
for (sp in 1:notu)	{
	while (otu_info$nfinds[sp]==0 && sp<=notu)	sp <- sp+1;
	if (sp>notu)	break;
	if (otu_info$fa_ub[sp]>otu_info$la_lb[sp])	{
		otu_info$rt_finds[sp]
		sites_per_stage_slice_otus[sp,]
		stage_slices$interval
		}
	}

sulcatus_finds <- pbdb_finds[pbdb_finds$accepted_name %in% "Schizodelphis sulcatus",];
sulcatus_finds <- sulcatus_finds[!sulcatus_finds$flags %in% uncertain_species,];
sulcatus_sites <- pbdb_sites[pbdb_sites$collection_no %in% sulcatus_finds$collection_no,];
sulcatus_sites <- sulcatus_sites[order(-sulcatus_sites$ma_lb),]

# RODs ####
colSums(sites_per_stage_slice_otus)
examples <- c("Br","Prb1","Brd2","Lgh");
for (a in 1:4)	{
	ii <- sort(sites_per_stage_slice_otus[sites_per_stage_slice_otus[,colnames(sites_per_stage_slice_otus) %in% "Br"]>0,colnames(sites_per_stage_slice_otus) %in% examples[a]],decreasing=TRUE);
	ii <- round(ii,0)
	ii <- ii[ii>0];
	jj <- ii/round(sites_per_stage_slice_controls[names(sites_per_stage_slice_controls) %in% examples[a]],0);
	ij <- length(jj);
	mxy <- log10(0.1*ceiling(max(jj)/0.1));
	mny <- log10(0.01*floor(min(jj)/0.01));
	numbers <- c();
	for (i in floor(mny):ceiling(mxy))	numbers <- c(numbers,(1:9)*10^i);
	numbers <- numbers[numbers>=10^mny];
	numbers <- numbers[numbers<=10^mxy];
	xlab_off <- 0.5;
	specify_basic_plot(mxx=ij,mnx=0,mxy=mxy,mny=mny,main="Bartonian",abscissa = "Rank Order Occurrences",ordinate="Prop. Possible Occurrences",font=franky,xsize=xsize,ysize=ysize,cexlab = 1.25,main_off = -0.5,xlab_off = xlab_off ,cexmain = 7/8);
	specified_axis_w_labels(axe=1,max_val=ij,min_val=0,maj_break=5,med_break=1,min_break=0,axis_labels=1:ij,label_pos="mid",font=franky,font_size=0.75/2,text_pos=-6);
	med_ticks <- numbers[log10(numbers)!=ceiling(log10(numbers))];
	numbers <- numbers[log10(numbers)==ceiling(log10(numbers))];
	log10_axes(axe=2,min_ax=mny,max_ax=mxy,numbers=numbers,med_ticks=med_ticks,font=franky,orient=2,font_size = 0.75/2,text_pos=0);
	ptcol <- stage_slices$color[match("Br",stage_slices$interval)];
	points((1:ij)-0.5,log10(jj),pch=21,bg=ptcol,cex=0.5);
	}




# Older STuff ####
xx <- pbdb_sites[pbdb_sites$collection_no %in% pbdb_finds$collection_no[pbdb_finds$accepted_name %in% "Schizodelphis sulcatus"],];
xx <- xx[order(-xx$ma_lb),];

mxy <- ceiling(max(otu_info$nfinds)/5)*5;
mny <- 1;
mxx <- ceiling(sum(otu_info$nfinds<100)/10)*10;
mnx <- 0;
specify_basic_plot(mxx=mxx,mnx=mnx,mxy=log10(mxy),mny=log10(mny),abscissa = "Taxon Ranked by Finds",ordinate="PBDB First Occurrence Candidates",font=franky,xsize=4.5)
xbreaks <- as.numeric(set_axis_breaks_new(mxx));
specified_axis(axe=1,max_val=mxx,min_val=0,maj_break=xbreaks[1],med_break=xbreaks[2],min_break=xbreaks[3],font=franky);
numbers <- c(1,10*(1:(mxy/10)));
med_ticks <- seq(5,mxy,by=5)[!seq(5,mxy,by=5) %in% numbers];
min_ticks <- seq(1,mxy,by=1)[!seq(1,mxy,by=1) %in% c(numbers,med_ticks)];
log10_axes(axe=2,min_ax=log10(mny),max_ax=log10(mxy),numbers=numbers,med_ticks=med_ticks,min_ticks=min_ticks,orient=2,font=franky)
lines(1:sotu,log10(otu_info$nfinds[1:sotu]),lwd=2);

fa_taxa <- (1:notu)[otu_info$fa_finds>0];
points(fa_taxa,log10(otu_info$fa_finds[fa_taxa]),pch=21,bg=spc_colors[fa_taxa],col=spc_colors[fa_taxa],lwd=0.25,cex=0.5);
points(400,log10(60),pch=21,,bg=halloween_colors[1],col=halloween_colors[1],lwd=0.25);
text(400,log10(60),": Extant",pos=4,family=franky);
points(400,log10(45),pch=21,,bg=halloween_colors[2],col=halloween_colors[2],lwd=0.25);
text(400,log10(45),": Extinct",pos=4,family=franky);

specify_basic_plot(mxx=mxx,mnx=mnx,mxy=log10(mxy),mny=log10(mny),abscissa = "Taxon Ranked by Finds",ordinate="PBDB Last Occurrence Candidates",font=franky,xsize=4.5)
specified_axis(axe=1,max_val=mxx,min_val=0,maj_break=xbreaks[1],med_break=xbreaks[2],min_break=xbreaks[3],font=franky);
log10_axes(axe=2,min_ax=log10(mny),max_ax=log10(mxy),numbers=numbers,med_ticks=med_ticks,min_ticks=min_ticks,orient=2,font=franky)
lines(1:sotu,log10(otu_info$nfinds[1:sotu]),lwd=2);
la_taxa <- (1:notu)[otu_info$la_finds>0];
points(la_taxa,log10(otu_info$la_finds[la_taxa]),pch=21,bg=spc_colors[la_taxa],col=spc_colors[la_taxa],lwd=0.25,cex=0.5);
points(400,log10(60),pch=21,,bg=halloween_colors[1],col=halloween_colors[1],lwd=0.25);
text(400,log10(60),": Extant",pos=4,family=franky);
points(400,log10(45),pch=21,,bg=halloween_colors[2],col=halloween_colors[2],lwd=0.25);
text(400,log10(45),": Extinct",pos=4,family=franky);

otu_info$rt_finds[boundary_crossers]
otu_info[otu_info$rt_finds>0,]

boundary_crossers <- (1:notu)[otu_info$boundary_crosser];
fa_combos <- array(0,dim=c(max(otu_info$nfinds),max(otu_info$fa_finds)));
la_combos <- array(0,dim=c(max(otu_info$nfinds),max(otu_info$la_finds)));
relv_boundary_crosser <- otu_info$boundary_crosser;
relv_boundary_crosser[otu_info$fa_finds==0] <- FALSE;
otu_info$nfinds[relv_boundary_crosser];
otu_info$fa_finds[relv_boundary_crosser];
otu_info$la_finds[relv_boundary_crosser];
for (sp in 1:notu) {
	if (relv_boundary_crosser[sp])	{
		fa_combos[otu_info$nfinds[sp],otu_info$fa_finds[sp]] <- fa_combos[otu_info$nfinds[sp],otu_info$fa_finds[sp]]+1;
		la_combos[otu_info$nfinds[sp],otu_info$la_finds[sp]] <- la_combos[otu_info$nfinds[sp],otu_info$la_finds[sp]]+1;
		}
	}
max_combos <- max(c(max(fa_combos),max(fa_combos)));
max_area <- max(c(max(fa_combos),max(fa_combos)))

mxx <- mxy <- max(otu_info$nfinds[relv_boundary_crosser]);
mnx <- mny <- 0;
specify_basic_plot(mxx,mnx,mxy,mny,abscissa = "Total Number of Species Occurrences",ordinate="Number of Occurrences from Earliest Interval",xsize=3,ysize = 3,font=franky);
xbreaks <- as.numeric(set_axis_breaks_new(mxx));
specified_axis(axe=1,max_val=mxx,min_val=mnx,maj_break=xbreaks[1],med_break=xbreaks[2],min_break=xbreaks[3],font=franky);
specified_axis(axe=2,max_val=mxx,min_val=mnx,maj_break=xbreaks[1],med_break=xbreaks[2],min_break=xbreaks[3],font=franky);
segments(mnx,mny,mxx,mxy,lty=2,lwd=2)
for (ma in max_combos:1)	for (i in 1:mxx)	for (j in 1:max(otu_info$fa_finds))	if (fa_combos[i,j]==ma)	points(i,j,pch=21,bg="gray75",cex=sqrt(ma)/2);

mxx <- max(otu_info$nfinds[relv_boundary_crosser]);
mxy <- max(otu_info$fa_finds[relv_boundary_crosser]);
mnx <- mny <- 0;
specify_basic_plot(mxx,mnx,mxy,mny,abscissa = "Total Number of Species Occurrences",ordinate="Number of Occurrences from Earliest Interval",xsize=3,ysize = 3,font=franky);
xbreaks <- as.numeric(set_axis_breaks_new(mxx));
ybreaks <- as.numeric(set_axis_breaks_new(mxy));
ybreaks[ybreaks<1] <- 1;
specified_axis(axe=1,max_val=mxx,min_val=mnx,maj_break=xbreaks[1],med_break=xbreaks[2],min_break=xbreaks[3],font=franky);
specified_axis(axe=2,max_val=mxy,min_val=mny,maj_break=ybreaks[1],med_break=ybreaks[2],min_break=ybreaks[3],font=franky);
segments(mnx,mny,mxy,mxy,lty=2,lwd=2);
for (ma in max_combos:1)	for (i in 1:mxx)	for (j in 1:max(otu_info$fa_finds))	if (fa_combos[i,j]==ma)	points(i,j,pch=21,bg="gray75",cex=sqrt(ma)/2);

specify_basic_plot(mxx,mnx,mxy,mny,abscissa = "Total Number of Species Occurrences",ordinate="Number of Occurrences from Latest Interval",xsize=3,ysize = 3,font=franky);
specified_axis(axe=1,max_val=mxx,min_val=mnx,maj_break=xbreaks[1],med_break=xbreaks[2],min_break=xbreaks[3],font=franky);
specified_axis(axe=2,max_val=mxy,min_val=mny,maj_break=ybreaks[1],med_break=ybreaks[2],min_break=ybreaks[3],font=franky);
segments(mnx,mny,mxy,mxy,lty=2,lwd=2);
for (ma in max_combos:1)	for (i in 1:mxx)	for (j in 1:max(otu_info$la_finds))	if (la_combos[i,j]==ma)	points(i,j,pch=21,bg="gray75",cex=sqrt(ma)/2);

otu_info[1,]
{}

#https://paleobiodb.org/data1.2/occs/list.csv?all_records&ident=all&private&show=refattr,classext,rem,entname,abund,crmod&limit=all

# data augmentation: cut!
fake_find <- pbdb_finds[match("Xenorophidae",pbdb_finds$family),];
fake_site <- pbdb_sites[match("Chandler Bridge",pbdb_sites$formation),];
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
fake_find$identified_name <- fake_find$accepted_name <- "Xenorophidae indet ChM PV5711";
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);
fake_find <- pbdb_finds[match("Xenorophidae",pbdb_finds$family),];
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
fake_find$identified_name <- fake_find$accepted_name <- "Xenorophidae indet ChM PV4746";
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);
fake_find$identified_name <- fake_find$accepted_name <- "Xenorophidae indet ChM PV2758";
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);
fake_find$identified_name <- fake_find$accepted_name <- "Albertocetus sp ChM PV4834";
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);
pbdb_sites[pbdb_sites$collection_no==196878,];
fake_find <- pbdb_finds[match("Squalodontidae",pbdb_finds$family),];
fake_find$identified_name <- fake_find$accepted_name <- "Squalodontidae indet ChM PV4961";
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);
fake_find <- pbdb_finds[match("Cynthiacetus",pbdb_finds$genus),];
fake_find$identified_name <- fake_find$accepted_name <- "cf Cynthiacetus sp CCNHM 167"
fake_site <- pbdb_sites[match("Tupelo Bay",pbdb_sites$formation),];
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
fake_find <- pbdb_finds[match("Agorophiidae",pbdb_finds$family),];
fake_find$identified_name <- fake_find$accepted_name <- "Agorophiidae indet ChM PV5852";
fake_site <- pbdb_sites[match("Ashley",pbdb_sites$formation),];
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);

fake_find <- pbdb_finds[match("Cetacea",pbdb_finds$order),];
fake_find$identified_name <- fake_find$accepted_name <- "Mysticeti indet ChM PV4745";
fake_site <- pbdb_sites[match("Ashley",pbdb_sites$formation),];
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);
fake_find$identified_name <- fake_find$accepted_name <- "Odontoceti indet ChM PV2764";
fake_site <- pbdb_sites[match("Ashley",pbdb_sites$formation),];
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);

fake_find$identified_name <- fake_find$accepted_name <- "Mysticeti indet ChM PV5720"
fake_site <- pbdb_sites[match("Chandler Bridge",pbdb_sites$formation),];
fake_find$collection_no <- fake_site$collection_no <- 1+max(pbdb_sites$collection_no);
pbdb_finds <- rbind(pbdb_finds,fake_find);
pbdb_sites <- rbind(pbdb_sites,fake_site);
{}

update_these <- FALSE;
if (update_these)	{
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 21411] <- 26.8;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 21411] <- 23.6;
	pbdb_sites$rock_no[pbdb_sites$member %in% "Givhans Ferry" & pbdb_sites$formation_no==22129] <- pbdb_sites$rock_no_sr[pbdb_sites$member %in% "Givhans Ferry" & pbdb_sites$formation_no==22129] <- 30650;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30650] <- 29.2;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30650] <- 27.9;
	pbdb_sites$rock_no[pbdb_sites$member %in% "Runnymede Marl" & pbdb_sites$formation_no==22129] <- pbdb_sites$rock_no_sr[pbdb_sites$member %in% "Givhans Ferry" & pbdb_sites$formation_no==22129] <- 30651;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30650] <- 29.2;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30650] <- 29.0;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 22129] <- 29.7;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 18848] <- 23.04;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 18848] <- 20.43;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Yuda"] <- 30654;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30654] <- 15.97;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30654] <- 13.82;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Kunnui"] <- 30659;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30659] <- 23.04;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30659] <- 15.97;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Yakumo"] <- 30661;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30659] <- 15.97;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30659] <- 11.63;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Kadonosawa"] <- 30663;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30663] <- 16.8;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30663] <- 15.97;
	pbdb_sites$rock_no[pbdb_sites$rock_no %in% 30663 & pbdb_sites$member %in% "Shikonai"] <- 30666;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30666] <- 16.25;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30666] <- 15.97;
	pbdb_sites$rock_no[pbdb_sites$rock_no %in% 30663 & pbdb_sites$member %in% "Tate"] <- 30667;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Suenomatsuyama"] <- 30665;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30665] <- 15.97;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30665] <- 14.89;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 27341] <- 41.03;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 27341] <- 40.0;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Panandhro"] <- 30669
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30669] <- 41.03;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30669] <- 40.0;
	pbdb_sites$rock_no[pbdb_sites$collection_no %in% 43029] <- 27341;
	pbdb_sites$ma_lb[pbdb_sites$collection_no %in% 43029] <- 41.15;
	pbdb_sites$ma_ub[pbdb_sites$collection_no %in% 43029] <- 39.0;
	pbdb_sites$formation[pbdb_sites$collection_no %in% 43029] <- "Harudi";
	pbdb_finds$occurrence_comments[pbdb_finds$collection_no==196950] <- "Cetotheriidae indet. ZIRM V28/1";
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Sabbie d'Asti"] <- 30670;
	pbdb_sites$ma_lb[pbdb_sites$formation %in% "Sabbie d'Asti"] <- 3.8;
	pbdb_sites$ma_ub[pbdb_sites$formation %in% "Sabbie d'Asti"] <- 3.0;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Panandhro"] <- 30669;
	pbdb_sites$ma_lb[pbdb_sites$formation %in% "Panandhro"] <- 41.2
	pbdb_sites$ma_ub[pbdb_sites$formation %in% "Panandhro"] <- 39.0
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Gehlberg"] <- 30671;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30671] <- 41.2;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30671] <- 39.0;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Blinovo"] <- 30678;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30678] <- 9.78;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30678] <- 7.55;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Chersonian"] <- 30681;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30681] <- 8.9;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30681] <- 7.55;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Itahashi"] <- 30689;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30689] <- 17.95;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30689] <- 14.8;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Mochikubetsu"] <- 30687;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30687] <- 2.39;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30687] <- 0.78;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Embetsu"] <- 30668;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30668] <- 15.97;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30668] <- 11.63;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Sables d'Anvers"] <- 30682;
	pbdb_sites$ma_lb[pbdb_sites$rock_no %in% 30682] <- 15.97;
	pbdb_sites$ma_ub[pbdb_sites$rock_no %in% 30682] <- 11.63;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Kharkov"] <- 30697;
	pbdb_sites$ma_lb[pbdb_sites$collection_no %in% c(77754,78014)] <- 37.67;
	pbdb_sites$ma_ub[pbdb_sites$collection_no %in% c(77754,78014)] <- 36.8;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Cromer Forest"] <- 30691;
	pbdb_sites$ma_lb[pbdb_sites$collection_no %in% 58420] <- 1.82;
	pbdb_sites$ma_ub[pbdb_sites$collection_no %in% 58420] <- 1.78;
	pbdb_sites$ma_lb[pbdb_sites$collection_no %in% c(77754,78014)] <- 0.86;
	pbdb_sites$ma_ub[pbdb_sites$collection_no %in% c(77754,78014)] <- 0.48;
	pbdb_sites$ma_lb[pbdb_sites$collection_no==159732] <- 0.01292;
	pbdb_sites$ma_ub[pbdb_sites$collection_no==159732] <- 0.01284;
	pbdb_sites$ma_lb[pbdb_sites$collection_no==159733] <- 0.03852;
	pbdb_sites$ma_ub[pbdb_sites$collection_no==159733] <- 0.03832;
	pbdb_sites$ma_lb[pbdb_sites$collection_no==238805] <- 0.01333;
	pbdb_sites$ma_ub[pbdb_sites$collection_no==238805] <- 0.01315;
	pbdb_sites$ma_lb[pbdb_sites$formation_no==21480] <- pbdb_sites$ma_lb[pbdb_sites$rock_no_sr==21479] <- 36.8;
	pbdb_sites$ma_ub[pbdb_sites$formation_no==21480] <- pbdb_sites$ma_ub[pbdb_sites$rock_no_sr==21479] <- 35.1;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Chippubetsu"] <- 30757;
	pbdb_sites$ma_lb[pbdb_sites$rock_no==30757] <- 5.55;
	pbdb_sites$ma_ub[pbdb_sites$rock_no==30757] <- 4.79;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Horokaoshirarika"] <- 30760;
	pbdb_sites$ma_lb[pbdb_sites$collection_no %in% c(45803,125179,224899)] <- 5.2;
	pbdb_sites$ma_ub[pbdb_sites$collection_no %in% c(45803,125179,224899)] <- 4.79;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Fossil Bluff Sandstone"] <- 30773;
	pbdb_sites$ma_lb[pbdb_sites$rock_no==30773] <- 22.5;
	pbdb_sites$ma_ub[pbdb_sites$rock_no==30773] <- 19.4;
	pbdb_sites$ma_ub[pbdb_sites$collection_no==28307] <- 21.3;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Hikatagawa"] <- 30767;
	pbdb_sites$ma_lb[pbdb_sites$rock_no==30767] <- 15.2;
	pbdb_sites$ma_ub[pbdb_sites$rock_no==30767] <- 11.63;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Gebel Hof"] <- 30775;
	pbdb_sites$ma_lb[pbdb_sites$rock_no==30775] <- 41.15;
	pbdb_sites$ma_ub[pbdb_sites$rock_no==30775] <- 40.0;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Hiramatsu"] <- 30780;
	pbdb_sites$ma_lb[pbdb_sites$rock_no==30780] <- 18.14;
	pbdb_sites$ma_ub[pbdb_sites$rock_no==30780] <- 16.8;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Cuevas"] <- 30781;
	pbdb_sites$ma_lb[pbdb_sites$rock_no==30781] <- 4.37;
	pbdb_sites$ma_ub[pbdb_sites$rock_no==30781] <- 3.85;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Gonda"] <- 30790;
	pbdb_sites$ma_lb[pbdb_sites$rock_no==30790] <- 7.26;
	pbdb_sites$ma_ub[pbdb_sites$rock_no==30790] <- 5.34;
	pbdb_sites$ma_lb[pbdb_sites$formation %in% "Entre Rios"] <- 11.63;
	pbdb_sites$ma_ub[pbdb_sites$formation %in% "Entre Rios"] <- 7.26
	pbdb_sites$rock_no[pbdb_sites$member %in% "Segendyk"] <- 30802;
	pbdb_sites$ma_lb[pbdb_sites$member %in% "Segendyk"] <- 26.0;
	pbdb_sites$ma_ub[pbdb_sites$member %in% "Segendyk"] <- 24.4;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Koetoi"] <- 30804;
	pbdb_sites$ma_lb[pbdb_sites$formation %in% "Koetoi"] <- 5.55;
	pbdb_sites$ma_ub[pbdb_sites$formation %in% "Koetoi"] <- 4.79;
	#pbdb_sites$ma_ub[pbdb_sites$collection_no==28307] <- 21.3;
	pbdb_sites$rock_no[pbdb_sites$formation %in% "Linzer Sanden"] <- 19085;
	pbdb_sites$ma_lb[pbdb_sites$formation %in% "Linzer Sanden"] <- 26.8;
	pbdb_sites$ma_ub[pbdb_sites$formation %in% "Linzer Sanden"] <- 23.5;
	}


{}

