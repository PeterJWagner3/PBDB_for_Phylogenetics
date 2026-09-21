#### Setup Program ####
# Setup Your Analysis
data_for_R_folder <- "~/Documents/R_Projects/Data_for_R/";
pbdb_directory <- "~/Documents/R_Projects/PaleoDB_Stuff/";
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
finest_chronostrat <- finest_chronostrat[finest_chronostrat$ma_lb<=time_scale$ma_lb[time_scale$interval %in% "Danian"],];
epoch_scale <- time_scale[time_scale$chronostratigraphic_rank %in% "Epoch" & time_scale$scale %in% "International",];
epoch_scale <- epoch_scale[epoch_scale$interval_sr=="",];
rock_to_zone_database <- rock_unit_data$rock_to_zone_database;
rock_unit_database <- rock_unit_data$rock_unit_database;

pbdb_sites <- pbdb_data_list$pbdb_sites_refined;
pbdb_finds <- pbdb_data_list$pbdb_finds;
pbdb_finds_old_ids <- pbdb_data_list$pbdb_finds_oldid;
pbdb_taxonomy <- pbdb_data_list$pbdb_taxonomy;
pbdb_taxonomy <- pbdb_data_list$pbdb_taxonomy;
pbdb_opinions <- pbdb_data_list$pbdb_opinions;
pbdb_references <- pbdb_data_list$pbdb_references;

pbdb_finds$identified_name[pbdb_finds$occurrence_comments %in% c("Balaenopteridae indet aka portisi MGPT 13803","Balaenopteridae indet aka portisi MRSN PU13808","Balaenopteridae indet aka portisi MCZ 17882","Balaenopteridae indet aka portisi SDSNH 21507","Balaenopteridae indet aka portisi SDSNH 65769 et 68698")] <- pbdb_finds$accepted_name[pbdb_finds$occurrence_comments %in% c("Balaenopteridae indet aka portisi MGPT 13803","Balaenopteridae indet aka portisi MRSN PU13808","Balaenopteridae indet aka portisi MCZ 17882","Balaenopteridae indet aka portisi SDSNH 21507","Balaenopteridae indet aka portisi SDSNH 65769 et 68698")] <- "Balaenoptera floridana";
pbdb_finds$identified_no[pbdb_finds$occurrence_comments %in% c("Balaenopteridae indet aka portisi MGPT 13803","Balaenopteridae indet aka portisi MRSN PU13808","Balaenopteridae indet aka portisi MCZ 17882","Balaenopteridae indet aka portisi SDSNH 21507","Balaenopteridae indet aka portisi SDSNH 65769 et 68698")] <- pbdb_finds$accepted_no[pbdb_finds$occurrence_comments %in% c("Balaenopteridae indet aka portisi MGPT 13803","Balaenopteridae indet aka portisi MRSN PU13808","Balaenopteridae indet aka portisi MCZ 17882","Balaenopteridae indet aka portisi SDSNH 21507","Balaenopteridae indet aka portisi SDSNH 65769 et 68698")] <- pbdb_taxonomy$taxon_no[match("Balaenoptera floridana",pbdb_taxonomy$taxon_name)];

# Setup Specifics for This Analysis ####
##### Select Nexus File for analysis ####
nexus_file_name <- file.choose()
#nexus_file_name <- "/Users/peterjwagner/Documents/R_Projects/Code_for_Paper_Reviews/doi_10_5061_dryad_vq83bk3qf__v20210121/Cetacean Metatree Data/Metatree data/Safe/FULL.nex";
char_data <- accersi_data_from_nexus_file(nexus_file_name,polymorphs,UNKNOWN,INAP);
otus <- char_data$OTUs;
otus <- pbdb_taxonomy$accepted_name[match(otus,pbdb_taxonomy$taxon_name)];
notu <- length(otus);
otus[notu] <- "Ribeiria australiensis"
char_matrix <- char_data$Matrix;
nchars <- ncol(char_matrix);
scored <- vector(length=notu);
for (i in 1:notu)	scored[i] <- sum(!char_matrix[i,] %in% c(INAP,UNKNOWN));
sum(scored==0);
otu_finds <- vector(length=notu);
names(otu_finds) <- gsub(" ","_",otus);
pbdb_sites$ma_lb[pbdb_sites$collection_no==96250] <- 529.0;
pbdb_sites$ma_ub[pbdb_sites$collection_no==96250] <- 525.4;
onset <- 0; end <- MAXNO;
otu_boundaries <- otu_site_info <- list();
for (tx in 1:notu)	{
	if (otus[tx] %in% pbdb_taxonomy$taxon_name)	{
		taxon_no <- pbdb_taxonomy$accepted_no[match(otus[tx],pbdb_taxonomy$taxon_name)];
		taxon_finds <- pbdb_finds[pbdb_finds$accepted_no %in% taxon_no,];
		if (nrow(taxon_finds)==0)
			taxon_finds <- pbdb_finds[pbdb_finds$identified_name %in% taxon_no,];
		taxon_finds <- taxon_finds[!taxon_finds$identified_name %in% otus[!otus %in% otus[tx]],]
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
names(otu_boundaries) <- names(otu_site_info) <- gsub(" ","_",otus);
#pbdb_sites[pbdb_sites$collection_no %in% c(208578,208579),c("ma_lb","ma_ub")]


otu_parents <- pbdb_taxonomy$parent_name[match(pbdb_taxonomy$genus_no[match(otus,pbdb_taxonomy$taxon_name)],pbdb_taxonomy$taxon_no)];
otu_parents_no <- pbdb_taxonomy$parent_no[match(pbdb_taxonomy$genus_no[match(otus,pbdb_taxonomy$taxon_name)],pbdb_taxonomy$taxon_no)];
otu_parents <- otu_parents[!is.na(otu_parents)];
otu_parents_no <- otu_parents_no[!is.na(otu_parents_no)];
parent_ranks <- pbdb_taxonomy$accepted_rank[match(pbdb_taxonomy$genus_no[match(otus,pbdb_taxonomy$taxon_name)],pbdb_taxonomy$taxon_no)];

study_taxon <- "Cetacea";
study_taxon_rank <- unique(pbdb_taxonomy$accepted_rank[pbdb_taxonomy$taxon_name %in% study_taxon]);
study_taxon_no <- unique(pbdb_taxonomy$accepted_no[pbdb_taxonomy$taxon_name %in% study_taxon]);
if (study_taxon_rank %in% standard_pbdb_taxon_ranks)	{
	relv_taxonomy <- pbdb_taxonomy[pbdb_taxonomy[,study_taxon_rank] %in% study_taxon,];
	relv_finds <- pbdb_finds[pbdb_finds[,study_taxon_rank] %in% study_taxon,];
	} else	{
	study_taxon_daughters <- accersi_daughter_taxa_from_taxon_no(parent_taxon_no=study_taxon_no,pbdb_taxonomy=pbdb_taxonomy);
	relv_taxonomy <- pbdb_taxonomy[pbdb_taxonomy$orig_no==1,];
	relv_taxonomy <- relv_taxonomy[relv_taxonomy$orig_no<0,];
	relv_finds <- pbdb_finds[pbdb_finds$occurrence_no==1,];
	relv_finds <- relv_finds[relv_finds$occurrence_no<1,];
	for (dd in 1:nrow(study_taxon_daughters))	{
		colname <- paste(study_taxon_daughters$daughter_rank[dd],"_no",sep="");
		relv_taxonomy <- rbind(relv_taxonomy,pbdb_taxonomy[pbdb_taxonomy[,colname]==study_taxon_daughters$taxon_no[dd],]);
		relv_finds <- pbdb_finds[pbdb_finds[,colname]==study_taxon_daughters$taxon_no[dd],];
		}
	}
# add outdated identifications
if (is.null(pbdb_finds_old_ids$accepted_name_orig))	pbdb_finds_old_ids$accepted_name_orig <- pbdb_finds_old_ids$accepted_name;
relv_finds <- unique(rbind(relv_finds,pbdb_finds_old_ids[pbdb_finds_old_ids$occurrence_no %in% relv_finds$occurrence_no,]));	# add old ids

relv_finds$identified_name <- gsub(" n. sp.","",relv_finds$identified_name);
relv_finds$identified_name <- gsub(" n. gen.","",relv_finds$identified_name);
relv_finds$identified_name <- gsub(" n. ssp.","",relv_finds$identified_name);
relv_finds$identified_name <- gsub("<sp. ","sp ",relv_finds$identified_name);
relv_finds$identified_name <- gsub("<sp ","sp ",relv_finds$identified_name);
relv_finds$identified_name <- gsub("<indet. ","indet ",relv_finds$identified_name);
relv_finds$identified_name <- gsub("<indet ","indet ",relv_finds$identified_name);
relv_finds$identified_name[relv_finds$identified_name!=gsub(">","",relv_finds$identified_name) & relv_finds$identified_name==gsub("<","",relv_finds$identified_name)] <- gsub(">","",relv_finds$identified_name[relv_finds$identified_name!=gsub(">","",relv_finds$identified_name) & relv_finds$identified_name==gsub("<","",relv_finds$identified_name)])
relv_finds$identified_name[relv_finds$identified_name!=gsub("/","",relv_finds$identified_name)] <- gsub("/","-",relv_finds$identified_name[relv_finds$identified_name!=gsub("/","",relv_finds$identified_name)]);
rfinds <- nrow(relv_finds);

#pbdb_finds$identified_name[pbdb_finds$accepted_name %in% "Pomatodelphis inaequalis"]
otus[otus %in% "Mysticeti indet USNM 314627"] <- "Maiabalaena nesbittae";
otus[otus %in% "Cetotheriidae indet ZIRM V28 1"] <- "Ciuciulea davidi";
otus[otus %in% "Xenorophus sp ChM PV4823"] <- "Xenorophus simplicidens";
otus[otus %in% "Basilosauridae indet MUSM 1443"] <- "Pachycetus paulsonii";
otus[otus %in% "Scaphokogiinae indet MUSM 3291 et 3405"] <- "Platyscaphokogia landinii";
otus[otus %in% "Balaenopteridae indet RBINS M.2315"] <- "Protororqualus wilfriedneesi";
otus[otus %in% "Balaenopteridae indet aka portisi MRSN PU13808 et MGPT 13803 et MCZ 17882 et SDSNH 21507 et 65769 et 68698"] <- "Balaenoptera floridana";
rownames(char_matrix) <- otus;
otus <- otus[!otus %in% "Mysticeti indet ChM PV4745"]; # juvenile, possibly of Coronodon havensteini according to Geisler et al. 2017
otus <- otus[otus!="allzero"];
otus <- unique(otus);
notu <- length(otus);
char_matrix <- char_matrix[rownames(char_matrix) %in% otus,]
name_counts <- hist(match(rownames(char_matrix),otus),breaks=0:notu,plot=FALSE)$counts;
names(name_counts) <- otus;
duplicates <- names(name_counts)[name_counts>1];
char_matrix <- char_matrix[unique(match(rownames(char_matrix),otus)),];
otus <- gsub(" dot ","\\.",otus);
otus <- gsub(" dash ","-",otus);
otus <- gsub(" slash ","-",otus);
rownames(char_matrix) <- otus;
spc_coded <- vector(length=notu);
for (sp in 1:notu)	spc_coded[sp] <- sum(!char_matrix[sp,] %in% c(UNKNOWN,INAP));

##### Set aside & edit PBDB Data ####
relv_genera <- unique(pbdb_taxonomy$genus[match(otus,pbdb_taxonomy$taxon_name)[!is.na(match(otus,pbdb_taxonomy$taxon_name))]]);
relv_genera_no <- unique(pbdb_taxonomy$genus_no[match(otus,pbdb_taxonomy$taxon_name)[!is.na(match(otus,pbdb_taxonomy$taxon_name))]]);
relv_families <- unique(pbdb_taxonomy$family[match(otus,pbdb_taxonomy$taxon_name)[!is.na(match(otus,pbdb_taxonomy$taxon_name))]]);
relv_families_no <- unique(pbdb_taxonomy$family_no[match(otus,pbdb_taxonomy$taxon_name)[!is.na(match(otus,pbdb_taxonomy$taxon_name))]]);
relv_genera_no <- relv_genera_no[relv_genera_no>0];
relv_families_no <- relv_families_no[relv_families_no>0];

relv_sites <- pbdb_sites[pbdb_sites$collection_no %in% relv_finds$collection_no,];
rsites_all <- nrow(relv_sites);
relv_sites <- relv_sites[relv_sites$ma_lb > 0.0117,]; # exclude Holocene;
relv_sites <- relv_sites[relv_sites$ma_lb <= time_scale$ma_lb[time_scale$interval=="Cenozoic"],]; # exclude Holocene;
rsites <- nrow(relv_sites);
relv_finds <- relv_finds[relv_finds$collection_no %in% relv_sites$collection_no,]
rfinds <- nrow(relv_finds);
site_refs <- sort(unique(relv_sites$reference_no));
site_ref_counts <- hist(relv_sites$reference_no,breaks=c(0,site_refs),plot=FALSE)$counts;
find_refs <- sort(unique(relv_finds$reference_no));
find_ref_counts <- hist(relv_finds$reference_no,breaks=c(0,find_refs),plot=FALSE)$counts;
reference_summary <- data.frame(reference_no=find_refs,noccs=find_ref_counts);
reference_summary <- reference_summary[order(-reference_summary$noccs),];
reference_summary$citation <- pbdb_references$formatted[match(reference_summary$reference_no,pbdb_references$reference_no)];
#reference_summary$citation[1:11]

relv_boundaries <- get_chronostratigraphic_boundaries(relv_sites);

otu_info <- data.frame(taxon=otus,taxon_no=pbdb_taxonomy$taxon_no[match(otus,pbdb_taxonomy$taxon_name)],
					   taxon_sr=pbdb_taxonomy$accepted_name[match(otus,pbdb_taxonomy$taxon_name)],
					   accepted_no=pbdb_taxonomy$accepted_no[match(otus,pbdb_taxonomy$taxon_name)],
					   taxon_rank=pbdb_taxonomy$accepted_rank[match(otus,pbdb_taxonomy$taxon_name)],extant=rep(FALSE,notu),
					   fa_lb=rep(0,notu),fa_ub=rep(0,notu),la_lb=rep(0,notu),la_ub=rep(0,notu),
					   nfinds=rep(0,notu),fa_finds=rep(0,notu),la_finds=rep(0,notu),rt_finds=rep(0,notu),
					   nrocks=rep(0,notu),fa_rocks=rep(0,notu),la_rocks=rep(0,notu),rt_rocks=rep(0,notu),
					   boundary_crosser=rep(FALSE,notu),min_range=rep(0,notu));
otu_info$taxon_sr[is.na(otu_info$taxon_sr)] <- otu_info$taxon[is.na(otu_info$taxon_sr)];
juniors <- otu_info[otu_info$taxon!=otu_info$taxon_sr,];
juniors$difference <- pbdb_taxonomy$difference[match(juniors$taxon,pbdb_taxonomy$taxon_name)];
#unique(pbdb_taxonomy$difference[match(juniors$taxon,pbdb_taxonomy$taxon_name)])

# alter finds so that junior synonyms are separated from senior synonyms IF senior is also in analysis
#	also: elevate nomens & other invalids
#	ignore recombinations
for (jr in 1:nrow(juniors))	{
	species_taxonomy <- pbdb_taxonomy[pbdb_taxonomy$taxon_name %in% juniors$taxon[jr],];
	wonky <- FALSE;
	if (nrow(species_taxonomy)>1 & (!"objective synonym of" %in% species_taxonomy$difference & !"replaced by" %in% species_taxonomy$difference))	{
		if (sum(species_taxonomy$accepted_name %in% otu_info$taxon)<nrow(species_taxonomy))
			species_taxonomy <- species_taxonomy[!species_taxonomy$accepted_name %in% otu_info$taxon,];
		if (nrow(species_taxonomy)>1)	{
			species_taxonomy_expanded <- pbdb_taxonomy[pbdb_taxonomy$orig_no %in% species_taxonomy$orig_no,];
			if (sum(species_taxonomy_expanded$taxon_no %in% pbdb_finds$identified_no)==1)	{
				species_taxonomy_expanded <- species_taxonomy_expanded[species_taxonomy_expanded$taxon_no %in% pbdb_finds$identified_no,];
				species_taxonomy <- species_taxonomy[species_taxonomy$orig_no %in% species_taxonomy_expanded$orig_no,];
				}
			}
		if (nrow(species_taxonomy)>1)	{
			orig_refs <- sapply(species_taxonomy$taxon_attr,strsplit," ")
			pub_yr <- vector(length=nrow(species_taxonomy));
			for (i in 1:length(orig_refs))	pub_yr[i] <- as.numeric(orig_refs[[i]][length(orig_refs[[i]])]);
			species_taxonomy <- species_taxonomy[match(min(pub_yr),pub_yr),]
			wonky <- TRUE;
			}
		if (nrow(species_taxonomy)==1)	{
			juniors$taxon_no[jr] <- species_taxonomy$taxon_no;
			juniors$accepted_no[jr] <- species_taxonomy$accepted_no;
			juniors$taxon_sr[jr] <- species_taxonomy$accepted_name;
			}
		}
	if (is.species(juniors$taxon_sr[jr]) || is.subspecies(juniors$taxon_sr[jr]))	{
		if ("objective synonym of"  %in% species_taxonomy$difference & juniors$taxon_sr[jr] %in% otus & nrow(species_taxonomy)>1)	{
			species_taxonomy <- species_taxonomy[gsub("objective","",species_taxonomy$difference)==species_taxonomy$difference,]
			juniors$taxon_sr[jr] <- species_taxonomy$accepted_name[1];
			juniors$taxon_rank[jr] <- species_taxonomy$accepted_rank[1];
			juniors$taxon_no[jr] <- species_taxonomy$taxon_no[match(juniors$taxon[jr],species_taxonomy$taxon_name)];
			juniors$accepted_no[jr] <- species_taxonomy$accepted_no[match(juniors$taxon[jr],species_taxonomy$taxon_name)];
			juniors$difference <- species_taxonomy$difference[match(juniors$taxon[jr],species_taxonomy$taxon_name)];
#			relv_finds[relv_finds$identified_no %in% species_taxonomy$taxon_no,]
			}
		if (species_taxonomy$difference %in% "subjective synonym of" && juniors$taxon_sr[jr] %in% otus)	{
			all_names <- unique(pbdb_taxonomy$taxon_name[pbdb_taxonomy$orig_no %in% species_taxonomy$orig_no])
			alter_these <- relv_finds[relv_finds$identified_name %in% all_names,];
			for (an in 1:length(all_names))
				alter_these <- unique(rbind(alter_these,relv_finds[gsub(all_names[an],"",relv_finds$identified_name)!=relv_finds$identified_name,]));
			if (nrow(alter_these)>0)	{
				alter_these$genus <- divido_genus_names_from_species_names(juniors$taxon[jr])
				alter_these$genus_no <- pbdb_taxonomy$taxon_no[match(alter_these$genus[1],pbdb_taxonomy$taxon_name)];
				alter_these$family <- pbdb_taxonomy$family[match(alter_these$genus_no[1],pbdb_taxonomy$taxon_no)];
				alter_these$family_no <- pbdb_taxonomy$family_no[match(alter_these$genus_no[1],pbdb_taxonomy$taxon_no)];
				alter_these$accepted_name <- alter_these$accepted_name_orig <- juniors$taxon[jr];
				alter_these$accepted_no <- relv_taxonomy$taxon_no[match(juniors$taxon[jr],relv_taxonomy$taxon_name)];

				relv_finds[relv_finds$occurrence_no %in% alter_these$occurrence_no,] <- alter_these;
				}
			}
		} else if (gsub("nomen","",species_taxonomy$difference)!=species_taxonomy$difference)	{
		all_names <- unique(pbdb_taxonomy$taxon_name[pbdb_taxonomy$orig_no %in% species_taxonomy$orig_no])
		alter_these <- relv_finds[relv_finds$identified_name %in% all_names,];
#		pbdb_finds[pbdb_finds$collection_no==65176,]
		for (an in 1:length(all_names))
			alter_these <- unique(rbind(alter_these,relv_finds[gsub(all_names[an],"",relv_finds$identified_name)!=relv_finds$identified_name,]));
		alter_these$genus <- divido_genus_names_from_species_names(juniors$taxon[jr])
		alter_these$genus_no <- pbdb_taxonomy$taxon_no[match(alter_these$genus[1],pbdb_taxonomy$taxon_name)];
		alter_these$family <- pbdb_taxonomy$family[match(alter_these$genus_no[1],pbdb_taxonomy$taxon_no)];
		alter_these$family_no <- pbdb_taxonomy$family_no[match(alter_these$genus_no[1],pbdb_taxonomy$taxon_no)];
		alter_these$accepted_name <- alter_these$accepted_name_orig <- juniors$taxon[jr];
		alter_these$accepted_no <- relv_taxonomy$taxon_no[match(juniors$taxon[jr],relv_taxonomy$taxon_name)];

		relv_finds[relv_finds$occurrence_no %in% alter_these$occurrence_no,] <- alter_these;
		juniors$taxon_rank[jr] <- species_taxonomy$taxon_rank[match(juniors$taxon[jr],species_taxonomy$taxon_name)];
		}
	}

##### Get stratigraphic information ####
juniors$difference <- NULL;
otu_info[otu_info$taxon %in% juniors$taxon,] <- juniors;
#sp <- match("Eudelphinus compressus",otus);
analyzed_finds <- pbdb_finds[1,];
analyzed_finds <- analyzed_finds[analyzed_finds$occurrence_no<1,];
analyzed_finds_fas <- analyzed_finds_las <- analyzed_finds;
analyzed_sites <- pbdb_sites[1,];
analyzed_sites <- analyzed_sites[analyzed_sites$occurrence_no<1,];
analyzed_sites_fas <- analyzed_sites_las <- analyzed_sites;
printbreaks <- round((notu*(1:9))/10,0);
boundary_rocks <- c();
stage_slices <- stage_slices[abs(stage_slices$ma_lb)<=abs(time_scale$ma_lb[time_scale$interval=="Cenozoic"]),];
stage_slices <- stage_slices[order(-abs(stage_slices$ma_lb)),]
hierarchical_chronostrat <- accersi_hierarchical_timescale(chronostrat_units=stage_slices$interval,time_scale=stage_slices,regional_scale="Stage Slice",ma_fuzz=10);
hierarchical_chronostrat <- hierarchical_chronostrat[order(-abs(hierarchical_chronostrat$ma_lb)),]
sites_per_stage_slice_dummy <- sites_per_stage_slice_otus <- tally_collections_occupied_by_subinterval(taxon_collections=relv_sites,hierarchical_chronostrat = hierarchical_chronostrat);
sites_per_stage_slice_dummy[sites_per_stage_slice_dummy>0] <- 0;
sites_per_stage_slice_otus <- array(sites_per_stage_slice_otus,dim=c(1,length(sites_per_stage_slice_otus)));
colnames(sites_per_stage_slice_otus) <- names(sites_per_stage_slice_dummy);
sites_per_stage_slice_otus <- sites_per_stage_slice_otus[sites_per_stage_slice_otus[,1]<0,];
hierarchical_chronostrat$ma_lb <- abs(hierarchical_chronostrat$ma_lb);
hierarchical_chronostrat$ma_ub <- abs(hierarchical_chronostrat$ma_ub);
sites_per_stage_slice_otus <- array(0,dim=c(notu,nrow(finest_chronostrat)));
rownames(sites_per_stage_slice_otus) <- otus;
colnames(sites_per_stage_slice_otus) <- finest_chronostrat$interval;
for (sp in 1:notu)	{
	if (sp %in% printbreaks)	print(paste(match(sp,printbreaks)*10,"% done",sep=""));
	otu_info$extant[sp] <- extant <- FALSE;
	if (!is.na(match(otus[sp],relv_taxonomy$taxon_name)))	{
		this_taxonomy <- relv_taxonomy[relv_taxonomy$taxon_name %in% otus[sp],];
		if (nrow(this_taxonomy)>1 && sum(this_taxonomy$difference=="")>0)
			this_taxonomy <- this_taxonomy[this_taxonomy$difference=="",];
		if (nrow(this_taxonomy)>1 & sum(this_taxonomy$accepted_no==this_taxonomy$taxon_no)>0)
			this_taxonomy <- this_taxonomy[this_taxonomy$accepted_no==this_taxonomy$taxon_no,];
		if (nrow(this_taxonomy)>1)	{
			birth_years <- vector(length=nrow(this_taxonomy));
			this_taxonomy$taxon_attr <- gsub("\\(","",this_taxonomy$taxon_attr);
			this_taxonomy$taxon_attr <- gsub("\\)","",this_taxonomy$taxon_attr);
			for (i in 1:nrow(this_taxonomy))
				birth_years[i] <- as.numeric(strsplit(this_taxonomy$taxon_attr[i]," ")[[1]][length(strsplit(this_taxonomy$taxon_attr[i]," ")[[1]])]);
			this_taxonomy <- this_taxonomy[birth_years==min(birth_years),];
			}
		otu_info$taxon_sr[sp] <- this_taxonomy$accepted_name;
		otu_info$taxon_rank <- this_taxonomy$accepted_rank;
		}
	if (!is.na(match(otus[sp],relv_taxonomy$taxon_name)))	if (relv_taxonomy$is_extant[match(otus[sp],relv_taxonomy$taxon_name)]=="extant")	extant <- otu_info$extant[sp] <- TRUE;
	if (is.na(match(otus[sp],relv_taxonomy$taxon_name)))	{
		otu_no <- 0;
		species_finds <- unique(rbind(relv_finds[relv_finds$identified_name %in% otus[sp],],
									  relv_finds[relv_finds$accepted_name %in% otus[sp],],
									  relv_finds[relv_finds$occurrence_comments %in% otus[sp],]));

		if (nrow(species_finds)==0)	{
			otu_molecules <- strsplit(otus[sp]," ")[[1]];
			basic_taxon <- otu_molecules[!otu_molecules %in% c("aff","cf")][1];
			basic_ranks <- c("phylum","class","order","family","genus")[relv_taxonomy[match(basic_taxon,relv_taxonomy$taxon_name),c("phylum","class","order","family","genus")]!=""];
			basic_rank <- basic_ranks[length(basic_ranks)];
			basic_taxon <- relv_taxonomy[match(basic_taxon,relv_taxonomy$taxon_name),basic_rank];
			poss_finds <- relv_finds[relv_finds[,basic_rank] %in% basic_taxon,];

			otu_molecules <- otu_molecules[!otu_molecules %in% c("sp","sp.","indet","indet.","cf","cf.","aff","aff.")]
			otu_molecules <- otu_molecules[!otu_molecules %in% relv_taxonomy$taxon_name];
			otu_label <- paste(otu_molecules,collapse=" ");
			species_finds <- poss_finds[gsub(paste(otu_molecules,collapse=" "),"",poss_finds$identified_name)!=poss_finds$identified_name,];
			if (nrow(species_finds)==0)
				species_finds <- poss_finds[gsub(otu_molecules[length(otu_molecules)],"",poss_finds$identified_name)!=poss_finds$identified_name,];
			if (nrow(species_finds)==0)
				species_finds <- poss_finds[gsub(otu_label,"",poss_finds$identified_name)!=poss_finds$identified_name,];
			if (nrow(species_finds)==0)
				species_finds <- pbdb_finds[gsub(otu_label,"",pbdb_finds$occurrence_comments)!=pbdb_finds$occurrence_comments,];
			}
		} else	{
		otu_no <- relv_taxonomy$taxon_no[match(otus[sp],relv_taxonomy$taxon_name)];
		species_finds <- relv_finds[relv_finds$accepted_no %in% otu_no,];
		if (nrow(species_finds)==0)	species_finds <- relv_finds[relv_finds$accepted_name %in% otus[sp],];
		if (nrow(species_finds)==0)	species_finds <- relv_finds[relv_finds$identified_name %in% otus[sp],];
		if (nrow(species_finds)==0)	species_finds <- relv_finds[gsub(otus[sp],"",relv_finds$identified_name)!=relv_finds$identified_name,];
		if (nrow(species_finds)==0)	{
			species_taxonomy <- relv_taxonomy[relv_taxonomy$orig_no %in% relv_taxonomy$orig_no[match(otus[sp],relv_taxonomy$taxon_name)],];
			relv_taxonomy[relv_taxonomy$taxon_name %in% c("Schizodelphis compressus"),]
			species_finds <- relv_finds[relv_finds$accepted_no %in% species_taxonomy$accepted_no,];
			if (nrow(species_finds)==0)
				for (i in 1:nrow(species_taxonomy))
					species_finds <- rbind(species_finds,relv_finds[gsub(species_taxonomy$taxon_name[i],"",relv_finds$identified_name)!=relv_finds$identified_name,]);
			if (nrow(species_finds)==0)
				for (i in 1:nrow(species_taxonomy))
					species_finds <- rbind(species_finds,pbdb_finds[gsub(species_taxonomy$taxon_name[i],"",relv_finds$identified_name)!=relv_finds$identified_name,]);
			}
		}
#	sum(relv_finds$flags %in% c("R","RI","RF","RIF"))
#	if (nrow(species_finds)>0)	if (sum(species_finds$flags %in% c("R","RI","RF","RIF"))>0)	print(paste(sp,otus[sp]));
#	}
	if (nrow(species_finds)>0)	{
		if (sum(species_finds$flags %in% reided)>0 & otu_no>0)	{
			reided_finds <- relv_finds[relv_finds$occurrence_no %in% species_finds$occurrence_no[species_finds$flags %in% reided],];
			reided_finds <- reided_finds[order(reided_finds$occurrence_no,reided_finds$created),]
			reided_keepers <- reided_finds[!reided_finds$flags %in% reided,]
			if (sum(!reided_keepers$accepted_no %in% otu_no)>0 & sum(!reided_keepers$accepted_no %in% otu_no)<nrow(species_finds))
				species_finds <- species_finds[!species_finds$occurrence_no %in% reided_keepers$occurrence_no[!reided_keepers$accepted_no %in% otu_no],];
			}
		if (nrow(species_finds)>1)	{
			dodgy <- (1:nrow(species_finds))[gsub("uncertain species","",species_finds$flags)!=species_finds$flags]
			notdodgy <- (1:nrow(species_finds))[gsub("uncertain species","",species_finds$flags)==species_finds$flags]
			if (length(dodgy)>0 & length(dodgy)<nrow(species_finds))	species_finds <- species_finds[notdodgy,];
			}
		analyzed_finds <- unique(rbind(analyzed_finds,species_finds));
		species_sites <- pbdb_sites[pbdb_sites$collection_no %in% species_finds$collection_no,];
		species_sites <- species_sites[order(-species_sites$ma_lb),];
		#paste(species_sites$collection_no,collapse=",");
		analyzed_sites <- unique(rbind(analyzed_sites,species_sites));
		if (nrow(species_sites)>1)	{
			species_sites <- species_sites[!species_sites$collection_no %in% 48887,];
			sites_per_stage_slice_otus[sp,] <- tally_collections_occupied_by_subinterval(taxon_collections=species_sites,hierarchical_chronostrat = hierarchical_chronostrat);
			}

		otu_info$fa_lb[sp] <- max(species_sites$ma_lb);
		otu_info$fa_ub[sp] <- max(species_sites$ma_ub);
		otu_info$la_lb[sp] <- min(species_sites$ma_lb);
		otu_info$la_ub[sp] <- min(species_sites$ma_ub);
		if (extant)	otu_info$la_lb[sp] <- otu_info$la_ub[sp] <- 0;
		otu_info$nfinds[sp] <- nrow(species_sites);
		otu_info$nrocks[sp] <- length(unique(species_sites$rock_no_sr[species_sites$rock_no_sr>0]));
		if (otu_info$fa_ub[sp]>=otu_info$la_lb[sp])	{
			# case where only some finds might be possible FAs or LAs
			# sites possibly older than the latest first appearance
			species_sites_fa <- species_sites[species_sites$ma_lb>otu_info$fa_ub[sp],];
			otu_info$fa_finds[sp] <- nrow(species_sites_fa);
			otu_info$fa_rocks[sp] <- length(unique(species_sites_fa$rock_no_sr[species_sites_fa$rock_no_sr>0]));
			boundary_rocks <- sort(unique(c(boundary_rocks,species_sites_fa$rock_no_sr[species_sites_fa$rock_no_sr>0])));
			analyzed_sites_fas <- unique(rbind(analyzed_sites_fas,species_sites_fa));
			analyzed_finds_fas <- rbind(analyzed_finds_fas,species_finds[species_finds$collection_no %in% species_sites_fa$collection_no,]);

			# sites possibly younger than the earliest last appearance
			if (!otu_info$extant[sp])	{
				species_sites_la <- species_sites[species_sites$ma_ub<otu_info$la_lb[sp],];
				otu_info$la_finds[sp] <- nrow(species_sites_la);
				# add la sites & finds here
				otu_info$la_rocks[sp] <- length(unique(species_sites_la$rock_no_sr[species_sites_la$rock_no_sr>0]));
				boundary_rocks <- sort(unique(c(boundary_rocks,species_sites_la$rock_no_sr[species_sites_la$rock_no_sr>0])));
				analyzed_sites_las <- unique(rbind(analyzed_sites_las,species_sites_la));
				analyzed_finds_fas <- rbind(analyzed_finds_las,species_finds[species_finds$collection_no %in% species_sites_la$collection_no,]);
				} else	{
				species_sites_la <- species_sites[1,];
				species_sites_la <- species_sites_la[species_sites_la$collection_no<1,];
				}

			if (otu_info$fa_ub[sp]>=otu_info$la_lb[sp])	{
				# boundary crosser !
				otu_info$boundary_crosser[sp] <- TRUE;
				otu_info$min_range[sp] <- otu_info$fa_ub[sp]-otu_info$la_lb[sp];
				boundary_sites <- unique(rbind(species_sites_fa,species_sites_la));
				species_sites_rt <- species_sites[!species_sites$collection_no %in% boundary_sites$collection_no,]
				otu_info$rt_finds[sp] <- nrow(species_sites_rt);
				otu_info$rt_rocks[sp] <- length(unique(species_sites_rt$rock_no_sr[species_sites_rt$rock_no_sr>0]));
				}
			} else	{
			analyzed_sites_fas <- unique(rbind(analyzed_sites_fas,species_sites));
			analyzed_finds_fas <- rbind(analyzed_finds_fas,species_finds);
			if (!otu_info$extant[sp])	{
				analyzed_sites_las <- unique(rbind(analyzed_sites_las,species_sites));
				analyzed_finds_las <- rbind(analyzed_finds_las,species_finds);
				}
			}

#		if (otu_info$fa_ub[sp]>=otu_info$la_ub[sp] & !otu_info$extant[sp])	{
#			# case where only some finds might be possible LAs
#			species_sites_la <- species_sites[species_sites$ma_ub<otu_info$la_lb[sp],];
#			otu_info$la_finds[sp] <- nrow(species_sites_la);
			# add la sites & finds here
#			otu_info$la_rocks[sp] <- length(unique(species_sites_la$rock_no_sr[species_sites_la$rock_no_sr>0]));
#			analyzed_sites_las <- unique(rbind(analyzed_sites_las,species_sites_la));
#			analyzed_finds_fas <- rbind(analyzed_finds_las,species_finds[species_finds$collection_no %in% species_sites_la$collection_no,]);
#			} else	{
#			analyzed_sites_las <- unique(rbind(analyzed_sites_las,species_sites));
#			analyzed_finds_las <- rbind(analyzed_finds_las,species_finds);
#			}

		} else if (otu_info$extant[sp])	otu_info$la_lb[sp] <- otu_info$la_ub[sp] <- 0;
#	if (27502 %in% boundary_rocks)	break;
	}

##### Summarize stratigraphic information ####
boundary_rocks_zoneless <- boundary_rocks[!boundary_rocks %in% rock_to_zone_database$rock_no_sr];
analyzed_sites_edge <- unique(rbind(analyzed_sites_fas,analyzed_sites_las));
missing_rocks <- data.frame(formation=sort(unique(analyzed_sites_edge$formation[analyzed_sites_edge$formation!="" & analyzed_sites_edge$rock_no==0])),
							sites=hist(match(analyzed_sites_edge$formation[analyzed_sites_edge$formation!="" & analyzed_sites_edge$rock_no==0],sort(unique(analyzed_sites_edge$formation[analyzed_sites_edge$formation!="" & analyzed_sites_edge$rock_no==0]))),breaks=0:length(unique(analyzed_sites_edge$formation[analyzed_sites_edge$formation!="" & analyzed_sites_edge$rock_no==0])),plot=FALSE)$counts);
missing_rocks2 <- data.frame(formation=sort(unique(analyzed_sites$formation[analyzed_sites$formation!="" & analyzed_sites$rock_no==0])),
							 sites=hist(match(analyzed_sites$formation[analyzed_sites$formation!="" & analyzed_sites$rock_no==0],sort(unique(analyzed_sites$formation[analyzed_sites$formation!="" & analyzed_sites$rock_no==0]))),breaks=0:length(unique(analyzed_sites$formation[analyzed_sites$formation!="" & analyzed_sites$rock_no==0])),plot=FALSE)$counts);
missing_rocks <- missing_rocks[order(-missing_rocks$sites),];
mrocks <- nrow(missing_rocks);

boundary_crossers <- (1:notu)[otu_info$boundary_crosser];
absent_whales <- otu_info[otu_info$nfinds==0 & !otu_info$extant,];
write.csv(absent_whales,"Absent_Whales.csv",row.names = FALSE);
#pbdb_finds[pbdb_finds$accepted_name %in% "Tlaxcallicetus sp MU EcSj5-18-95",]

analyzed_finds$ma_lb <- pbdb_sites$ma_lb[match(analyzed_finds$collection_no,pbdb_sites$collection_no)];
analyzed_finds$ma_ub <- pbdb_sites$ma_ub[match(analyzed_finds$collection_no,pbdb_sites$collection_no)];
sum(analyzed_finds$created<"2022-01-01 00:00:00")
write.csv(analyzed_finds,"Relevant_Cetacean_Finds.csv",row.names = FALSE);

#otu_info$nfinds[otu_info$nfinds==0 & !otu_info$extant] <- -1;
otu_occurrence_counts <- hist(abs(otu_info$nfinds),breaks=0:max(otu_info$nfinds),plot=FALSE)$counts;
mxfnd <- max(otu_info$nfinds);
otu_info <- otu_info[order(-otu_info$nfinds),];
mxy <- ceiling(max(otu_info$nfinds)/5)*5;
mny <- 1;
mxx <- ceiling(sum(otu_info$nfinds<100)/10)*10;
mnx <- 0;
specify_basic_plot(mxx=mxx,mnx=mnx,mxy=log10(mxy),mny=log10(mny),abscissa = "Taxon Ranked by Finds",ordinate="Total PBDB Occurrences",font=franky,xsize=4.5,cexlab = 1.25);
xbreaks <- as.numeric(set_axis_breaks_new(mxx));
specified_axis(axe=1,max_val=mxx,min_val=0,maj_break=xbreaks[1],med_break=xbreaks[2],min_break=xbreaks[3],font=franky);
numbers <- c(1,10*(1:(mxy/10)));
med_ticks <- seq(5,mxy,by=5)[!seq(5,mxy,by=5) %in% numbers];
min_ticks <- seq(1,mxy,by=1)[!seq(1,mxy,by=1) %in% c(numbers,med_ticks)];
log10_axes(axe=2,min_ax=log10(mny),max_ax=log10(mxy),numbers=numbers,med_ticks=med_ticks,min_ticks=min_ticks,orient=2,font=franky,text_pos=0.75);

spc_colors <- rep(halloween_colors[2],notu);
spc_colors[otu_info$extant] <- halloween_colors[1];
sotu <- sum(otu_info$nfinds>0);
points(1:sotu,log10(otu_info$nfinds[1:sotu]),pch=21,bg=spc_colors[1:sotu],col=spc_colors[1:sotu],lwd=0.25,cex=0.5);
points(400,log10(60),pch=21,,bg=halloween_colors[1],col=halloween_colors[1],lwd=0.25);
text(400,log10(60),": Coded",pos=4,family=franky);
points(400,log10(45),pch=21,,bg=halloween_colors[2],col=halloween_colors[2],lwd=0.25);
text(400,log10(45),": Extinct",pos=4,family=franky);

range_counts <- hist(otu_info$min_range,breaks=-1:ceiling(max(otu_info$min_range)),plot=FALSE)$counts;
range_counts_extant <- hist(otu_info$min_range[otu_info$extant],breaks=-1:ceiling(max(otu_info$min_range)),plot=FALSE)$counts;
range_counts_extinct <- hist(otu_info$min_range[!otu_info$extant],breaks=-1:ceiling(max(otu_info$min_range)),plot=FALSE)$counts;
mxy <- ceiling(max(range_counts_extinct)/50)*50;
no_ranges <- range_counts[1] ;
no_ranges_extinct <- range_counts_extinct[1] ;
no_ranges_extant <- range_counts_extant[1] ;
range_counts[1] <- sum(otu_info$fa_ub==otu_info$la_lb & otu_info$fa_lb>0)
range_counts_extant[1] <- sum(otu_info$fa_ub[otu_info$extant]==otu_info$la_lb[otu_info$extant] & otu_info$fa_lb[otu_info$extant]>0);
range_counts_extinct[1] <- sum(otu_info$fa_ub[!otu_info$extant]==otu_info$la_lb[!otu_info$extant] & otu_info$fa_lb[!otu_info$extant]>0);
range_counts_extinct_plot <- range_counts_extinct+range_counts_extant;
#otu_info[otu_info$min_range %in% max(otu_info$min_range),]
mny <- 0.1;
mxx <- 1+ceiling(max(otu_info$min_range));
mnx <- 0;
specify_basic_plot(mxx=mxx,mnx=mnx,mxy=log10(mxy),mny=log10(mny),abscissa = "Minimum Range (myr)",ordinate="No. Species",font=franky,xsize=3,cexlab=1.25);
xbreaks <- as.numeric(set_axis_breaks_new(mxx));
xbreaks[xbreaks<1] <- 1;
specified_axis(axe=1,max_val=mxx,min_val=0,maj_break=xbreaks[1],med_break=xbreaks[2],min_break=xbreaks[3],font=franky);
numbers <- unique(c(1,c(1,5)*10,100*(1:(mxy/100))));
med_ticks <- c(5,seq(50,mxy,by=50)); med_ticks <- med_ticks[!med_ticks %in% numbers];
min_ticks <- c(1:10,10*(2:10)); min_ticks <- min_ticks[!min_ticks %in% c(med_ticks,numbers)];
log10_axes(axe=2,min_ax=log10(mny),max_ax=log10(mxy),numbers=numbers,med_ticks=med_ticks,min_ticks=min_ticks,orient=2,font=franky,font_size = 11/12)

rect(-0.5,log10(mny),0,log10(no_ranges_extinct),col=makeTransparent(halloween_colors[2]));
rect(0,log10(mny),0.5,log10(no_ranges_extant),col=makeTransparent(halloween_colors[1]));
for (i in 1:length(range_counts))	{
	if (range_counts_extinct[i]>0)	{
		rect(i-1.5,log10(mny),i-1,log10(range_counts_extinct[i]),col=halloween_colors[2]);
		} else if (range_counts_extant[i]>0)	{
		rect(i-1.5,log10(mny),i-1,log10(mny),col="black");
		}
	if (range_counts_extant[i]>0)	{
		rect(i-1.0,log10(mny),i-0.5,log10(range_counts_extant[i]),col=halloween_colors[1]);
		} else if (range_counts_extinct[i]>0)	{
		rect(i-1.0,log10(mny),i-0.5,log10(mny),col="black");
		}
	}
points(9,log10(400),pch=22,,bg=halloween_colors[1],cex=2);
text(9,log10(400),": Extant",pos=4,family=franky);
points(9,log10(200),pch=22,,bg=halloween_colors[2],cex=2);
text(9,log10(200),": Extinct",pos=4,family=franky);

##### Sampling controls ####
stage_slices <- stage_slices[abs(stage_slices$ma_lb)<=abs(time_scale$ma_lb[time_scale$interval=="Cenozoic"]),];
hierarchical_chronostrat <- accersi_hierarchical_timescale(chronostrat_units=stage_slices$interval,time_scale=stage_slices,regional_scale="Stage Slice",ma_fuzz=10);

marine_sites <- pbdb_sites[pbdb_sites$environment %in% marine_environments,];
marine_sites <- marine_sites[marine_sites$ma_lb<=time_scale$ma_lb[time_scale$interval=="Cenozoic"],];
marine_finds <- pbdb_finds[pbdb_finds$collection_no %in% marine_sites$collection_no,];
marine_finds <- marine_finds[marine_finds$phylum %in% c("Chordata","Vertebrata"),];
marine_finds <- marine_finds[marine_finds$identified_rank %in% c("species","subspecies") | marine_finds$accepted_rank %in% c("species","subspecies"),];
marine_sites <- marine_sites[marine_sites$collection_no %in% marine_finds$collection_no,];
marine_sites <- marine_sites[order(-marine_sites$ma_lb),];
marine_sites <- marine_sites[marine_sites$ma_lb>0.0117,];
marine_finds <- marine_finds[marine_finds$collection_no %in% marine_sites$collection_no,];
marine_sites_rockless <- marine_sites[marine_sites$rock_no==0 & !marine_sites$formation %in% "",];
marine_sites_rockless <- marine_sites_rockless[order(-marine_sites_rockless$ma_lb),]
par(pin=c(3,3));
missing_rocks_vert <- data.frame(formation=unique(marine_sites_rockless$formation),
								 cases=hist(match(marine_sites_rockless$formation,unique(marine_sites_rockless$formation)),breaks=0:length(unique(marine_sites_rockless$formation)),plot=FALSE)$counts);
missing_rocks_vert <- missing_rocks_vert[order(-missing_rocks_vert$cases),];
missing_rocks_vert[6:10,]
#unique(marine_sites_rockless$formation)[11:20]
#length(unique(marine_sites_rockless$formation))
finds_per_stage_slice_marine <- tally_occurrences_per_subinterval(taxon_collections=marine_sites,taxon_finds=marine_finds,hierarchical_chronostrat = hierarchical_chronostrat);
sites_per_stage_slice_marine <- tally_collections_occupied_by_subinterval(taxon_collections=marine_sites,hierarchical_chronostrat = hierarchical_chronostrat);
rocks_per_stage_slice_marine <- tally_rock_units_occupied_by_subinterval(taxon_collections=marine_sites,hierarchical_chronostrat = hierarchical_chronostrat);

#unique(marine_sites_rockless$formation)[1:10]
cetacea_sites$collection_no[cetacea_sites$ma_lb==max(cetacea_sites$ma_lb)]
pbdb_taxonomy$accepted_rank[pbdb_taxonomy$taxon_name=="Crocodylia"] <- "order";
control_taxa <- c("Crocodylia","Sirenia","Cetacea","Desmostylia","Pinnipedia");
control_taxon_ranks <- pbdb_taxonomy$accepted_rank[match(control_taxa,pbdb_taxonomy$taxon_name)];
control_finds <- pbdb_finds[1,];
control_finds <- control_finds[control_finds$occurrence_no<0,];
control_sites <- pbdb_sites[1,];
control_sites <- control_sites[control_sites$collection_no<0,];
for (ct in 1:length(control_taxon_ranks))	{
	if (control_taxon_ranks[ct] %in% colnames(pbdb_finds))	{
		group_finds <- pbdb_finds[pbdb_finds[,control_taxon_ranks[ct]] %in% control_taxa[ct],];
		group_finds <- group_finds[group_finds$identified_rank %in% c("species","subspecies") | group_finds$accepted_rank %in% c("species","subspecies"),];
		group_sites <- pbdb_sites[pbdb_sites$collection_no %in% group_finds$collection_no,]
		} else	{
		control_daughters <- accersi_daughter_taxa_from_taxon_no(parent_taxon_no=pbdb_taxonomy$accepted_no[match(control_taxa[ct],pbdb_taxonomy$taxon_name)],pbdb_taxonomy);
		group_finds <- pbdb_finds[1,];
		group_sites <- pbdb_sites[1,];
		group_finds <- group_finds[group_finds$occurrence_no<0,];
		group_sites <- group_sites[group_sites$collection_no<0,];
		for (cd in 1:nrow(control_daughters))	{
			daughter_finds <- pbdb_finds[pbdb_finds[,control_daughters$daughter_rank[cd]] %in% control_daughters$daughter[cd],];
			group_finds <- rbind(group_finds,daughter_finds[daughter_finds$identified_rank %in% c("species","subspecies") | daughter_finds$accepted_rank %in% c("species","subspecies"),]);
			group_sites <- unique(rbind(group_sites,pbdb_sites[pbdb_sites$collection_no %in% group_finds$collection_no,]));
			}
		}
	group_sites <- group_sites[group_sites$environment %in% marine_environments,];
	group_finds <- group_finds[group_finds$collection_no %in% group_sites$collection_no,];

	control_finds <- rbind(control_finds,group_finds);
	control_sites <- unique(rbind(control_sites,group_sites));
	}

control_sites <- control_sites[control_sites$ma_lb<=max(abs(stage_slices$ma_lb)),];
control_sites <- control_sites[control_sites$ma_lb>0.0117,];
control_finds <- control_finds[control_finds$collection_no %in% control_sites$collection_no,];

finds_per_stage_slice_controls <- tally_occurrences_per_subinterval(taxon_collections=control_sites,taxon_finds=control_finds,hierarchical_chronostrat = hierarchical_chronostrat);
sites_per_stage_slice_controls <- tally_collections_occupied_by_subinterval(taxon_collections=control_sites,hierarchical_chronostrat = hierarchical_chronostrat);
rocks_per_stage_slice_controls <- tally_rock_units_occupied_by_subinterval(taxon_collections=control_sites,hierarchical_chronostrat = hierarchical_chronostrat);

cetacea_finds <- control_finds[control_finds$order %in% "Cetacea",];
cetacea_sites <- control_sites[control_sites$collection_no %in% cetacea_finds$collection_no,];
add_these <- analyzed_finds[analyzed_finds$collection_no %in% analyzed_sites$collection_no[!analyzed_sites$collection_no %in% cetacea_sites$collection_no],]
add_these <- add_these[match(unique(add_these$occurrence_no),add_these$occurrence_no),]
add_these <- add_these[,colnames(add_these) %in% colnames(cetacea_finds)];
cetacea_finds <- rbind(cetacea_finds,add_these);
cetacea_sites <- unique(rbind(cetacea_sites,analyzed_sites[analyzed_sites$collection_no %in% add_these$collection_no,]));

finds_per_stage_slice_cetacea <- tally_occurrences_per_subinterval(taxon_collections=cetacea_sites,taxon_finds=cetacea_finds,hierarchical_chronostrat = hierarchical_chronostrat);
sites_per_stage_slice_cetacea <- tally_collections_occupied_by_subinterval(taxon_collections=cetacea_sites,hierarchical_chronostrat = hierarchical_chronostrat);
rocks_per_stage_slice_cetacea <- tally_rock_units_occupied_by_subinterval(taxon_collections=cetacea_sites,hierarchical_chronostrat = hierarchical_chronostrat);

ybreaks <- set_axis_breaks_new(max(sites_per_stage_slice_marine));
mxy <- ceiling(max(sites_per_stage_slice_marine)/min(ybreaks))*min(ybreaks);

stage_slices <- stage_slices[order(-stage_slices$ma_lb),];
time_scale_minor <- stage_scale[abs(stage_scale$ma_ub)<max(abs(stage_slices$ma_lb)),];
time_scale_major <- epoch_scale[abs(epoch_scale$ma_ub)<max(abs(stage_slices$ma_lb)),];

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

