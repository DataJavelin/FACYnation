## Code to develop a statistical model which describes year to year variance in crop yields
## Crops: wheat, maize, rice and soybean
## See "parameters for crop and data type" for setup options

rm(list=ls())

## calendars for different crops
crop_cal = function(country, crop, ctype) {
  if (crop == "Maize") {
    if (country == "China") {
## April to October
      grow_seas = c("Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct")
## indicator identifying which year the months are in
      grow_seas_id = c(1,1,1,1,1,1,1)
    }

    if (country == "USA") {
## April to October  
      grow_seas = c("Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct")
## indicator identifying which year the months are in
      grow_seas_id = c(1,1,1,1,1,1,1)
    }
## close maize
  }

  if (crop == "Wheat") {
## calendars for "spring" wheat
    if (ctype == "Spring") {
      if (country == "China") {
	grow_seas = c("May", "Jun", "Jul", "Aug", "Sep")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,1)
      }
      if (country == "USA") {
	grow_seas = c("Apr","May", "Jun", "Jul", "Aug", "Sep")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,1,1)
      }
      if (country == "Canada") {
	grow_seas = c("Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,1,1,1,1)
      }
      if (country == "Argentina") {
	grow_seas = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,1,1,1,1,2)
      }
      if (country == "Australia") {
	grow_seas = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,1,1,1,1,1,2,2)
      }
    }

## calendars for "winter" wheat
    if (ctype == "Winter") {
      if (country == "UK") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2,2,2)
      }
      if (country == "China") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2)
      }
      if (country == "India") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2)
      }
      if (country == "USA") {
	grow_seas = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,2,2,2,2,2,2,2,2)
      }  
      if (country == "Germany") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2,2,2)
      }
      if (country == "France") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2,2,2)
      }
    }

## countries which we assume are winter wheat
      if (country == "Belgium") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2,2,2)
      }
    
      if (country == "Netherlands") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2,2,2)
      }

      if (country == "Russia") {
	grow_seas = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,2,2,2,2,2,2,2,2)
      }

      if (country == "Poland") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2,2,2)
      }

      if (country == "Denmark") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2,2,2)
      }

      if (country == "Ireland") {
	grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,2,2,2,2,2,2,2,2)
      }
## close wheat
  }

  if (crop == "Soybean") {
    if (country == "China") {
      grow_seas = c("Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct")
## indicator identifying which year the months are in
      grow_seas_id =  c(1,1,1,1,1,1,1)
    }

    if (country == "USA") { 
      grow_seas = c("May", "Jun", "Jul", "Aug", "Sep", "Oct")
## indicator identifying which year the months are in
      grow_seas_id = c(1,1,1,1,1,1)
    }

    if (country == "Argentina") { 
      grow_seas = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")
## indicator identifying which year the months are in
      grow_seas_id = c(1,1,1,2,2,2,2,2,2)
    }

    if (country == "Brazil") { 
      grow_seas = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")
## indicator identifying which year the months are in
      grow_seas_id = c(1,1,1,1,2,2,2,2,2)
    }
## close soybean
  }

  if (crop == "Rice") {
    if (ctype == "Early") {
      if (country == "China") {
	grow_seas = c("Feb","Mar", "Apr", "May", "Jun", "Jul")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,1,1)
      }
    }
    if (ctype == "Mid") {
      if (country == "China") {
	grow_seas = c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,1,1,1,1)
      }
    }
    if (ctype == "Late") {
      if (country == "China") {
	grow_seas = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
## indicator identifying which year the months are in
	grow_seas_id = c(1,1,1,1,1,1)
      }
    }
## close rice
  }

## return the growing season and growing season id (i.e. which month is in which year)
  A = c()
  A$grow_seas = grow_seas
  A$grow_seas_id = grow_seas_id
  return(A)
}

## function to return the country of a province/state/region
prov_country = function(province) {

  china_regs = c("Jilin", "Liaoning", "Heilongjiang", "Hebei", "Shanxi", "Shandong", "Henan", "Shaanxi",
  "Jiangsu", "Anhui", "Hubei", "Jiangxi", "Guangxi", "Hunan", "Guangdong", "Zhejiang", "Fujian", "Sichuan", 
  "Chongqing", "Yunnan", "Guizhou")
  attr(china_regs, "cntry") = "China"

  us_regs = c("Kansas", "Nebraska", "Colorado", "Ohio", "South Dakota", "Washington", "Idaho", "Montana", 
  "Oklahoma", "Texas", "North Dakota", "Minnesota", "Iowa", 
  "Illinois", "Indiana", "Missouri")
  attr(us_regs, "cntry") = "USA"

  india_regs = c("Haryana", "Madhya Pradesh", "Punjab", "Rajasthan", "Uttar Pradesh")
  attr(india_regs, "cntry") = "India"

  aus_regs = c("Western Australia", "New South Wales", "South Australia", "Victoria")
  attr(aus_regs, "cntry") = "Australia"

  can_regs = c("Manitoba", "Saskatchewan", "Alberta")
  attr(can_regs, "cntry") = "Canada"

  arg_regs = c("Buenos Aires", "Cordoba", "Santa Fe")
  attr(arg_regs, "cntry") = "Argentina"

  brz_regs = c("Mato Grosso", "Goias", "Mato Grosso do Sul", "Parana", "Rio Grande do Sul")
  attr(brz_regs, "cntry") = "Brazil"

  uk_regs = c("United Kingdom")
  attr(uk_regs, "cntry") = "United Kingdom"

  fra_regs = c("France")
  attr(fra_regs, "cntry") = "France"

  ger_regs = c("Germany")
  attr(ger_regs, "cntry") = "Germany"

  bel_regs = c("Belgium")
  attr(bel_regs, "cntry") = "Belgium"

  den_regs = c("Denmark")
  attr(den_regs, "cntry") = "Denmark"

  ned_regs = c("Netherlands")
  attr(ned_regs, "cntry") = "Netherlands"

  rus_regs = c("Russia")
  attr(rus_regs, "cntry") = "Russia"

  ire_regs = c("Ireland")
  attr(ire_regs, "cntry") = "Ireland"

  pol_regs = c("Poland")
  attr(pol_regs, "cntry") = "Poland"

  all_regs = c(china_regs, us_regs, india_regs, aus_regs, can_regs, arg_regs, brz_regs, uk_regs, fra_regs, ger_regs, bel_regs, den_regs, ned_regs, rus_regs, ire_regs, pol_regs)
  all_cntrys = c(rep(attributes(china_regs)$cntry, length(china_regs)), rep(attributes(us_regs)$cntry, length(us_regs)), 
  rep(attributes(india_regs)$cntry, length(india_regs)),rep(attributes(aus_regs)$cntry,length(aus_regs)), 
  rep(attributes(can_regs)$cntry,length(can_regs)), rep(attributes(arg_regs)$cntry,length(arg_regs)), 
  rep(attributes(brz_regs)$cntry,length(brz_regs)), rep(attributes(uk_regs)$cntry,length(uk_regs)), 
  rep(attributes(fra_regs)$cntry,length(fra_regs)), rep(attributes(ger_regs)$cntry,length(ger_regs)), rep(attributes(bel_regs)$cntry,length(bel_regs)),
  rep(attributes(den_regs)$cntry,length(den_regs)), rep(attributes(ned_regs)$cntry,length(ned_regs)), rep(attributes(rus_regs)$cntry,length(rus_regs)),
  rep(attributes(ire_regs)$cntry,length(ire_regs)), rep(attributes(pol_regs)$cntry,length(pol_regs)))
  country_name = all_cntrys[which(all_regs == province)]
#  browser()
  return(country_name)
}

## module to get either absolute or fractional yield anomalies
get_data = function(crop, fraction, averaging_method) {
  print(crop)
  if (fraction == 1) {
    data = read.csv(paste("frac_",crop,"_",averaging_method,"_yield_anoms.csv", sep=""), header = TRUE,sep = "\t", stringsAsFactors = FALSE)
  } 
  if (fraction == 0) {
    data = read.csv(paste(crop,"_",averaging_method, "_yield_anoms.csv", sep=""), header = TRUE,sep = "\t", stringsAsFactors = FALSE)
  }
#  browser()
  return(data)
}

## module to reformat the region names
region_name_format = function(pattern_match, regionnames){
## find the indices of the region name list which correspond to your required regions
  out = grepl(pattern_match, regionnames)
  region_fullnames = regionnames[which(as.list(out) == "TRUE")]
  print(region_fullnames)
  substrings = strsplit(region_fullnames, "_")

  region_full = character(0)
  for (i in 1:length(region_fullnames)) {
    subs = unlist(substrings[i])
    if (subs[length(subs)] == "All") {
      region_full[i] =  subs[length(subs)-1]
    } else {
      region_full[i] = (subs[length(subs)])
    }
  }

## Now split the region names into words and replace the space with an underscore
  regions_f = gsub('([[:upper:]])', ' \\1', region_full)
  regions_out = substring(regions_f, 2, nchar(regions_f))
#regions = gsub(" ", "_", substring(regions_f, 2, nchar(regions_f)))  
  A = c()
  A$indices = out
  A$region_fullnames = region_fullnames
  A$regions = regions_out
  return(A)
}

##================================================================
## parameters for crop and data type
##================================================================

## read yield anom data
## crop type (e.g. wheat, maize, soybean)
crop = "Maize"
## if wheat, we need to specify whether it's spring or winter
crop_type = "Spring"
## options are: "Winter (Wheat)", "Spring (Wheat, Maize, Soybean)", "Summer (Maize)", "Early (Rice)", "Mid (Rice)", "Late (Rice)", "NULL" (wheat - specified national values)
## Or specify particular regions
regs = "NULL"
## use rolling median or mean
ave_method = "median"
## use fractional anomalies (=1, absolute=0)
use_frac = 0
## indicator for missing values
empty_val = -999
## Number of times to resample observations (removing one year with replacement)
smpl = 100
## percentiles for monthly sensitivities, to be plotted
sens_pciles = c(0.16, 0.5, 0.84)
## empirical yield threhsolds
emp_thrs = c()
emp_thrs$thresholds = c(-0.3, -0.3, -1.0, NA)
emp_thrs$crop = c("Wheat", "Soybean", "Maize", "Rice")

file_dir = "~epope/climate_security/python_code/HCCP/hccp_output/test/"
out_dir = "~epope/climate_security/python_code/HCCP/hccp_output/test/"

##=================================================================
## Main code
##=================================================================

## Read in the climatology files
p_file = paste('precip_climatology_', crop,'.csv', sep="")
t_file = paste('temp_climatology_', crop,'.csv', sep="")
dir = "/net/home/h06/epope/climate_security/python_code/HCCP/hccp_output/test/"
t_clim_data = read.csv(paste(dir,t_file, sep=""), header = TRUE,sep = "\t", stringsAsFactors = FALSE)
p_clim_data = read.csv(paste(dir,p_file, sep=""), header = TRUE,sep = "\t", stringsAsFactors = FALSE)

t_clim = t_clim_data[,2:13]
p_clim = p_clim_data[,2:13]

## read in region name file
reg_name_file = paste('/net/home/h06/epope/climate_security/python_code/HCCP/hccp_output/test/', 'region_names.csv', sep="")
regnamelist = read.csv(reg_name_file, header = TRUE,sep = "\t", stringsAsFactors = FALSE)[2]

## make sure the winter wheat regions are correctly categorised in both yield and climate files
cn_list = c("Denmark", "Ireland", "Netherlands", "Belgium", "Poland", "Russia")
for (k in cn_list) {
  regnamelist[which(regnamelist[,1] == paste("Wheat_",k,"_All",sep="")),1] = paste("Wheat_Winter_",k,"_All",sep="")
  t_clim_data$X[which(t_clim_data$X == paste("Wheat_",k,"_All",sep=""))] = paste("Wheat_Winter_",k,"_All",sep="")
}

if (all(regs == "NULL")) {
## do the pattern search to find the right regions 
  if (crop == "Soybean") {
    pattern = paste(crop, sep="")
  } else {
    pattern = paste(crop,".*",crop_type, sep="")
  }
## reformat region names
  reglist = region_name_format(pattern, regnamelist$Name)
  regions = reglist$regions
  region_fullnames = reglist$region_fullnames

  climreglist = region_name_format(pattern, t_clim_data$X)
  clim_regions = climreglist$regions
  clim_region_fullnames = climreglist$region_fullnames
  clim_regions_inds = climreglist$indices
} else {
  region_fullnames = regs
  regions = character(0)
  for (s in 1:length(regs)) {
    regions[s] = unlist(strsplit(region_fullnames, "_")[s])[2]
  }
  clim_region_fullnames = regs
  clim_regions = regions
}

## special cases
if (any(regions == "U K")) {
  regions[which(regions == "U K")] = "United Kingdom"
}

if (any(clim_regions == "U K")) {
  clim_regions[which(clim_regions == "U K")] = "United Kingdom"
}

if (any(regions == "Rio Grandedo Sul")) {
  regions[which(regions == "Rio Grandedo Sul")] = "Rio Grande do Sul"
}

if (any(clim_regions == "Rio Grandedo Sul")) {
  clim_regions[which(clim_regions == "Rio Grandedo Sul")] = "Rio Grande do Sul"
}

if (any(regions == "Mato Grossodo Sul")) {
  regions[which(regions == "Mato Grossodo Sul")] = "Mato Grosso do Sul"
}

if (any(clim_regions == "Mato Grossodo Sul")) {
  clim_regions[which(clim_regions == "Mato Grossodo Sul")] = "Mato Grosso do Sul"
}

if (crop == "Rice") {
  regions = paste(regions, '-', crop_type, " Rice", sep="") 
}

print(regions)
print("==============================================")
  cat("Crop type = ", crop_type, "-", crop, "\n")

if (use_frac == 0) {
  abs_frac = "absolute"
} else {
  abs_frac = "fractional"
}
cat("Using", abs_frac, "yield anomalies", "\n")
print("==============================================")

##======================================================================
## Select the regions and extract the necessary bits of the yield data
##======================================================================
if (crop == "Wheat") {
  wheat_types = c("Winter", "Spring")
  if (all(wheat_types != crop_type)) {
    print("Error - crop type undefined: must be either winter or spring")
    browser()
  }
  skipc = 3
## get the data
  yield_data = get_data(crop, use_frac, ave_method)
  inds = skipc:length(colnames(yield_data))
## get the appropriate subset
  yield_anoms = yield_data[,inds]
  for (k in cn_list) {
    yield_data$Region[which(yield_data$Region == paste("Wheat_",k,"_All",sep=""))] = paste("Wheat_Winter_",k,"_All",sep="")
  }
  y_name_list = yield_data$Region
  rownames(yield_anoms) = y_name_list
## generate a file name
  stats_fname = paste(crop_type,"_wheat_output_stats_new.csv", sep="")
## this variable contains the names of yield anomalies which are national
  countries = NULL
}

if (crop == "Maize") {
  if ((crop_type != "Spring") & (crop_type != "Summer")) {
    print("Error - no crop subtypes to be defined")
    browser()
  }
  skipc = 3
  yield_data = get_data(crop, use_frac, ave_method)
  inds = skipc:length(colnames(yield_data))
  yield_anoms = yield_data[,inds]
  y_name_list = yield_data$Region
  rownames(yield_anoms) = yield_data$Region
  stats_fname = paste(crop_type, "_maize_output_stats_new.csv", sep="")
  countries = NULL
}

if (crop == "Soybean") {
  if (crop_type != "Spring") {
    print("Error - no crop subtypes to be defined")
    browser()
  }
  skipc = 3
  yield_data = get_data(crop, use_frac, ave_method)
  inds = skipc:length(colnames(yield_data))
  yield_anoms = yield_data[,inds]
  y_name_list = yield_data$Region
  rownames(yield_anoms) = yield_data$Region
  stats_fname = paste(crop_type, "_soybean_output_stats_new.csv", sep="")
  countries = NULL
}

if (crop == "Rice") {
  rice_types = c("Early", "Mid", "Late")
  if (all(rice_types != crop_type)) {
    print("Error - crop type undefined: must be early, mid or late")
    browser()
  }
  skipc = 3
  yield_data = get_data(crop, use_frac, ave_method)
  inds = skipc:length(colnames(yield_data))
  yield_anoms = yield_data[,inds]
  y_name_list = yield_data$Region
  rownames(yield_anoms) = yield_data$Region
  stats_fname = paste(crop_type, "_rice_output_stats_new.csv", sep="")
  countries = NULL
}

years_sub = substring(colnames(yield_anoms), 2)
colnames(yield_anoms) = years_sub

## subset provinces according to the regions/provinces/states of interest and the yield values which are real 
prov_ids = match(y_name_list, region_fullnames)
real_prov_ids = which(!is.na(prov_ids))
province_subset = y_name_list[real_prov_ids]

i_max = length(province_subset)

all_mons = c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

## WATCH data 1980:2014; therefore, we are interested in yields from 1980-2014
##======================================================================
## Loop through selected regions
##======================================================================
for (i in 1:i_max) {
  prv = strsplit(province_subset[i], "_")
  country = unlist(prv)[length(unlist(prv))-1]
  print(country)

  if (unlist(prv)[length(unlist(prv))] == "All") {
    prov = unlist(prv)[length(unlist(prv))-1]
  } else {
    prov = unlist(prv)[length(unlist(prv))]
  }
  print(prov)

  rind = which(region_fullnames == province_subset[i])
## find which the index of the temperature climatology file the current province matches 
  rind_clim2 = which(t_clim_data$X == province_subset[i])   
## get crop calendar
  grow_seas_info = crop_cal(country, crop, crop_type)
  grow_seas = grow_seas_info$grow_seas
  grow_seas_id = grow_seas_info$grow_seas_id
## derive necessary quantities from crop calendar
  nmonths = length(grow_seas)
  ypts = 0:(length(grow_seas)-1)
  xmin = -1.0*length(grow_seas)
  xmax = 1.0*length(grow_seas)
  print(grow_seas)

## open the appropriate temperature/precipitation files
  file_name_temp = paste(region_fullnames[rind],"_temp_anom_real.csv", sep="")
  file_name_precip = paste(region_fullnames[rind],"_precip_anom_real.csv", sep="")
  filename_t = paste(region_fullnames[rind],"_temp_sens.png", sep="")
  filename_p = paste(region_fullnames[rind],"_precip_sens.png", sep="")
  filename_corr = paste(region_fullnames[rind],"_corr.png", sep="")
  filename_ts = paste(region_fullnames[rind],"_ts.png", sep="")

  fn_temp = paste(file_dir, file_name_temp, sep="")
  fn_precip = paste(file_dir, file_name_precip, sep="")

  temp_data = read.csv(fn_temp, header = TRUE,sep = "\t")
  precip_data = read.csv(fn_precip, header = TRUE,sep = "\t")
##=======================================================================================================
## data tests
##=======================================================================================================
## check this is the same regions as originally specified
  if (rownames(yield_anoms)[real_prov_ids[i]] != province_subset[i]){
    print("Error/mismatch - different regions specified")
    browser()
  }

## check that data is the same, so that there hasn't been an error with assigning the names
  delta = yield_anoms[real_prov_ids[i],]-yield_anoms[which(y_name_list == province_subset[i]),]
  if (any(delta[!is.na(delta)] != 0) ) {
    print("Error - different data specified")
    browser()
  }

##=======================================================================================================
## Get yield data for specified years.
## Need to be careful, since winter wheat is sown in year x and harvested in year x+1
##=======================================================================================================
## The year refers to the year in which the yield is taken
  yanom_years = as.numeric(colnames(yield_anoms))
## find first/last year where observed yield anoms have a real value (i.e. the years for which we have data)!  
  frst_yr = yanom_years[min(which(!is.na(yield_anoms[real_prov_ids[i],])))]
  lst_yr = yanom_years[max(which(!is.na(yield_anoms[real_prov_ids[i],])))]

## use these to create a sequence of harvest years
  harvest_years = frst_yr:lst_yr
## create the sowing years (which depend on crop type and geographical location)
  if (any(grow_seas_id == 2)) {
    sow_years = harvest_years - 1
  } else {
    sow_years = harvest_years
  }

## find years for which we have temperature/precipitation data
  attributes(temp_data)$names[1] = 'year'
  metdata_years = temp_data$year

## overlap between the two time series
  min_years = max(min(metdata_years),min(sow_years))
  max_years = min(max(metdata_years),max(harvest_years))

## years for which we require Met data
  met_years = min_years:max_years
  n_metyears = length(met_years)

## years for which we require yield data
  if (any(grow_seas_id == 2)) {
    years = (min_years+1):max_years
  } else {
    years = min_years:max_years
  }
  nyears = length(years)

## get the yield anomalies for these years
  start_yr = which(yanom_years == min(years))
  end_year = which(yanom_years == max(years))

  y_anom_subset = as.numeric(yield_anoms[real_prov_ids[i],start_yr:end_year])

## generate arrays as we loop through each region
  score_precip = array(NA, c(nyears, nmonths) )
  colnames(score_precip) = grow_seas  
  rownames(score_precip) = years 
  score_temp = array(NA, c(nyears, nmonths) )
  colnames(score_temp) = grow_seas  
  rownames(score_temp) = years 
  
  score_temp_clim_mean = array(0, nmonths)
  score_temp_clim_sd = array(0, nmonths)
  score_precip_clim_mean = array(0, nmonths)
  score_precip_clim_sd = array(0, nmonths)

  sens_precip = array(NA, c(smpl, nmonths) )
  sens_temp = array(NA, c(smpl, nmonths) ) 
  sens_precip_pcile = array(NA, c(length(sens_pciles), nmonths) )
  sens_temp_pcile = array(NA, c(length(sens_pciles), nmonths) )

  temp_mon_sd = array(NA, c(nmonths) )
  precip_mon_sd = array(NA, c(nmonths) )

## store the simulated yields (one year is removed)
  sim_yield_t_all = array(0, c(smpl, (nyears-1)))
  sim_yield_p_all = array(0, c(smpl, (nyears-1) ))
  sim_yield_t = array(0, c(nyears))
  sim_yield_p = array(0, c(nyears))

  cor_t = array(0, c(smpl)) 
  cor_p = array(0, c(smpl))
  cor_tp = array(0, c(smpl))
  tp_cor = array(0, c(nmonths))

## calculate the accumulation of anomalies throughout the growing season for each year
  for (y in 1:nyears) {
    yind = which(temp_data$year == years[y])
## loop through months
    for (mon in 1:nmonths) {
## get the actual month name
      month = grow_seas[mon]
## turn this into an index access the right data from the #_data arrays 
      mon_ind = which(attributes(temp_data)$names == month)
      clim_mon_ind = which(attributes(t_clim_data)$names == month)
## determine whether we are in the first or second year of the growing season
## check to see whether the growing season crosses more than one year
      if (any(grow_seas_id == 2)) {
## if yes, then set adjuster to be previous year or current year as appropriate according to the current month of the growing season
	if (grow_seas_id[mon] == 1) {
	  ct = -1
	} else if (grow_seas_id[mon] == 2){
	  ct = 0
	}
      } else {
## if not just set the adjuster to 0
	ct = 0
      }
## store the anomalies in the score_# arrays
      score_temp[y,mon] = temp_data[(yind+ct),mon_ind]
      score_precip[y,mon] = precip_data[(yind+ct),mon_ind]
    }
  }
  
## now calculate the standard deviations
  for (mon in 1:nmonths) {
    score_temp_clim_sd[mon] = sd(score_temp[,mon])
    score_precip_clim_sd[mon] = sd(score_precip[,mon])
  }

## Derive true sensitivity. Also sub-sample years to assess the uncertainty in the sensitivity
  y_inds = which(is.finite(y_anom_subset))
  y_new = y_anom_subset[y_inds]
## remove one year at a time, with replacement
  for (n in 1:smpl) {
    y_smpl_inds = sample(y_inds, (length(y_inds)-1), replace = TRUE)
    y_smpl = y_anom_subset[y_smpl_inds]

    t_mat = score_temp[y_smpl_inds,]   
    p_mat = score_precip[y_smpl_inds,]
    
## multilinear regression method (but also set the intercept to be zero!!!! (The . means include all predictors that have not been mentioned so far))
    p_fit2 <- lm(y_smpl ~ 0 + ., data = data.frame(p_mat))
    p_summary = summary(p_fit2)
    t_fit2 <- lm(y_smpl ~ 0 + ., data = data.frame(t_mat))
    t_summary = summary(t_fit2)
##
    sens_temp[n,] = t_fit2$coefficients
    sens_precip[n,] = p_fit2$coefficients

## estimate the yields for all of the different sensitivities
    for (y in 1:length(y_smpl_inds)) {
      sim_yield_t_all[n,y] = sum(score_temp[y_smpl_inds[y],]*sens_temp[n,])
      sim_yield_p_all[n,y] = sum(score_precip[y_smpl_inds[y],]*sens_precip[n,])
    }

## derive the correlations for all of the different yield predictions
    cor_t[n] = cor(sim_yield_t_all[n,1:length(y_smpl)], y_smpl)
    cor_p[n] = cor(sim_yield_p_all[n,1:length(y_smpl)], y_smpl)
    cor_tp[n] = cor(sim_yield_t_all[n,1:length(y_smpl)], sim_yield_p_all[n,1:length(y_smpl)])
  }

## now get the percentiles of the monthly sensitivities
  for (m in 1:nmonths) {
    sens_temp_pcile[,m] = quantile(sens_temp[,m], sens_pciles, na.rm=T)
    sens_precip_pcile[,m] = quantile(sens_precip[,m], sens_pciles, na.rm=T)
  }
  
  cat("Standard deviation of yield (yield-obs) = ", sd(y_anom_subset), "\n")

## now extract and store the 10th, 50th and 90th percentile of the sensitivity for each month
  png(filename_t, width=7.25,height=7.25,units="in",res=300)
  plot(x=1, y=1, type="n", ylim=c(-1.0, 1.0), xlim=c(1,nmonths), 
  ylab='Sensitivity (tonnes / hectare / K)', xlab='Month', xaxt='n', main=paste('Yield sensitivity to temperature - ', country," (",prov,")", sep=""))
  lines(1:nmonths, sens_temp_pcile[2,], col='black', lwd = 2)
  lines(1:nmonths, sens_temp_pcile[1,], col='blue', lwd = 2, lty=2)
  lines(1:nmonths, sens_temp_pcile[3,], col='blue', lwd = 2, lty=2)
  axis(side = 1, at=1:nmonths, labels = grow_seas)
  dev.off()

  png(filename_p, width=7.25,height=7.25,units="in",res=300)
  plot(x=1, y=1, type="n", ylim=c(-0.05, 0.05), xlim=c(1,nmonths), 
  ylab='Sensitivity (tonnes / hectare / mm)', xlab='Month', xaxt='n', main=paste(crop, ' yield sensitivity to precipitation - ', country," (",prov,")", sep=""))
  lines(1:nmonths, sens_precip_pcile[2,], col='black', lwd = 2)
  lines(1:nmonths, sens_precip_pcile[1,], col='blue', lwd = 2, lty=2)
  lines(1:nmonths, sens_precip_pcile[3,], col='blue', lwd = 2, lty=2)
  axis(side = 1, at=1:nmonths, labels = grow_seas)
  dev.off()

## Using sensitivities and realisations, predict the yield based on observed anomalies to see how it compares to actual yield!
  for (y in 1:nyears) {
    sim_yield_t[y] = sum(score_temp[y,]*sens_temp_pcile[2,])
    sim_yield_p[y] = sum(score_precip[y,]*sens_precip_pcile[2,])   
  }

## rescale the simulated yield and add noise so that it matches the obs
  pcor = cor(y_new, sim_yield_p[y_inds])
  sdy = sd(y_new)
  scale_factor_p = pcor*sdy/sd(sim_yield_p[y_inds])

  sim_yield_pp = scale_factor_p*sim_yield_p[y_inds] 
  y_sig = sd(y_anom_subset[y_inds])

##========================================================================
## Plot correlation and time series
##========================================================================

  png(filename_ts, width=7.25,height=7.25,units="in",res=300)
  plot(x=1, y=1, type="n", ylim=c(min(y_new), max(y_new)), xlim=c(1,nyears), 
  ylab='Yield anomaly', xlab='Year', xaxt = 'n', main=paste('Time series of real and simulated yield anomalies - ', country," (",prov,")", sep="") )
  axis(side = 1, at=1:length(sim_yield_pp), labels = years[y_inds])
  lines(y_inds, y_new[y_inds], col='blue', lwd=3)
  lines(1:length(y_inds), sim_yield_pp, col='black')
  abline(h = emp_thrs$thresholds[which(emp_thrs$crop == crop)], col='red', lty=2)
  if (length(emp_thrs$thresholds[which(emp_thrs$crop == crop)]) != 0) {
    legend('topleft',  c("Observed", "Prediction (precipitation)", "Prediction (combined)", "Shock threshold (indicator)"), lty=c(1,1,2,2), lwd=c(3,1,1,1), col=c("blue", "black", "black","red"), bty='n', cex=1.0)
  } else {
    legend('topleft',  c("Observed", "Prediction (precipitation)", "Prediction (combined)"), lty=c(1,1,2), lwd=c(3,1,1), col=c("blue", "black", "black"), bty='n', cex=1.0)
  }
  dev.off()

  cat("Correlation between simulated (temperature) and observed yield (10th, 50th, 90th %ile) =", quantile(cor_t, sens_pciles, na.rm=T), "\n")
  cat("Correlation between simulated (precipitation) and observed yield (10th, 50th, 90th %ile) =", quantile(cor_p, sens_pciles, na.rm=T), "\n")
  cat("Correlation between simulated (precipitation) and simulated (temperature) yield (10th, 50th, 90th %ile) =", quantile(cor_tp, sens_pciles, na.rm=T), "\n")
##========================================================================
## Print output to csv file
##========================================================================
  sens_out = c()
  A = c()
  A$country = country
  sens_out$ country = country
  if (any(countries == country)) {
    A$region = "National"
    sens_out$region = "National"
  } else {
    A$region = prov
    sens_out$region = prov
  }

  A$tcorr = signif(unname(quantile(cor_t, 0.50, na.rm=T)), 3)
  A$pcorr = signif(unname(quantile(cor_p, 0.50, na.rm=T)), 3)
  A$tpcorr = signif(unname(quantile(cor_tp, 0.50, na.rm=T)), 3)
  A$ysd = y_sig

  mons = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  temp_out = rep(NA, 12)
  precip_out = rep(NA, 12)
  temp_out_scaled = rep(NA, 12)
  precip_out_scaled = rep(NA, 12)
  fr = rep(NA, 12)
  for (m in 1:nmonths) {
    mon = grow_seas[m]
    mon_ind = which(mons == mon)
    temp_out[mon_ind] = sens_temp_pcile[2,m]
    precip_out[mon_ind] = sens_precip_pcile[2,m]
    temp_out_scaled[mon_ind] = sens_temp_pcile[2,m]/(A$tcorr*y_sig)
    precip_out_scaled[mon_ind] = sens_precip_pcile[2,m]/(A$pcorr*y_sig)
#    print(mon)
#    print(mon_ind)
    fr[mon_ind] = as.numeric(m/nmonths)
  }

## Raw sensitivities
  sens_out$temp_sens = temp_out
  sens_out$precip_sens = precip_out
## Months
  sens_out$mon = mons
## sensitivities scaled by yield standard deviation
  sens_out$temp_sens_scaled = temp_out_scaled
  sens_out$precip_sens_scaled = precip_out_scaled
## climatology
  sens_out$temp_bar = unlist(t_clim_data[rind_clim2,2:13])
  sens_out$precip_bar = unlist(p_clim_data[rind_clim2,2:13])
## Fraction through growing season
  sens_out$fr = fr

  output_fname = paste(crop, "_", crop_type,"_",ave_method,"_output_stats_Theo.csv", sep="")
  output_sname = paste(crop, "_", crop_type,"_",ave_method,"_sensitivity_stats_Theo.csv", sep="")
  corr_cols = c("Country", "Region", "T-corr", "P-corr","TP-corr","sd(yield anoms)")
  sens_cols = c("Country", "Region", "Temp_sens", "Precip_sens", "Month", "Temp_sens_scaled", "Precip_sens_scaled", "T_clim", "P_clim","fr")

  if (i == 1) {
    write.table(A, file = output_fname, row.names = FALSE, sep=",", col.names=corr_cols)
    write.table(sens_out, file = output_sname, row.names = FALSE, sep=",", col.names=sens_cols)
  } else {
    write.table(A, file = output_fname, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    write.table(sens_out, file = output_sname, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  }
## close loop over i regions
}

