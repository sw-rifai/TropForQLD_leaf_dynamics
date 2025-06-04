#Description: process BoM station data

# imports
pacman::p_load(tidyverse, arrow, data.table, janitor, lubridate, 
               bigleaf)

## list files
list.files("data/met_bom/", 
           recursive = T)

## unpack 
tmp_dir <- tempdir()
path_unpack <- file.path(tmp_dir, "unpacked")
unzip("data/met_bom/DC02D_99999999_10821611-20250604T044636Z-1-001.zip", 
      exdir = path_unpack)

## unpacked file list
flist <- list.files(path_unpack, recursive = T, full.names = T)


#View("DC02D_99999999_10821611/DC02D_Notes_9999999910821611.txt")
# readLines(file.path(path_unpack,"DC02D_99999999_10821611/DC02D_Notes_9999999910821611.txt"))
# readLines(file.path(path_unpack,"DC02D_99999999_10821611/DC02D_StnDet_9999999910821611.txt"))
# read_csv(file.path(path_unpack,"DC02D_99999999_10821611/DC02D_StnDet_9999999910821611.txt"), 
#            skip = 1)


flist_dat <- flist[str_detect(flist, "_Data_")]
# flist_dat <- file.path(path_unpack,dirname(flist)[1], flist_dat)
# file.exists(flist_dat)
# flist_dat[1]

# tmp <- read_csv(file.path(path_unpack,flist_dat[1]))
# fname <- flist_dat[1]
# file.exists(fname)

fn_proc_bom <- function(fname){
  tmp <- read_csv(fname)
  #names(tmp)
  #View(tmp)
  
  tmp <- janitor::clean_names(tmp)
  # head(tmp)
  tmp <- tmp %>% 
    mutate(date = ymd(year_month_day_in_yyyymmdd_format)) %>% 
    rename(Tair_09h = air_temperature_observation_at_09_hours_local_time_in_degrees_c, 
           Tair_15h = air_temperature_observation_at_15_hours_local_time_in_degrees_c, 
           rh_09h = relative_humidity_for_observation_at_09_hours_local_time_in_percentage_percent, 
           rh_15h = relative_humidity_for_observation_at_15_hours_local_time_in_percentage_percent)
  
  out <- tmp %>% 
    mutate(vpd_09h = bigleaf::rH.to.VPD(rH = rh_09h/100, Tair = Tair_09h, 
                                        Esat.formula = "Sonntag_1990"), 
           vpd_15h = bigleaf::rH.to.VPD(rH = rh_15h/100, Tair = Tair_15h, 
                                        Esat.formula = "Sonntag_1990"))
  return(out)  
}
# fn_proc_bom(fname)

l_dat <- flist_dat %>% 
  lapply(., fn_proc_bom)

dat0 <- rbindlist(l_dat)
dat0$vpd_09h %>% summary
dat0$vpd_15h %>% summary


