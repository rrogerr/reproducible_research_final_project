######################### IMPORTS #########################

library(dplyr)
library(ggplot2)

path <- "/home/rogelio/Desktop/datasciencecoursera/"
p <- "reproducible_research_final_project"

path <- paste0(path, p)

setwd(path)

storm_data0 <- read.csv("./repdata%2Fdata%2FStormData.csv.bz2")


########################### DATA ###########################
######################## PROCESSING ########################


human_dmg <- data.frame(evtype = storm_data0$EVTYPE,
                        fatalities = storm_data0$FATALITIES,
                        injuries = storm_data0$INJURIES)
                         
econ_dmg <- data.frame(evtype = storm_data0$EVTYPE,
                       propdmg = storm_data0$PROPDMG,
                       propdmgexp = storm_data0$PROPDMGEXP,
                       cropdmg = storm_data0$CROPDMG,
                       cropdmgexp = storm_data0$CROPDMGEXP)

# add columns for total damage: fat_inj = fatalities + injuries
# and crop_prop = crop damage + property damage
human_dmg <- 
        mutate(human_dmg, fat_inj = fatalities + injuries)


# change orders of magnitude ("h", "K", "M", "B") by numeric
# equivalents (100, 1000, 1000000, 1000000000)
econ_dmg <- mutate(econ_dmg, 
                   propdmgexp = gsub("[Hh]", "100", propdmgexp)) %>%
        mutate(propdmgexp = gsub("[Kk]", "1000", propdmgexp)) %>%
        mutate(propdmgexp = gsub("[Mm]", "1000000", propdmgexp)) %>%
        mutate(propdmgexp = gsub("[Bb]", "1000000000", propdmgexp)) %>%
        mutate(cropdmgexp = gsub("[Hh]", "100", cropdmgexp)) %>%
        mutate(cropdmgexp = gsub("[Kk]", "1000", cropdmgexp)) %>%
        mutate(cropdmgexp = gsub("[Mm]", "1000000", cropdmgexp)) %>%
        mutate(cropdmgexp = gsub("[Bb]", "1000000000", cropdmgexp))


#change orders of magnitude to numeric type
econ_dmg <- 
        mutate(econ_dmg, propdmgexp = as.numeric(propdmgexp)) %>%
        mutate(cropdmgexp = as.numeric(cropdmgexp))

# calculate 
# property and crop damage = significant digits x magnitude
# add column for total damage
econ_dmg <- 
        mutate(econ_dmg, pdmg_total = propdmg * propdmgexp) %>%
        mutate(cdmg_total = cropdmg * cropdmgexp) %>%
        mutate(crop_prop = cdmg_total + pdmg_total)

# Collapse human_dmg and econ_dmg taking the mean
# to see which events on average cause most damages
human_dmg_mean <- 
        aggregate(.~evtype, human_dmg, FUN = mean)

econ_dmg_mean <- 
        aggregate(.~evtype, econ_dmg, FUN = mean)


######################### ANALYSIS #########################
############################ & #############################
######################### RESULTS ##########################


########################## HUMAN ###########################
########################## DAMAGE ##########################

# sort events by human damage
most_h_dmg <- tail(human_dmg_mean[order(human_dmg_mean$fat_inj),], 
                   n = 10)

# if we don't do the following ggplot will not respect the
# order of evtype by fat_inj
most_h_dmg$evtype <- factor(most_h_dmg$evtype,
       levels = most_h_dmg$evtype[order(most_h_dmg$fat_inj)])

ggplot(most_h_dmg, aes(evtype, fat_inj)) + 
        geom_bar(stat = "identity", fill = "red") + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0)) +
        labs(x = "type of event", y = "fatalities + injuries",
             title = "Fatalities plus injuries\nper one event of each type") +
        coord_flip()


######################### ECONOMIC #########################
########################## DAMAGE ##########################

most_econ_dmg_prop <- 
        tail(econ_dmg_mean[order(econ_dmg_mean$pdmg_total),], 
                      n = 10)

most_econ_dmg_crop <- 
        tail(econ_dmg_mean[order(econ_dmg_mean$cdmg_total),], 
                       n = 10)

most_econ_dmg_sum <- 
        tail(econ_dmg_mean[order(econ_dmg_mean$crop_prop),], 
             n = 10)

lab <- rep(c("property", "crops", "property + crops"), each = 10)

econ_data <- 
        data.frame(evtype = c(as.character(most_econ_dmg_prop$evtype),
                              as.character(most_econ_dmg_crop$evtype),
                              as.character(most_econ_dmg_sum$evtype)),
                   damage = c(most_econ_dmg_prop$pdmg_total,
                              most_econ_dmg_crop$cdmg_total,
                              most_econ_dmg_sum$crop_prop),
                   lab = lab)

ggplot(econ_data, aes(evtype, damage)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0)) +
        facet_wrap(~lab, ncol = 3) + 
        labs(x = "type of event", y = "damage [US dollars]",
             title = "economic damage\nper one event of each type") +
        coord_flip()