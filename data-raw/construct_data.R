### process example data for package ###
library(fticRanalysis)

# read in .csv data #
data = read.csv("data-raw/example12T_data.csv")
fdata = read.csv("data-raw/example12T_metadata.csv")
fdata = fdata[,1:4]

edata = data[,1:21]
emeta = data[,c(1,22:28,30,33)]

fticr12T_edata = edata
fticr12T_fdata = fdata
fticr12T_emeta = emeta
save(fticr12T_edata, file = "data/fticr12T_edata.rda")
save(fticr12T_fdata, file = "data/fticr12T_fdata.rda")
save(fticr12T_emeta, file = "data/fticr12T_emeta.rda")

# create an peakIcrData object #
mydata = as.peakIcrData(e_data = edata, f_data = fdata, e_meta = emeta,
                        edata_cname = "Mass", mass_cname = "Mass", fdata_cname = "SampleID",
                        isotopic_cname = "C13", isotopic_notation = "1",
                        c_cname = "C", h_cname = "H", o_cname = "O", 
                        n_cname = "N", s_cname = "S", p_cname = "P",
                        instrument_type = "12T")
peakIcrData = mydata

mydata = group_designation(mydata, main_effects = c("Location", "Crop.Flora"))

mydata = compound_calcs(mydata)

massFilt = mass_filter(mydata)
mydata = applyFilt(massFilt, mydata, min_mass = 200, max_mass = 900)

molFilt = molecule_filter(mydata)
mydata = applyFilt(molFilt, mydata, min_num = 2)

peakIcrProcessed = mydata

save(peakIcrData, file = "data/peakIcrData.rda")
save(peakIcrProcessed, file = "data/peakIcrProcessed.rda")