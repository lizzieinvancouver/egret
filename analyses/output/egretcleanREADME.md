Started 28 July 2025

On Saturna, by Lizzie for now 

What does each colum in egretclean mean? I am so glad you asked....

## The first 45 columns are the scraped data 

These were directly scraped from papers and are described in:
egret/data/egret.xlsx meta_general tab below (data scraped in data_detailed tab)

### I reproduce that information here: 

* datasetID -- short name for each publication
* study	-- used to differentiate between separate experiments within a publication. Shorthand: exp1, exp2, exp3 etc.
* entered.by -- 3-5 letter acronym 
* genus -- genus of individual
* species -- species of individual (without author)
* variety -- variety, accession, cultivar, or other subspecies designation
* crop -- Y/N plant is a crop species
* woody -- Y/N plant has a woody stem
* source.population -- name of population if applicable
* providence.lat -- Latitude of samples, in decimal degrees. Negative = South (_cleaned!_ see also egretDataScraping)
* providence.long -- Longitude of samples, in decimal degrees. Negative = West (_cleaned!_ see also egretDataScraping)
* providence.altitude -- altitude of sample origin, in meters (see also egretDataScraping)
* continent -- Continent of seed collection
* no.indiv.collected -- number of seeds collected in the field per indiviudal
* year.collected -- year seeds were collected
* storage.type -- how were seeds stored (e.g. dry, moist, in darkness
* storage.time -- how long were seeds stored for in days
* storage.humidity -- humidity of where seeds were stored in percent humidity
* storage.temp -- temperature during storage
* year.germination -- year of germination experiment
* treatment -- general name of treatment (e.g. dark, warm, dark/moist)
* chill.temp -- chilling or stratification temperature before germination
* chill.duration -- duration of chilling or stratification (days)
* germ.temp -- temperature during germination
* other.treatment -- treatments other than stratification, chemcial, light
* photoperiod -- light regime in hours daylight 
* chemical -- chemcials applied
* chemical.concent -- chemical concentration in ppm
* trt.duration -- duration for which a chemical treatment was applied (days)
* scarification -- Y/N 
* scarif.type -- how was it done? (e.g. chemcials, mechanical with sandpaper, pericarp removal)
* soaking -- Y/N pre-germination soaking
* soaked.in -- solution used for soaking
* soaking.duration -- how long were seeds soaked in minutes
* seed.mass.given -- Y/N - do they report the weight of the seeds either in the text or a table?
* respvar -- response variable of scraped data, e.g. per.germ = percent germination, mgt = mean germination time
* response -- value of respvar 
* error.type -- type of error recorded e.g. standard error
* resp.error -- error bars of scraped figure, if applicable
* reps -- number of replicates e.g. number of pots or petri dish per treatment
* n.per.rep -- number of seeds per pot or petri dish
* germ.duration -- number of days germination monitored -- negative values should be used if germination occurs prior to the start of the experiment 
* germ.tim.zero -- The zero point germination is measured relative to - "end cold strat" - other options include "unknown", "start emergence, X days after end cold strat", "other, end of treatment X"
* figure -- figure or table data taken from
* Notes -- Add notes on assumptions you made, or things you are not sure of

## The next columns are the result of cleaning

They are cleaned in analyses/cleaning/cleanall.R 

* germTempGen -- a column for weighted average temperatures, even for those that alternate, uses photoperiod as our thermoperiod
* germTemp  -- cleaned column of temperatures during germination, includes alternating values with slashes for alternating for varied temperatures, when alternating temperatures were ranges (eg. 27-29/6-18) vales were averaged for each alternate value
* germDuration -- cleaned column of germination duration, 
* germTempClass -- denotes whether germination temperatures varied, with 3 categories: "alternating", "constant" , "three levels"
* germTempDay -- cleaned day temperature during germination, averaged when a range, coded as tempDay and renamed to germTempDay at end of code
* germTempNight -- cleaned night temperature during germination, averaged when a range, coded as tempNight and renamed to germTempDay at end of code
* germPhotoperiod -- cleaned column of photoperiod conditions during germination, includes alternating values with slashes.
* germPhotoperiodDay -- day time photoperiod conditions taken from germPhotoperiod, usually the first value of the alternating day/night entries, or assumed so if not clear in the paper
* germPhotoperiodNight -- duration of night photoperiod taken from germPhotoperiod, usually the second value of the alternating day/night entries
* scarifType -- detailed description of scarification (as of July 2025: this is a slightly cleaned version of scarif.type that we stopped fully cleaning at some point)
* scarifTypeGen -- detailed description of scarification: NA, chemical, mechanical or soaking (which is in hot water)
* scarifTypeSpe -- very similar to scarifType, just cleaner **FIXME: clean up this one and delete out scarifType
* chemicalCor -- chemical column cleaned so there *should* be no duplicates that are mis-spelled or ordered differently (but you should double-check this)
* storageType -- storage information focusing on wet/dry/cold/room temp (around 21 different types)
* storageDetails -- storage information including substrate/vessel on top of wet/dry/cold/room temp 
* storageTemp -- storage temperatures given sequentially seperated by `then`
* storageDuration -- storage durations in days given sequentially seperated by `then`
* dormancyTemp
* dormancyDuration
* responseVar -- cleaned column of respvar with cleaned names for the response variables (n = 106)
* responseValue -- cleaned column of response, but in characters DELETED
* responseValueNum -- same as responseValue, but numeric, values are the same but with trailing zeros. 
* responseErrorType -- cleaned the names of the different types of errors, but error values not cleaned.
* photoperiodCor -- photoperiod simplified to light or dark (because there is not so much other info in photoperiod column)
* treatmentCor     
* treatmentDetails
* chemicalConcent -- chemical concentrations, when given sequentially they are separate by `+`, 
* chemicalConcentUnit -- chemical concentration in units, when given sequentially they are separate by `+`, when the unit is given it is correct (Victor checked), otherwise you can assume ppm (but Victor did not check)
* datasetIDstudy -- a unique experiment (datasetID and study pasted together)
* latbi	-- latin binomial (genus and species pasted together, includes subsp.)
* provLatLon -- provenance latitude and longitude pasted together
* provLatLonAlt -- provenance altitude??? WTF FIXME

