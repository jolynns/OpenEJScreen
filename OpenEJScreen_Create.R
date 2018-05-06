# Jolynn Schmidt
# 1/18/2017
# This Script will apply the CalEnviroScreen Methodology
# to the EJScreen indicators

# Use Arcgis R-Bridge
library(arcgisbinding)
# Confirm ArcPro License
arc.check_product()

# Set location of EJScreeen Data file for the State
myData <- "C:/GIS/Capstone/NJ_Screen/NJ_Screen.gdb/EJSCREEN_StatePctile_v4_NJ"

# open NJ selected data
Ejscreen_NJ <- arc.open(path = myData)

# Apply the calulations from CalEnviroScreen to create a new column

# Select the rows we need for each component and remove low population areas
EJS_USED <- arc.select(object = Ejscreen_NJ, fields = c('ID','ST_ABBREV','ACSTOTPOP', 
                                                        'P_PM25', 'P_OZONE', 'P_DSLPM', 
                                                        'P_PTRAF', 'P_LDPNT', 'P_PRMP',
                                                        'P_PNPL', 'P_PTSDF', 'P_PWDIS',
                                                        'P_CANCR', 'P_RESP','P_MINORPCT', 
                                                        'P_LWINCPCT', 'P_LNGISPCT', 
                                                        'P_LESHSPCT'), where_clause = "ACSTOTPOP >= 100")

# Step 1
# Exposure Component Score = Average(OzonePctile, PM2.5Pctile, NATAdpmPctile, leadPctile, DryCleanersPctile, TrafficPctile, RMPPctile)
# Environmental Component Score = Average(SperfundPctile, TSDFPctile, wastewaterPctile) X 0.5
# Sensitive Population Component Score = Average(RespiratoryPctile, CancerPctile)
# Socioeconomic Component Score = Average(EducationPctile, LinguisticIsoPctile, PovertyPctile, MinorityPctile)

# Prep
# assign indicators to their proper component
EJS_Expos_pctiles <- EJS_USED[ c('P_PM25', 'P_OZONE', 'P_DSLPM', 'P_PTRAF', 'P_LDPNT', 'P_PRMP')]
EJS_Env_pctiles <- EJS_USED[ c('P_PNPL', 'P_PTSDF', 'P_PWDIS')]
EJS_SensPop_pctiles <- EJS_USED[ c('P_CANCR', 'P_RESP')]
EJS_SocioEco_pctiles <- EJS_USED[ c('P_MINORPCT', 'P_LWINCPCT', 'P_LNGISPCT', 'P_LESHSPCT')]

# calulate the Average for each component to get the component score
Expos_CS <- apply(EJS_Expos_pctiles,1, FUN = mean)
Env_CS <- (apply(EJS_Env_pctiles,1, FUN = mean)) * 0.5
SensPop_CS <- apply(EJS_SensPop_pctiles,1, FUN = mean)
SocioEco <- apply(EJS_SocioEco_pctiles,1, FUN = mean)

# Step 2
# Pollution Burden = (Exposure Component Score + Environmental Component Score) / (1 + 0.5)
# Population Characteristics = (Sensitive Population Component Score + Socioeconomic Component Score) / 2

# Average further to get the Pollution Burden and Population Char.
PolBurden <- ((Expos_CS + Env_CS)/1.5)
PopChar <- ((SensPop_CS + SocioEco)/2)

# Step 3
# Scaled Pollution Burden = (Pollution Burden / Highest Pollution Burden) X 10
# Scaled Population Characteristics = (Population Characteristics / Highest Population Characteristics) X 10

# Get highest values
M_PolBurden <- max(PolBurden, na.rm = T)
M_PopChar <- max(PopChar, na.rm = T)

S_PolBurden <- ((PolBurden/M_PolBurden)*10)
S_PopChar <- ((PopChar/M_PopChar)*10)

# Step 4
# Screening Score = Scaled Pollution Burden X Scaled Population Characteristics

OEJScreen_Score <- (S_PopChar*S_PolBurden)

# Step 5
# Screening Rank

#get number of rows
OEJS_Rows <- NROW(OEJScreen_Score)

# rank and then assign percentiles
OEJScreen_Rank <- rank(OEJScreen_Score, na.last = TRUE,ties.method = c("min"))
OEJScreen_PctRank <- round(((OEJScreen_Rank/OEJS_Rows) * 100), digits = 2)

# add the new data we just created back to the original data
EJS_USED$OEJS_Score <- OEJScreen_Score
EJS_USED$OEJS_Rank <- OEJScreen_Rank
EJS_USED$OEJS_PctRank <- OEJScreen_PctRank

# Write out the new OEJScree_NJ and use the projection from EJScreen_NJ
arc.write('C:/GIS/Capstone/NJ_Screen/NJ_Screen.gdb/OEJScreen_NJ',EJS_USED, shape_info = arc.shapeinfo(Ejscreen_NJ) )


