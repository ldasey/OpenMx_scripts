rm(list = ls())


library(OpenMx)
library(umx)
library(tidyverse)
library(haven)
library(psych)

# use multiple cores? 19 here
mxOption(key='Number of Threads', value=19)

# turn off scientific priting and increase max.print
options(scipen=999)
options(max.print=1000000)

# optimizer to use. SLSQP is good for ordinal data
mxOption(key = "Default optimizer", value = "SLSQP")

path <- "/analysis/"

# folder to save the results to
path_out <- paste0(path, "out/")

# Repeated measurement. See below how variables should be named (i.e. w indicates wave and T indicating twin order)
var <- "pop0100"

# ordinal levels, will result in length(levels)-1 thresholds
levels = c(1, 2, 3, 4)

# how many epochs?
tries <- 10


manifest_w1_T1 <- paste0(var, "_w1_T1")
manifest_w1_T2 <- paste0(var, "_w1_T2")
manifest_w2_T1 <- paste0(var, "_w2_T1")
manifest_w2_T2 <- paste0(var, "_w2_T2")
manifest_w3_T1 <- paste0(var, "_w3_T1")
manifest_w3_T2 <- paste0(var, "_w3_T2")


# read in data
data <- read_dta(paste0(path_out, "out_all.dta"))

# convert from tibble to dataframe as OpenMx won't accept tibbles
data <- data.frame(data)


latents = c(paste0(rep(c("AL", "CL", "EL"), each = 2), 1:2), paste0(rep(c("AS", "CS", "ES"), each = 2), 1:2),
            "e_w1_T1", "e_w2_T1", "e_w3_T1", "e_w1_T2", "e_w2_T2", "e_w3_T2", "L1", "L2", "S1", "S2")

covars <- c("sex_w1_T1", "age0101_w1_T1")


manifest <- c(manifest_w1_T1, manifest_w1_T2, manifest_w2_T1, manifest_w2_T2, manifest_w3_T1, manifest_w3_T2)
manifests <- c(manifest, covars)



# create factor variables
for (i in manifest) {
  data[, i] <- mxFactor(data[, i], levels = levels)
}

# create separate DFs for MZ and DZ twins
mzData = subset(data, zyg0102_w1 == 1)
dzData = subset(data, zyg0102_w1 == 2)



latVariances <- list(mxPath( from=c(paste0(rep(c("AL", "CL", "EL"), each = 2), 1:2), paste0(rep(c("AS", "CS", "ES"), each = 2), 1:2)), arrows=2,
                        free=FALSE, values=1),
                     mxPath( from=c("e_w1_T1", "e_w2_T1", "e_w3_T1", "e_w1_T2", "e_w2_T2", "e_w3_T2"), arrows=2,
                             free=TRUE, values=1, labels = c("e1", "e2", "e3", "e1", "e2", "e3"), lbound = 0.000000000001))
latVariances2 <- mxPath( from=c("L1", "L2", "S1", "S2"), arrows=2,
                        free=TRUE, values=1, labels = c("V1", "V2", "V1", "V2"))
latMeans     <- mxPath( from="one", to=c(paste0(rep(c("AL", "CL", "EL"), each = 2), 1:2), paste0(rep(c("AS", "CS", "ES"), each = 2), 1:2),
                                         "e_w1_T1", "e_w2_T1", "e_w3_T1", "e_w1_T2", "e_w2_T2", "e_w3_T2"), arrows=1,
                        free=FALSE, values=0 )
latMeans2     <- mxPath( from="one", to=c("L1", "L2", "S1", "S2"), arrows=1,
                        labels = c("lmeans", "lmeans", "smeans", "smeans"),free=TRUE, values=c(0.5, 0.5,.3, .3) )



paths_e <- list(mxPath(from="e_w1_T1", to=manifest_w1_T1, arrows=1,
                       free=FALSE, values = 1),
                mxPath(from="e_w2_T1", to=manifest_w2_T1, arrows=1,
                       free=FALSE, values = 1),
                mxPath(from="e_w3_T1", to=manifest_w3_T1, arrows=1,
                       free=FALSE, values = 1),
                mxPath(from="e_w1_T2", to=manifest_w1_T2, arrows=1,
                       free=FALSE, values = 1),
                mxPath(from="e_w2_T2", to=manifest_w2_T2, arrows=1,
                       free=FALSE, values = 1),
                mxPath(from="e_w3_T2", to=manifest_w3_T2, arrows=1,
                       free=FALSE, values = 1))

paths_e_MZ <- list(mxPath(from = "e_w1_T1", to = "e_w1_T2", free = TRUE, labels = "e1_cov", values = .1, lbound = .000000000000000001),
                   mxPath(from = "e_w2_T1", to = "e_w2_T2", free = TRUE, labels = "e2_cov", values = .1, lbound = .000000000000000001),
                   mxPath(from = "e_w3_T1", to = "e_w3_T2", free = TRUE, labels = "e3_cov", values = .1, lbound = .000000000000000001))
paths_e_DZ <- list(mxPath(from = "e_w1_T1", to = "e_w1_T2", free = TRUE, labels = "e1_cov", values = .1, lbound = .000000000000000001),
                   mxPath(from = "e_w2_T1", to = "e_w2_T2", free = TRUE, labels = "e2_cov", values = .1, lbound = .000000000000000001),
                   mxPath(from = "e_w3_T1", to = "e_w3_T2", free = TRUE, labels = "e3_cov", values = .1, lbound = .000000000000000001))



facloadings_level <- list(mxPath(from = "L1", to = c(manifest_w1_T1), arrows = 1, free = FALSE, values = 1),
                          mxPath(from = "L1", to = c(manifest_w2_T1), arrows = 1, free = FALSE, values = 1),
                          mxPath(from = "L1", to = c(manifest_w3_T1), arrows = 1, free = FALSE, values = 1),
                          mxPath(from = "L2", to = c(manifest_w1_T2), arrows = 1, free = FALSE, values = 1),
                          mxPath(from = "L2", to = c(manifest_w2_T2), arrows = 1, free = FALSE, values = 1),
                          mxPath(from = "L2", to = c(manifest_w3_T2), arrows = 1, free = FALSE, values = 1))


facloadings_slope <- list(mxPath(from = "S1", to = c(manifest_w1_T1), arrows = 1, free = FALSE, values = 0),
                          mxPath(from = "S1", to = c(manifest_w2_T1), arrows = 1, free = FALSE, labels = "loading_slope_1[1,1]"),
                          mxPath(from = "S1", to = c(manifest_w3_T1), arrows = 1, free = FALSE, values = 1),
                          mxPath(from = "S2", to = c(manifest_w1_T2), arrows = 1, free = FALSE, values = 0),
                          mxPath(from = "S2", to = c(manifest_w2_T2), arrows = 1, free = FALSE, labels = "loading_slope_2[1,1]"),
                          mxPath(from = "S2", to = c(manifest_w3_T2), arrows = 1, free = FALSE, values = 1))

algebras <- list(mxMatrix(type = "Full", values = 1, free = TRUE, labels = "y", nrow = 1, ncol=1),
                 mxAlgebra(expression = y, name = "loading_slope_1"),
                 mxAlgebra(expression = y, name = "loading_slope_2"))


ACE <- list(mxPath(from = "AL1", to = "L1", arrows = 1, free = TRUE, labels = "a_l", values = 0.6, lbound = 0.0000000000000000000000000001),
            mxPath(from = "AL2", to = "L2", arrows = 1, free = TRUE, labels = "a_l", values = 0.6, lbound = 0.0000000000000000000000000001),
            mxPath(from = "CL1", to = "L1", arrows = 1, free = TRUE, labels = "c_l", values = 0.2, lbound = 0.0000000000000000000000000001),
            mxPath(from = "CL2", to = "L2", arrows = 1, free = TRUE, labels = "c_l", values = 0.2, lbound = 0.0000000000000000000000000001),
            mxPath(from = "EL1", to = "L1", arrows = 1, free = TRUE, labels = "e_l", values = 0.6, lbound = 0.0000000000000000000000000001),
            mxPath(from = "EL2", to = "L2", arrows = 1, free = TRUE, labels = "e_l", values = 0.6, lbound = 0.0000000000000000000000000001),
            mxPath(from = "AS1", to = "S1", arrows = 1, free = TRUE, labels = "a_s", values = 0.1, lbound = 0.0000000000000000000000000001),
            mxPath(from = "AS2", to = "S2", arrows = 1, free = TRUE, labels = "a_s", values = 0.1, lbound = 0.0000000000000000000000000001),
            mxPath(from = "CS1", to = "S1", arrows = 1, free = TRUE, labels = "c_s", values = 0.2, lbound = 0.0000000000000000000000000001),
            mxPath(from = "CS2", to = "S2", arrows = 1, free = TRUE, labels = "c_s", values = 0.2, lbound = 0.0000000000000000000000000001),
            mxPath(from = "ES1", to = "S1", arrows = 1, free = TRUE, labels = "e_s", values = 0.6, lbound = 0.0000000000000000000000000001),
            mxPath(from = "ES2", to = "S2", arrows = 1, free = TRUE, labels = "e_s", values = 0.6, lbound = 0.0000000000000000000000000001)
)


ACE_cross <- list(mxPath(from = c("AL1", "CL1", "EL1"), to = "S1", arrows = 1, free = TRUE, labels = c("al_s", "cl_s", "el_s"), values = c(-.1,-.1-.1)),
                  mxPath(from = c("AL2", "CL2", "EL2"), to = "S2", arrows = 1, free = TRUE, labels = c("al_s", "cl_s", "el_s"), values = c(-.1,-.1-.1)))





betaWeights_sex  <- list(mxPath(from="sex_w1_T1", to=manifest, arrows=1, free=TRUE, labels=c("beta_sex_w1", "beta_sex_w1", "beta_sex_w2", "beta_sex_w2",
                                                                                             "beta_sex_w3", "beta_sex_w3"), values = .12))



betaWeights_age  <- list(mxPath(from="age0101_w1_T1", to=manifest, arrows=1, free=TRUE, labels=c("beta_age_w1", "beta_age_w1", "beta_age_w2", "beta_age_w2",
                                                                                                 "beta_age_w3", "beta_age_w3"), values = .1)
                      )

betaWeights_cov <- list(mxPath(from = c("sex_w1_T1", "age0101_w1_T1"), arrows = 2, free = FALSE, values = 1, labels = c("var_sex", "var_age")),
                        mxPath(from = "sex_w1_T1", to = "age0101_w1_T1", arrows = 2, free = TRUE, values = .1, labels = "cov_betas"))


DZ_cor <- list(mxPath(from = "AL1", to = "AL2", arrows = 2, free = FALSE, values = 0.5),
               mxPath(from = "AS1", to = "AS2", arrows = 2, free = FALSE, values = 0.5))

MZ_cor <- list(mxPath(from = "AL1", to = "AL2", arrows = 2, free = FALSE, values = 1),
               mxPath(from = "AS1", to = "AS2", arrows = 2, free = FALSE, values = 1))

c_cor <- list(mxPath(from = "CL1", to = "CL2", arrows = 2, free = FALSE, values = 1),
              mxPath(from = "CS1", to = "CS2", arrows = 2, free = FALSE, values = 1))



dataMZ <- mxData(observed=mzData, type = "raw")
dataDZ <- mxData(observed=dzData, type = "raw")



threshold <- mxThreshold(vars=manifest, nThresh=3, free = c(FALSE, FALSE, TRUE), labels = c("true1", "true2", "true3"), values = c(0,1,2))


paths <- list(latVariances, paths_e, facloadings_level, facloadings_slope, ACE, c_cor, latMeans2, ACE_cross, threshold, betaWeights_age, betaWeights_sex, betaWeights_cov, algebras)


modelMZ      <- mxModel(model="MZ", type="RAM", manifestVars=manifests,
                        latentVars=latents, paths, MZ_cor, dataMZ, paths_e_MZ)
modelDZ      <- mxModel(model="DZ", type="RAM", manifestVars=manifests,
                        latentVars=latents, paths, DZ_cor, dataDZ, paths_e_DZ)

obj          <- mxFitFunctionMultigroup(c("MZ", "DZ"))


fit          <- mxFitFunctionML()


model     <- mxModel("LGC", fit, modelMZ, modelDZ, obj)




result_normal <- mxTryHardOrdinal(model, extraTries = tries)
summary(result_normal)


# test submodels

model_error <- omxSetParameters(model, labels = c("e1", "e2", "e3"), newlabels = "e", name = "Reduce error")
model_error <- omxSetParameters(model_error, labels = c("e1_cov", "e2_cov", "e3_cov"), newlabels = "e_cov", name = "Reduce error")

result_normal_error <- mxTryHardOrdinal(model_error, extraTries = tries)

summary(result_normal_error)

mxCompare(result_normal, result_normal_error)
omxAkaikeWeights(c(result_normal, result_normal_error))



# c_l e_ls e_s

model_2 <- omxSetParameters(model, labels = "c_s", free = FALSE, values = 0, name = "Reduce c_s")
result_normal_2 <- mxTryHardOrdinal(model_2, extraTries = tries)
summary(result_normal_2)
omxAkaikeWeights(c(result_normal, result_normal_error, result_normal_2))




model_3 <- omxSetParameters(model_2, labels = "a_s", free = FALSE, values = 0, name = "Reduce a_s")
result_normal_3 <- mxTryHardOrdinal(model_3, extraTries = tries)
summary(result_normal_3)
omxAkaikeWeights(c(result_normal, result_normal_error, result_normal_2, result_normal_3))





model_4 <- omxSetParameters(model_3, labels = "al_s", free = FALSE, values = 0, name = "Reduce al_s")
result_normal_4 <- mxTryHardOrdinal(model_4, extraTries = tries)
summary(result_normal_4)
omxAkaikeWeights(c(result_normal, result_normal_error, result_normal_2, result_normal_3, result_normal_4))




model_5 <- omxSetParameters(model_3, labels = "c_l", free = FALSE, values = 0, name = "Reduce c_l")
result_normal_5 <- mxTryHardOrdinal(model_5, extraTries = tries)
summary(result_normal_5)
omxAkaikeWeights(c(result_normal, result_normal_error, result_normal_2, result_normal_3, result_normal_4, result_normal_5))


# model 4 or 5! try both together:



model_6 <- omxSetParameters(model_5, labels = "al_s", free = FALSE, values = 0, name = "Reduce al_s and c_l")
result_normal_6 <- mxTryHardOrdinal(model_6, extraTries = tries)
summary(result_normal_6)
omxAkaikeWeights(c(result_normal, result_normal_error, result_normal_2, result_normal_3, result_normal_4, result_normal_5, result_normal_6))



# model 4 the best



# model 5 is the best model! continue with algebra and CIs

algebra    <- list(mxAlgebra( expression=a_l*a_l, name="Alevel"),
                   mxAlgebra( expression=c_l*c_l, name="Clevel"),
                   mxAlgebra( expression=e_l*e_l, name="Elevel"),
                   mxAlgebra( expression=a_s*a_s+al_s*al_s, name="Aslope"),
                   mxAlgebra( expression=c_s*c_s+cl_s*cl_s, name="Cslope"),
                   mxAlgebra( expression=e_s*e_s+el_s*el_s, name="Eslope"),
                   mxAlgebra( expression=Alevel + Clevel + Elevel, name="Totlevel"),
                   mxAlgebra( expression=Aslope + Cslope + Eslope, name="Totslope"),
                   mxAlgebra(expression = Totslope + Totlevel, name = "Tot"),
                   mxAlgebra( expression=Alevel / Tot, name="Alevelprop"),
                   mxAlgebra( expression=Clevel / Tot, name="Clevelprop"),
                   mxAlgebra( expression=Elevel / Tot, name="Elevelprop"),
                   mxAlgebra( expression=Aslope / Tot, name="Aslopeprop"),
                   mxAlgebra( expression=Cslope / Tot, name="Cslopeprop"),
                   mxAlgebra( expression=Eslope / Tot, name="Eslopeprop"),
                   mxAlgebra(expression = Tot/(e1+Tot), name="Properror1"),
                   mxAlgebra(expression = Tot/(e2+Tot), name="Properror2"),
                   mxAlgebra(expression = Tot/(e3+Tot), name="Properror3"),
                   mxAlgebra(expression = a_l*al_s, name = "covA"),
                   mxAlgebra(expression = c_l*cl_s, name = "covC"),
                   mxAlgebra(expression = e_l*el_s, name = "covE"),
                   mxAlgebra(expression = covA/(sqrt(Alevel)*sqrt(Aslope)), name="rA"),
                   mxAlgebra(expression = covC/(sqrt(Clevel)*sqrt(Cslope)), name="rC"),
                   mxAlgebra(expression = covE/(sqrt(Elevel)*sqrt(Eslope)), name="rE"),
                   mxAlgebra(expression = (covA+covC+covE)/(sqrt(Totlevel)*sqrt(Totslope)), name = "rP"),
                   mxAlgebra(expression = covA/(covA+covC+covE), name = "rA_p"),
                   mxAlgebra(expression = covC/(covA+covC+covE), name = "rC_p"),
                   mxAlgebra(expression = covE/(covA+covC+covE), name = "rE_p"),
                   mxAlgebra(expression = Alevel/Totlevel, name = "A_prop_uni"),
                   mxAlgebra(expression = Clevel/Totlevel, name = "C_prop_uni"),
                   mxAlgebra(expression = Elevel/Totlevel, name = "E_prop_uni"))


model_final <- mxModel(model_4, algebra)
result_final <- mxTryHardOrdinal(model_final, extraTries = tries)


blub <- mxBootstrap(result_final)

CI_boot <- c("Alevel", "Clevel", "Elevel", "Aslope", "Cslope", "Eslope", "Totlevel", "Totslope", "Tot", "Alevelprop", "Clevelprop", "Elevelprop", "Aslopeprop",
             "Cslopeprop", "Eslopeprop", "Properror1", "Properror2", "Properror3", "e1", "e2", "e3", "rC", "rE", "rP", "rC_p", "rE_p",
             "A_prop_uni", "C_prop_uni", "E_prop_uni")
results_CI <- list()
for (i in CI_boot) {
  results_CI[[i]] <- mxBootstrapEvalByName(i, model = blub, bq = c(0.025, 0.975))
}

summary_final <- summary(result_final)
summary_final_v <- summary(result_final, verbose = TRUE)
akaike <- omxAkaikeWeights(c(result_normal, result_normal_error, result_normal_2, result_normal_3, result_normal_4, result_normal_5, result_normal_6))
compare <- mxCompare(base = result_normal, comparison = c(result_normal_error, result_normal_2, result_normal_3, result_normal_4, result_normal_5, result_normal_6))

summary_final
summary_final_v


results <- list(summary_final, summary_final_v, akaike, compare, results_CI, result_final)
save(results, file = paste0(path, "results/biometric_.RData"))
