
File_path <- "/Users/windshield/Desktop/Robumeta/Stata_Test_function/"
#RUN THIS CODE: testthat::test_file(paste0(File_path,"Stata_Test_Code.R"))


########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
#Import Stata Results

library(testthat)
library(gdata)
Stata_Result <- read.xls(xls = paste0(File_path,"StataResult.xlsx"),as.is = TRUE)

########################################################################################################
########################################################################################################

#Comparing with old Stata result
#Test on Correlated Model
context("CorrelatedModel_Compared with Old Stata Result")

## Extract values to be tested.
Stata_Result_Corr_coef <-as.vector(as.numeric(Stata_Result[13,1:4]))
Stata_Result_Corr_df <- as.vector(as.numeric(Stata_Result[336,1:4]))
Stata_Result_Corr_tau_squ <-as.vector(as.numeric(Stata_Result[3,1]))

##Start testing
test_that("Tau_square_Est", {
  expect_equivalent(Stata_Result_Corr_tau_squ,  0.133275131)
})

test_that("Coef_Est", {
  expect_equivalent(Stata_Result_Corr_coef,  c(-0.001678781, -0.003476397, 0.346688465, 0.480294315))
})

test_that("df_Est", {
  expect_equivalent(Stata_Result_Corr_df,  c(2.18402209103,  15.522548365,  13.45787834, 12.551791778))
})



#Test on Hierarchical Model
context("HierarchicalModel_Compared with Old Stata Result")
# Extract values to be tested.
Stata_Result_Hier_coef <-as.vector(as.numeric(as.character(Stata_Result[14,6:9])))
Stata_Result_Hier_df <- as.vector(as.numeric(as.character(Stata_Result[148,6:9])))
Stata_Result_Hier_tau_squ <-as.vector(as.numeric(as.character(Stata_Result[3,6])))
Stata_Result_Hier_omega_squ <-as.vector(as.numeric(as.character(Stata_Result[5,6])))
##Start testing
test_that("Tau_square_Est", {
  expect_equivalent(Stata_Result_Hier_tau_squ,  0.05015956)
})

test_that("Omega_square_Est", {
  expect_equivalent(Stata_Result_Hier_omega_squ,  0.09366396)
})

test_that("Coef_Est", {
  expect_equivalent(Stata_Result_Hier_coef,  c(0.001584995,  0.001309979,  0.698231322, -0.401368603))
})

test_that("df_Est", {
  expect_equivalent(Stata_Result_Hier_df,  c(2.006756978, 1.886770432, 5.640611896, 2.151046670))
})


########################################################################################################
########################################################################################################
#Comparing with R result
#Build correlated model with R
library(robumeta)
robu_result_corr <- robu(effectsize ~ followup + males + college,
                         data = corrdat,
                         studynum = studyid,
                         var.eff.size = var,
                         modelweights = "CORR",
                         small = T,
                         rho = 1)



#Build hierarchical model with R
robu_result_hier <- robu(effectsize ~ followup + males + sreport,
                         data =hierdat,
                         studynum = studyid,
                         var.eff.size = var,
                         modelweights = "HIER",
                         small = T)

#Test on Correlated Model
context("CorrelatedModel_Compared with R Result")

## Extract values to be tested.
Stata_Result_Corr_coef <-as.vector(as.numeric(Stata_Result[13,1:4]))
Stata_Result_Corr_df <- as.vector(as.numeric(Stata_Result[336,1:4]))
Stata_Result_Corr_tau_squ <-as.vector(as.numeric(Stata_Result[3,1]))




##Start testing
test_that("Tau_square_Est", {
  expect_equivalent(Stata_Result_Corr_tau_squ,  robu_result_corr$mod_info$tau.sq[1,1])
})

test_that("Coef_Est", {
  expect_equivalent(Stata_Result_Corr_coef,  robu_result_corr$b.r[c(2:4,1)])
})

test_that("df_Est", {
  expect_equivalent(Stata_Result_Corr_df,   robu_result_corr$dfs[c(2:4,1)])
})



#Test on Hierarchical Model
context("HierarchicalModel_Compared with R Result")
# Extract values to be tested.
Stata_Result_Hier_coef <-as.vector(as.numeric(as.character(Stata_Result[14,6:9])))
Stata_Result_Hier_df <- as.vector(as.numeric(as.character(Stata_Result[148,6:9])))
Stata_Result_Hier_tau_squ <-as.vector(as.numeric(as.character(Stata_Result[3,6])))
Stata_Result_Hier_omega_squ <-as.vector(as.numeric(as.character(Stata_Result[5,6])))
##Start testing
test_that("Tau_square_Est", {
  expect_equivalent(Stata_Result_Hier_tau_squ, robu_result_hier$mod_info$tau.sq[1,1])
})
test_that("Omega_square_Est", {
  expect_equivalent(Stata_Result_Hier_omega_squ, robu_result_hier$mod_info$omega.sq[1,1])
})

test_that("Coef_Est", {
  expect_equivalent(Stata_Result_Hier_coef,  robu_result_hier$b.r[c(2:4,1)])
})

test_that("df_Est", {
  expect_equivalent(Stata_Result_Hier_df, robu_result_hier$dfs[c(2:4,1)])
})






