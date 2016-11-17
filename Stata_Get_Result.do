discard
//Import data.
use "/Users/windshield/Desktop/Robumeta/RVE-Example-Data/Correlated_Example.dta"
//Run function Robumeta and get results for correlated model (saved as ereturn).
robumetaMT12  effectsize followup males college, variance(var) weighttype(correlated) study(studyid) 

//Creat a new .xlsx file for storing results.
putexcel set /Users/windshield/Desktop/Robumeta/Stata_Test_function/StataResult.xlsx,replace

//store all ereturn values/results in that .xlsx file, start from cell "A1".
putexcel (A1) = e*, names

discard
use "/Users/windshield/Desktop/Robumeta/RVE-Example-Data/Hierarchical_Example.dta"
//Run function Robumeta and get results for hierarchical model
robumetaMT12  effectsize followup males college, study(clusterid) variance(var) weighttype(hierarchical)

//store all ereturn values/results in that .xlsx file, start from cell "A1".
putexcel (F1) = e*, names
