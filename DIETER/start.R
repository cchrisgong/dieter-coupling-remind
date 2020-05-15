library(lucode)

scenname <- NULL
readArgs("scenname")
if (!dir.exists("output")) dir.create("output")
print(scenname)
foldername <- paste0("output/",scenname,"_",format(Sys.time(), "%Y_%m_%d_%H-%M-%S"))
dir.create(foldername)
file.copy(c(
	"DIETER_v1.0.2.gms",
	"dataload.gms",
	"Scenario.gms",
	"fix.gms",
	"Data_input.gdx",
	"cluster_start.cmd",
	"Load.csv",
	"Conventionals.csv",
	"Renewables.csv",
	"VRE_potential.csv",
	"Time_Data.csv",
	"DSM_curt.csv",
	"DSM_shift.csv",
	"Storage.csv",
	"Reserves.csv",
	"Reserves2.csv",
	"phi_AC.csv",
        "FuelPrice.csv",
	"AC_demand.csv",
	"Reserves_hourly.csv"),foldername)
setwd(foldername)
system("sbatch cluster_start.cmd")
