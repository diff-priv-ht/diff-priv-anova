
import os

def make_csv(header, rho, i_file, o_file):
    with open(i_file, "r") as file:
        db_sizes = file.readline()
        powers = file.readline()
    db_sizes = db_sizes.split("\n")[0]
    db_sizes = db_sizes.split(", ")
    powers = powers.split("\n")[0]
    powers = powers.split(", ")
    
    with open(o_file, "a") as file:
        if ((os.stat(o_file).st_size) == 0):
            file.write(header + "\n")
        for i in range(len(powers)):
            file.write(db_sizes[i] + ", " + powers[i] + ", " + str(rho) + "\n" )
    return

make_csv("N, power, rho", 0.5, 
   "manyEffectSizesEpsFrac5_F_1_epsilon_1_epsilonNumAlloc_0.5_groupSizesDb_10-10-10_meansDb_0.35-0.5-0.65_standDev_0.25_alpha_0.05_.csv", "graphdata_simga025_means035")

make_csv("N, power, rho", 0.55, 
    "manyEffectSizesEpsFrac5_F_1_epsilon_1_epsilonNumAlloc_0.55_groupSizesDb_10-10-10_meansDb_0.35-0.5-0.65_standDev_0.25_alpha_0.05_.csv", "graphdata_simga025_means035")

make_csv("N, power, rho", 0.6, 
    "manyEffectSizesEpsFrac5_F_1_epsilon_1_epsilonNumAlloc_0.6_groupSizesDb_10-10-10_meansDb_0.35-0.5-0.65_standDev_0.25_alpha_0.05_.csv", "graphdata_simga025_means035")

make_csv("N, power, rho", 0.65, 
    "manyEffectSizesEpsFrac5_F_1_epsilon_1_epsilonNumAlloc_0.65_groupSizesDb_10-10-10_meansDb_0.35-0.5-0.65_standDev_0.25_alpha_0.05_.csv", "graphdata_simga025_means035")

make_csv("N, power, rho", 0.7, 
    "manyEffectSizesEpsFrac5_F_1_epsilon_1_epsilonNumAlloc_0.7_groupSizesDb_10-10-10_meansDb_0.35-0.5-0.65_standDev_0.25_alpha_0.05_.csv", "graphdata_simga025_means035")


make_csv("N, power, rho", 0.75, 
    "manyEffectSizesEpsFrac5_F_1_epsilon_1_epsilonNumAlloc_0.75_groupSizesDb_10-10-10_meansDb_0.35-0.5-0.65_standDev_0.25_alpha_0.05_.csv", "graphdata_simga025_means035")


