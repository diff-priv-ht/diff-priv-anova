import Fq
import Gq
import Database
import File_Management

def data_generation_sim(epsilons, epsilon_num_allocs, group_sizes_all, means_all, \
    n_runs_stat, run_true_devs, run_est_devs, session_id, stand_devs, stats):
    """
    This function simulates databases (with an effect size) with the given parameters. Note that the databases must all have
    the same number of groups.
    """
    for eps in epsilons:
        for epsilon_num_alloc in epsilon_num_allocs:
            for group_sizes_db in group_sizes_all:
                for means_db in means_all:
                    for stand_dev in stand_devs:
                        for stat in stats:
                            run_data(eps, epsilon_num_alloc, group_sizes_db, means_db, n_runs_stat, session_id, stand_dev, stat)


def generate_null_dist(epsilons, epsilon_num_allocs, group_sizes_all, means_all, \
    n_runs_null, n_runs_stat, run_true_devs, run_est_devs, session_id, stand_devs, stats):
    """
    This function simulates null distributions for the simulated (alternate) databases and writes the 
    statistics for each database out to a file in a folder ending in _null.
    """
    for eps in epsilons:
        for epsilon_num_alloc in epsilon_num_allocs:
            for group_sizes_db in group_sizes_all:
                for means_db in means_all:
                    for stand_dev in stand_devs:
                        for stat in stats:
                            [statistics, _stand_devs] = File_Management.read_stats(eps, epsilon_num_alloc, group_sizes_db, \
                                                                                   means_db, session_id, stand_dev, stat, isEst=False, isNull=False)[0]
                            if run_true_devs:
                                # simulate true null distribution with given epsilon and group size, standard deviation
                                simulate_null_dists(eps, epsilon_num_alloc, group_sizes_db, n_runs_null,session_id, [stand_dev], stat, isEst=False)
                                
                            if run_est_devs:
                                # simulate estimated null distribution with epsilon, group size, mses
                                simulate_null_dists(eps, epsilon_num_alloc, group_sizes_db, n_runs_null, session_id, _stand_devs, stat, isEst=True, stand_dev_true=stand_dev)
                        

def run_data(eps, epsilon_num_alloc, group_sizes_db, means_db, n_runs, \
                    session_id, stand_dev, stat, isEst=False, isNull=False, stand_dev_true=None):
    """
    This function creates an output file containing the statistics and the estimated standard deviation (if the data isn not null)
    for the database it simulates (given all of the database parameters)
    
    """
    dir_name = session_id + "_Data/" + File_Management.make_output_folder(session_id, stat, isEst, isNull, isResults=False)
    fout = File_Management.name_output_file(eps, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat, stand_dev_true=stand_dev_true)
    with open(dir_name + "/" + fout, "w+") as file:
        for n in range(n_runs-1):
            database = Database.Normal(group_sizes_db, means_db, stand_dev)
            s = stat.private_statistic(database, eps, epsilon_num_alloc)
            if isNull:
                file.write(str(s[0]) + "\n")
            else:
                file.write(str(s[0]) + "," + str(s[1]) + "\n")

        # Don't write a newline on last line
        database =  Database.Normal(group_sizes_db, means_db, stand_dev)      
        s = stat.private_statistic(database, eps, epsilon_num_alloc)
        if isNull:
            file.write(str(s[0]))
        else:
            file.write(str(s[0]) + "," + str(s[1]))

def simulate_null_dists(eps, epsilon_num_alloc, group_sizes_db, n_runs_null, session_id, stand_devs, stat, isEst, stand_dev_true=None):
    """
    This function creates the null distributions for the databases with the given parameters.
    """
    num_groups = len(group_sizes_db)
    means_db = [0.5]*num_groups
    
    simulation_flags = []
    for stand_dev in stand_devs:
        if stand_dev >= 0:
            simulation_flags.append(True)
            if isEst:
                run_data(eps, epsilon_num_alloc, group_sizes_db, means_db, n_runs_null, \
                         session_id, stand_dev,stat, isEst, isNull=True, stand_dev_true=stand_dev_true)
            else:
                run_data(eps, epsilon_num_alloc, group_sizes_db, means_db, n_runs_null, \
                         session_id, stand_dev,stat, isEst, isNull=True)
        else:
            simulation_flags.append(False)
    return simulation_flags






