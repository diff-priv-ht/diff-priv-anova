import File_Management

def aggregate(params):
    """
    This function writes to files the number of null statistics that are above and the number that are below the alternate statistics
    """
    session_id = params["session_id"]
    stats = params["stats"]
    group_sizes_all = params["group_sizes_all"]
    stand_devs = params["stand_devs"]
    means_all = params["means_all"]
    epsilons = params["epsilons"]
    epsilon_num_allocs = params["epsilon_num_allocs"]
    alphas = params["alphas"]
    n_runs_null = params["n_runs_null"]
    n_runs_stat = params["n_runs_stat"]
    
    for eps in epsilons:
        for epsilon_num_alloc in epsilon_num_allocs:
            for group_sizes_db in group_sizes_all:
                for means_db in means_all:
                    for stand_dev in stand_devs:
                        for stat in stats:
                            if params["run_true_devs"]:
                                _aggregate_true_devs(eps, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat)
                            if params["run_est_devs"]:
                                _aggregate_est_devs(eps, epsilon_num_alloc, group_sizes_db, means_db, n_runs_null, n_runs_stat, session_id, stand_dev, stat)
                                
   
def _aggregate_true_devs(eps, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat):
    """
    This function computes the number of null simulations have statistic values above the effect data's statistic, 
    and writes the number above and the total number to a file. We chose to take this approach because it makes
    grouping by certain values (e.g. group by epsilon fraction and standard deviation but vary over group sizes) later easier. 
    """
    dir = File_Management.make_output_folder(session_id, stat, isEst=False, isNull=False, isResults=True)
    filename = File_Management.name_output_file(eps, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat)
    
    # Read in the F-values for the simulated databases with the given effect size
    [alt_stats, stand_devs] = File_Management.read_stats(eps, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat, isEst=False, isNull=False)[0]
        
    # Read in the F-values for the null databases
    null_true_stats = File_Management.read_stats(eps, epsilon_num_alloc, group_sizes_db, [0.5]*len(group_sizes_db), session_id, stand_dev, stat, isEst=False, isNull=True)[0]
    counts_above = []
    counts_total = []
    for alt in alt_stats:
        count_above = 0
        total = 0
        for null in null_true_stats:
            if null >= alt: # we want to know how many null stats are as extreme or more than the alternate
                count_above += 1
            total += 1
        counts_above.append(count_above)
        counts_total.append(total)
            
            
    path = session_id + "_Data/" + dir + "/" + filename
    str_counts_above = str(counts_above)[1:-1]
    str_counts_total = str(counts_total)[1:-1]
    with open(path, "w") as file:
        file.write(str_counts_above + "\n")
        file.write(str_counts_total)

def _aggregate_est_devs(eps, epsilon_num_alloc, group_sizes_db, means_db, n_runs_null, n_runs_stat, session_id, stand_dev, stat):
    """
    This function does the same thing as _aggretate_true_devs() except that it handles the estimated standard deviations.
    """
    
    # Get the correct directory
    dir = File_Management.make_output_folder(session_id, stat, isEst=True, isNull=False, isResults=True)
    filename = File_Management.name_output_file(eps, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat)
    path = session_id + "_Data/" + dir + "/" + filename

    # Read in stats from simulated data (under given effect size)
    [alt_stats,stand_devs] = File_Management.read_stats(eps, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat, isNull=False)[0]
    counts_above = []
    counts_total = []
    
    for i  in range(len(stand_devs)):
        count_above = 0
        count_total = 0
        if stand_devs[i] >= 0:
            # Read in the null simulations for a given standard deviation estimate
            null_est_stats = File_Management.read_stats(eps, epsilon_num_alloc, group_sizes_db, [0.5]*len(group_sizes_db), session_id, stand_devs[i], stat,isEst=True, isNull=True)[0]
                        
            for null_stat in null_est_stats:
                if null_stat >= alt_stats[i]:
                    count_above+=1
                count_total += 1
                
            counts_above.append(count_above)
            counts_total.append(count_total)
        # If the standard deviation estimate is negative, we automatically don't reject the null hypothesis
        else:
            counts_above.append(n_runs_null)
            counts_total.append(n_runs_null)

    str_counts_above = str(counts_above)[1:-1]
    str_counts_total = str(counts_total)[1:-1]
    with open(path, "w") as file:
        file.write(str_counts_above + "\n")
        file.write(str_counts_total)
    
