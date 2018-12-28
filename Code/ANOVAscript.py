from ANOVA import *
from TestStatisticAnalysis import *
import math

# ToDo: histograms of MSE^ and of critical values
# ToDo: Precompute critical values for sigmas
# ToDo: plot histogram of sigma estimates to get sense of spread
# ToDo: Figure out how different the critical values are
# ToDo: have it write a file of n epsilon sigma critical value combo meals
# ToDo: When MSE negative: how often does this happen when the MSA is also negative and stuff?
# ToDo: general how often does MSE go negative? maybe plot mse msa vals, see what they look like
# ToDo: make it so that you can input None as epsilon

### Initialize test parameters ###

n_groups = 3    # number of groups in databases
group_sizes_all = list(range(10,2000,250))

"""
The database class takes array group_sizes as input, where index i of group_sizes is the size of group i in the database.
Here, we assume every group per database has the same group size. Index j of group_sizes_all is the group size of all of
the groups in database j. We then create group_sizes_list from this, where index j of group_sizes_list is the group sizes
of every group in database j, each of which have the same size. 
"""
group_sizes_list = [[size]*n_groups for size in group_sizes_all]

means_list = [0.35, 0.5, 0.65]
means_list_null = [0.5]*n_groups

stand_dev = 0.15

n_runs_fstat = 1000
n_runs_null = 1000

epsilons = [0.5, 1]
eps_frac = 0.5

significance = 0.05

### Run test ###

a = ANOVA()

generate_data = True
if generate_data:
    databases = create_databases(group_sizes_list, means_list, stand_dev)
    print("Many runs...")
    many_runs(a, n_runs_fstat, databases, epsilons, eps_frac)

# Note: in accordance with differential privacy practices, the algorithms 
# will not access the data after doing the private calculations. 
# Everything beyond this point is post-processing.
databases = [] 

analysis = True
if analysis:
    ### P-Value Calculations ###

    print("Calculating p-values...")

    # Initialize arrays we'll store all of the power calculations
    percent_sig_true_by_eps = []
    percent_sig_est_by_eps = []
    inconsistency_counts_by_eps = []
    true_sig_est_not_counts_by_eps = []
    true_not_est_sig_counts_by_eps = []
    stdev_est_by_group = []

    for eps in epsilons:
        # Initialize arrays:

        # Array of percentage of significance p-values for each group size, for set epsilon
        percent_sig_true = []       # Using true variance of data
        percent_sig_est = []        # Using estimated variance of data

        # Array of counts for times where the true variance gives significant result while estimated variance doesn't
        # or vice versa

        inconsistency_counts = []
        true_sig_est_not_counts = []
        true_not_est_sig_counts = []

        # Iterate through each group size
        for group_size in group_sizes_all:
            # Compute true null distribution
            db_size = group_size*n_groups
            true_null_dist = a.null_distribution(n_groups, db_size, n_runs_null, eps, eps_frac, stand_dev**2)

            # Read in data for given group size and epsilon
            [stats, mses] = read_stats_by_group_epsilon(a, eps, group_size)[0]

            stdev_est = []
            # Looking at the standard deviation estimate instead of MSE
            for mse in mses:
                if mse >=0:
                    stdev_est.append(math.sqrt(mse))

            stdev_est_by_group.append(stdev_est)

            # Initialize variables
            significance_inconsistency = 0
            true_sig_est_not = 0
            true_not_est_sig = 0
            est_pvals = []
            true_pvals = []

            # Iterate through all of the data collected for each epsilon-database-size combination
            for (count, fstat) in enumerate(stats):

                # Compute estimated null distribution
                est_null_dist = a.null_distribution(n_groups, db_size, n_runs_null, eps, eps_frac, mses[count])

                # Compute p-values and append to est_pvals and true_pvals
                est_pval = pval(est_null_dist, fstat)
                true_pval = pval(true_null_dist, fstat)
                est_pvals.append(est_pval)
                true_pvals.append(true_pval)

                # Flag if one of est_pval or true_pval is significant while the other one isn't
                est_sig = False
                true_sig = False
                if est_pval >= significance:
                    est_sig = True
                if true_pval >= significance:
                    true_sig = True

                if est_sig and not true_sig:
                    true_not_est_sig += 1
                    significance_inconsistency += 1
                elif not est_sig and true_sig:
                    true_sig_est_not += 1
                    significance_inconsistency += 1

            # Use calculated p-values to calculate power of statistic on group size and epsilon
            percent_sig_est.append(percent_significant(est_pvals, significance))
            percent_sig_true.append(percent_significant(true_pvals, significance))

            # Collect the number of inconsistent significance values there were between estimated and true p-values
            inconsistency_counts.append(significance_inconsistency/n_runs_fstat)
            true_sig_est_not_counts.append(true_sig_est_not/n_runs_fstat)
            true_not_est_sig_counts.append(true_not_est_sig/n_runs_fstat)


        percent_sig_true_by_eps.append(percent_sig_true)
        percent_sig_est_by_eps.append(percent_sig_est)
        inconsistency_counts_by_eps.append(inconsistency_counts)
        true_sig_est_not_counts_by_eps.append(true_sig_est_not_counts)
        true_not_est_sig_counts_by_eps.append(true_not_est_sig_counts)


    ### Plot the power of the test with all epsilons ###

    total_sizes = [group_size*n_groups for group_size in group_sizes_all]
    power_plot_many(a, total_sizes, percent_sig_est_by_eps, significance, epsilons, [0]*len(epsilons), n_runs_fstat)

    ### Plot number of inconsistent points ###

    # plt.suptitle('Number of times true variance gives significance and MSE gives insignificant result'
    #              , fontsize=10)
    # plt.title(str(n_runs_fstat) + " Runs of F-Statistic " + str(n_runs_null) +
    #           " Runs to simulate estimated null distribution per F-statistic", fontsize=8)
    # for (count, ys) in enumerate(true_sig_est_not_counts_by_eps):
    #     plt.plot(total_sizes, ys, label="Epsilon = " + str(epsilons[count]))
    # plt.legend(loc=4)
    # plt.show()
    #
    # plt.suptitle('Number of times true variance insignificant but MSE gives significant result'
    #              , fontsize=10)
    # plt.title(str(n_runs_fstat) + " Runs of F-Statistic " + str(n_runs_null) +
    #           " Runs to simulate estimated null distribution per F-statistic", fontsize=8)
    # for (count, ys) in enumerate(true_not_est_sig_counts_by_eps):
    #     plt.plot(total_sizes, ys, label="Epsilon = " + str(epsilons[count]))
    # plt.legend(loc=4)
    # plt.show()

    ### Plot Histogram of MSE ###

    # for (count,mses) in enumerate(stdev_est_by_group):
    #     dir_name = "stdev_est_histograms"
    #     if not os.access(dir_name, os.F_OK):
    #         os.mkdir(dir_name, mode=0o777)
    #     plt.hist(mses, bins=100)
    #     plt.title("Standard Deviation Estimate Histogram with groupsize " + str(group_sizes_all[count]))
    #     plt.savefig(dir_name + '/' + str(group_sizes_all[count]) + '.png', bbox_inches='tight')
    #     plt.close()
    # plt.show()