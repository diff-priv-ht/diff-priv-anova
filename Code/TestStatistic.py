import numpy as np


class TestStatistic:

    name = ""

    def __init__(self):
        pass

    def mean(self, d):
        """
        Generates total mean of database values, iterating over the individual groups of data.
        :param d: Instance of a database object.
        :return: Total mean of data.
        """

        flat_list = [item for sublist in d.data for item in sublist]  # list-of-lists to list
        overall_mean = np.mean(flat_list)
        return overall_mean

    def group_means(self, d):
        """
        Generates list of group means.
        :param d: Instance of a database object.
        :return: array where the value of index i corresponds to the mean of group i of d.data.
        """

        means = []
        i = 0
        while i < d.n_groups:
            means.append(np.mean(d.data[i]))
            i += 1
        return means

    def standard_deviation(self, d):
        """
        ToDo: this is incorrect, in terms of calculating the std_dev's of the multiple groups
        Outputs the sample standard deviation of the database d.
        :param d: Instance of a database object.
        :return: standard deviation
        """
        flat_list = [item for sublist in d.data for item in sublist]  # list-of-lists to list
        standard_dev = np.std(flat_list)
        return standard_dev

    def public_statistic(self, d):
        """
        Runs a specific statistic on database d.
        :param d: Instance of a database object.
        :return: Test statistic.
        """
        pass

    def private_statistic(self, d, epsilon, epsilon_frac):
        """
        Runs a specific private statistic on database d.
        :param d: Instance of a database object.
        :param epsilon: Privacy parameter
        :param epsilon_frac: description of how to allocate privacy parameter
        :return: Noisy test statistic.
        """
        pass

    def variance_est(self, d, epsilon, epsilon_frac):
        """
        Generates estimate of variance for a given database.
        :param d: Instance of a database object
        :param epsilon: Privacy parameters
        :param epsilon_frac: discription of how to allocate privacy parameter.
        :return:
        """
        pass

    def null_distribution(self, n_groups, db_size, n_runs, epsilon, epsilon_frac, variance):
        """
        Generates null distribution to compare statistic against? Should this be here??
        :param n_groups: Number of groups in database
        :param db_size: Total size of database
        :param n_runs: The number of runs to run the null distribution calculation.
        :param epsilon: Privacy parameter
        :param variance_est: Optional parameter; True if using estimated rather than true variance
        :return: null distribution
        """
        pass
