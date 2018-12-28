from TestStatistic import *
import numpy as np
import Database


class ANOVA(TestStatistic):
    name = "ANOVA"

    @staticmethod
    def SSE_sensitivity():
        """
        Upper bound on sensitivity of SSE calculation as proven in paper.
        :return: sensitivity
        """
        return 7

    @staticmethod
    def SSA_sensitivity(database_size):
        """
        Upper bound on sensitivity of SSA calculation as proven in paper.
        :param database_size: int. The size of the database you are using 
        to calculate the private statistic.
        :return: sensitivity
        """
        return 9 + (5/database_size)

    def public_SSE(self, d):
        """
        Public (non-differentially private) calculation of SSE given database d
        :param d: database object
        :return: SSE
        """
        means = self.group_means(d)
        sse = 0
        for i in range(d.n_groups):
            for j in range(len(d.data[i])):
                sse += (d.data[i][j] - means[i]) ** 2
        return sse

    def public_SSA(self, d):
        """
        Public (non-differentially private) calculation of SSA given database d
        :param d: database object
        :return: SSA
        """
        means = self.group_means(d)
        grand_mean = self.mean(d)
        ssa = 0
        for i in range(d.n_groups):
            ssa += d.group_sizes[i] * (means[i] - grand_mean) ** 2
        return ssa

    def private_SSE(self, d, epsilon, epsilon_frac):
        """
        (epsilon_frac*epsilon)-differentially private calculation of SSE given database d
        :param d: database object
        :param epsilon: Privacy parameter (for total F-statistic calculation)
        :param epsilon_frac: Percentage of privacy parameter to use in this calculation
        :return: (epsilon_frac * epsilon)-differentially private SSE
        """
        noise = np.random.laplace(0.0, self.SSE_sensitivity()/(epsilon*epsilon_frac))
        return self.public_SSE(d) + noise

    def private_SSA(self, d, epsilon, epsilon_frac):
        """
        (epsilon_frac*epsilon)-differentially private calculation of SSA given database d.
        :param d: database object
        :param epsilon: Privacy parameter (for total F-statistic calculation)
        :param epsilon_frac: Percentage of privacy parameter to use in this calculation
        :return: (epsilon_frac * epsilon)-differentially private SSA
        """
        noise = np.random.laplace(0.0, self.SSA_sensitivity(d.database_size)/(epsilon*epsilon_frac))
        return self.public_SSA(d) + noise

    def public_statistic(self, d):
        """
        Public computation of the ANOVA F-statistic on database d
        :param d: database object
        :return: F-statistic
        """
        MSA = self.public_SSA(d)/(d.n_groups - 1)
        MSE = self.public_SSE(d)/(d.database_size - d.n_groups)
        return [MSA/MSE, MSE]

    def private_statistic(self, d, epsilon, SSA_epsilon_frac):
        """
        Epsilon-differentially private computation of the ANOVA F-statistic on database d
        :param d: database object
        :param epsilon: Privacy parameter
        :param SSA_epsilon_frac: Percentage of privacy parameter allocated to SSA calculation.
        :return: epsilon-private F-statistic
        """
        if epsilon == None:
            return self.public_statistic(d)
        MSA = self.private_SSA(d, epsilon, SSA_epsilon_frac)/(d.n_groups - 1)
        MSE = self.private_SSE(d, epsilon, 1-SSA_epsilon_frac)/(d.database_size - d.n_groups)
        return [MSA/MSE, MSE]

    def null_distribution(self, n_groups, db_size, n_runs, epsilon, SSA_epsilon_frac, variance):
        """
        Null-distribution for F-statistic on database d, run n_runs number of times.
        Uses fact that the null distribution SSA and SSE are distributed according to
        variance times the chi-squared distributions with degrees of freedom n_groups - 1 and
        total size - n_groups respectively.
        :param n_groups: number of groups in database
        :param db_size: total size of the database.
        :param n_runs: number of runs
        :param epsilon: Privacy parameter
        :param SSA_epsilon_frac: Percentage of privacy parameter allocated to SSA calculation
        :param variance: of the null distribution data
        :return: numpy array of null-distribution F-statistics, n_runs long.
        """
        SSA_null = variance * np.random.chisquare(n_groups - 1, n_runs) \
                   + np.random.laplace(0.0, self.SSA_sensitivity(db_size)/(epsilon*SSA_epsilon_frac), n_runs)
        SSE_null = variance * np.random.chisquare(db_size - n_groups, n_runs) \
                   + np.random.laplace(0.0, self.SSE_sensitivity()/(epsilon*(1-SSA_epsilon_frac)), n_runs)
        MSA_null = SSA_null/(n_groups - 1)
        MSE_null = SSE_null/(db_size - n_groups)
        return MSA_null/MSE_null
