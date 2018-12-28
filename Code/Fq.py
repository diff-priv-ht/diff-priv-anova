from TestStatistic import *
import Database
import numpy as np
import math


class Fq(TestStatistic):
    def __init__(self, exponent):
        if exponent  <= 0:
            # Our sensitivity analysis and privacy guarantee don't hold for non-positive exponents.
            raise Exception("Exponent must be positive.")
        self.exponent = exponent
        self.name = "F_" + str(exponent)

    def SQE_sensitivity(self, database_size):
        """
        :return: Provable upper bound on sensitivity of SQE for given exponent
        """
        if self.exponent < 1:
            sensitivity = 1 + 2*((database_size/2)**(1 - self.exponent))
        else: 
            sensitivity = 1 + database_size*(1 - (1 - (2/database_size))**self.exponent)
        return sensitivity

    def SQA_sensitivity(self, database_size):
        """
        :return: Provable upper bound on sensitivity of SQA for given exponent.
        """
        if self.exponent < 1:
            sensitivity = 1 + database_size*((3/database_size)**self.exponent)
        else:  
            sensitivity = 1 + database_size*(1 - (1 - (3/database_size))**self.exponent)
        return sensitivity


    def public_SQE(self, d):
        """
        Public (non-differentially private) calculation of SQE given database d and an exponent.
        :param d: database object
        :return: SQE
        """
        means = self.group_means(d)
        spe = 0
        for i in range(d.n_groups):
            for j in range(len(d.data[i])):
                spe += (abs(d.data[i][j] - means[i]))**self.exponent
        return spe

    def public_SQA(self, d):
        """
        Public (non-differentially private) calculation of SQA given database d.
        :param d: database object
        :return: SQA
        """
        means = self.group_means(d)
        grand_mean = self.mean(d)
        spa = 0
        for i in range(d.n_groups):
            tmp = abs(means[i] - grand_mean)**self.exponent
            spa += d.group_sizes[i] * tmp
        return spa

    def private_SQE(self, d, epsilon, epsilon_num_alloc):
        """
        (epsilon_frac*epsilon)-differentially private calculation of SQE given database d.
        :param d: database object
        :param epsilon: Privacy parameter (for total F-statistic calculation)
        :param epsilon_frac: Percentage of privacy parameter to use in this calculation
        :return: (epsilon_frac * epsilon)-differentially private SQE
        """
        noise = np.random.laplace(0.0, self.SQE_sensitivity(d.database_size)/(epsilon*epsilon_num_alloc))
        return self.public_SQE(d) + noise

    def private_SQA(self, d, epsilon, epsilon_num_alloc):
        """
        (epsilon_frac*epsilon)-differentially private calculation of SQA given database d.
        :param d: database object
        :param epsilon: Privacy parameter (for total F-statistic calculation)
        :param epsilon_frac: Percentage of privacy parameter to use in this calculation
        :return: (epsilon_frac * epsilon)-differentially private SQA
        """
        noise = np.random.laplace(0.0, self.SQA_sensitivity(d.database_size)/(epsilon*epsilon_num_alloc))
        return self.public_SQA(d) + noise

    def public_statistic(self, d):
        """
        Public computation of the ANOVA F-statistic on database d
        :param d: database object
        :return: Fq-statistic
        """
        msa = self.public_SQA(d)/(d.n_groups - 1)
        mse = self.public_SQE(d)/(d.database_size - d.n_groups)
        return [msa/mse, mse]

    @staticmethod
    def standdev_estimate(mse):
        """
        ToDo: Need different standard deviation estimate probably...?
        Returns an estimate of the standard deviation of our database. We use this estimate since
        our F-statistic comes from the half-normal distribution.
        :param mse: absolute value version of mse
        :return: estimate of standard deviation according to half-normal distribution.
        """
        
        standdev = math.sqrt(math.pi / 2) * mse
        return standdev

    def private_statistic(self, d, epsilon, epsilon_num_alloc):
        """
        Epsilon-differentially private computation of the ANOVA F-statistic on database d
        :param d: database object
        :param epsilon: Privacy parameter
        :param SQA_eps_frac: Percentage of privacy parameter allocated to SSA calculation.
        :return:
        """
        if epsilon == None:
            return self.public_statistic(d)
        msa = self.private_SQA(d, epsilon, epsilon_num_alloc)/(d.n_groups - 1)
        mse = self.private_SQE(d, epsilon, 1 - epsilon_num_alloc)/(d.database_size - d.n_groups)
        standdev_est = self.standdev_estimate(mse)
        return [msa/mse, standdev_est]
