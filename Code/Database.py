import numpy as np


class Data:
    """
    This class holds our data, either imported from some external source, or
    generated from a specified distribution.
    """
    def __init__(self, group_sizes):
        self.database_size = sum(group_sizes)
        self.group_sizes = group_sizes
        self.n_groups = len(group_sizes)
        self.distribution_type = ""
        self.data = []

    def generate_data(self):
        pass


class Normal(Data):
    """
    This data object generates data from a Normal distribution.
    """
    Data.distribution_type = "Normal"

    def __init__(self, group_sizes, means_list, stand_dev):
        """
        :param database_size: Total size of database.
        :param group_sizes: Array containing sizes of each group.
        :param means_list: Array containing population means for each group.
        :param stand_dev: Standard deviation of the groups as a float.
        """
        Data.__init__(self, group_sizes)
        self.means_list = means_list
        self.stand_dev = stand_dev
        self.generate_data()

    def generate_data(self):
        """
        Generates normally-distributed data with appropriate dimensions and specified
        population group means and standard deviation.
        :return:
        """
        self.data =[]
        
        for group in range(len(self.means_list)):
            rand_vars = np.array(np.random.normal(self.means_list[group], self.stand_dev, self.group_sizes[group]))
            clipped = np.clip(rand_vars, 0, 1)        # This limits the range of datapoints to [0,1]
            self.data.append(clipped)
        return None



