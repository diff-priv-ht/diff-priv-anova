import os
# The purpose of this file is to contain all of our filemanagement functions, including creating directories, naming files, reading from files, writing to files, etc.

def new_session_dir(alphas, epsilons, epsilon_num_alloc, generate_data, group_sizes_all,
                    means_all, n_runs_null, n_runs_stat, run_true_devs, runs_est_devs, session_id, stand_devs, stats):
    """
    Creates output directory to store all data from a given "session" or run of the script.
    """
    dir_name = session_id + "_Data"
    if os.access(dir_name, os.F_OK):
        raise Exception("Session already exists. Rename session or delete previous session.")
    else:
        os.mkdir(dir_name, mode=0o777)
        make_param_file(alphas, epsilons, epsilon_num_alloc, generate_data, group_sizes_all,
                        means_all, n_runs_null, n_runs_stat, run_true_devs, runs_est_devs, session_id, stand_devs, stats)
    
def make_param_file(alphas, epsilons, epsilon_num_allocs, generate_data, group_sizes_all,
                    means_all, n_runs_null, n_runs_stat, run_true_devs, run_est_devs, session_id, stand_devs, stats):
    """
    Creates an output file to store all of the parameter settings for the current session.
    """
    filename = session_id + "_Parameters.txt"
    dir_name = session_id + "_Data"
    path = dir_name + "/" + filename
    with open(path, "w") as file:
        stats_list = []
        for stat in stats:
            stats_list.append(stat.name)
        file.write("session_id = " + session_id + "\n")
        file.write("stats = " + str(stats_list) + "\n")
        file.write("generate_data = " +str(generate_data) + "\n")
        file.write("run_true_devs = " + str(run_true_devs) + "\n")
        file.write("run_est_devs = " + str(run_est_devs) + "\n")
        file.write("n_runs_null = " + str(n_runs_null) + "\n")
        file.write("n_runs_stat = " + str(n_runs_stat) + "\n")
        file.write("stand_devs = " + str(stand_devs) + "\n")
        file.write("group_sizes_all = " + str(group_sizes_all) + "\n")
        file.write("means_all = " + str(means_all) + "\n")
        file.write("epsilons = " + str(epsilons) + "\n")
        file.write("epsilon_num_allocs = " + str(epsilon_num_allocs) + "\n")
        file.write("alphas = " + str(alphas))


def name_output_file(epsilon, epsilon_num_alloc, group_sizes_db, \
                    means_db, session_id, stand_dev, stat, alpha=None, stand_dev_true = None):
    """
    This creates the names for all of the files we will write. It is a full description
    of the database parameters that produced the data.
    """
    means_str = ""
    for mean in means_db:
        means_str += str(mean) + "-"
    means_str = means_str[:-1]

    group_sizes_str = ""
    for group in group_sizes_db:
        group_sizes_str += str(group) + "-"
    group_sizes_str = group_sizes_str[:-1]
        
    name = str(session_id) + "_" + stat.name +  "_epsilon_" + str(epsilon) + "_epsilonNumAlloc_" + str(epsilon_num_alloc) + \
    "_groupSizesDb_" + group_sizes_str + "_meansDb_" + means_str + "_standDev_" +  str(stand_dev) 
    if alpha is not None:
        name += "_alpha_" + str(alpha)
    if stand_dev_true is not None:
        name += "_standDevTrue_" + str(stand_dev_true)
    name += "_.csv"

    return name

def parameters_from_filename(filename):
    """
    Returns a dict of attributes for the given filename. Note that the numerical attributes like
    stand_dev will be returned as numbers.
    """
    params = {}
    # Get the session ID and name of statistic
    s = filename.split("_")
    params["session_id"] =  s[0] 
    params["stat.name"] = s[1] + "_" + s[2]

    # The rest of the parameters
    index = s.index("epsilon")
    params["epsilon"] = float(s[index+1])

    index = s.index("epsilonNumAlloc")
    params["epsilon_num_alloc"] = stringToArrayOrFloat(s[index+1])

    index = s.index("groupSizesDb")
    params["group_sizes_db"] = dash_delimited_to_list(s[index+1])

    index = s.index("meansDb")
    params["means_db"] = dash_delimited_to_list(s[index+1])

    index = s.index("standDev")
    params["stand_dev"] = float(s[index+1])

    if "standDevTrue" in s:
        index = s.index("standDevTrue")
        params["stand_dev_true"] = s[index+1]
      
    if "alpha" in s:
        index = s.index("alpha")
        params["alpha"] = float(s[index+1])

    return params 

def dash_delimited_to_list(dash_delimited):
    """
    Takes a dash-delimited string of floatsand converts it to a list of floats.
    i.e. stringlist = '1-2-3' and returns [1,2,3]
    """
    list_of_strings = dash_delimited.split("-")
    list_of_floats = [float(i) for i in list_of_strings]
    return list_of_floats

def make_output_folder(session_id, stat, isEst, isNull, isResults, isGraphing=False):
    """
    Creates output file to store all of the csv's specific to the stat
    """
    dir_name = stat.name
    tags = ""
    if isNull:
        tags += "_null"
        
    if isEst:
        tags += "_estimate"
    else:
        tags += "_true"
    
    if isResults:
        tags += "_results"
        
    if isGraphing:
        tags += "_graphs"
        
    if tags == "_true":
        tags = "_output"

    dir_name = dir_name + tags
    if not os.access(session_id + "_Data/" + dir_name, os.F_OK):     # Checks if directory already exists
        os.mkdir(session_id + "_Data/" + dir_name, mode=0o777)       # General read/write mode

    return dir_name

    
def file_matches_traits(eps, epsilon_num_alloc, group_sizes_db, filename, means_db, 
                    stand_dev, session_id, stat, alpha=None):
    """
    This function checks whether a given filename has the correct parameters. 
    """
    parameters = parameters_from_filename(filename)
    isEpsilon = (parameters["epsilon"] == eps)
    isEpsNumAlloc = (parameters["epsilon_num_alloc"] == epsilon_num_alloc)
    isGroupSizes = (parameters["group_sizes_db"] == group_sizes_db)
    isMeansDb = (parameters["means_db"] == means_db)
    isStandDev = (parameters["stand_dev"] == stand_dev)
    isSession = (parameters["session_id"] == session_id)
    isStat = (parameters["stat.name"] == stat.name)
    isAlpha = True
    if alpha is not None:
        isAlpha = (parameters["alpha"] == alpha)
    
    if isAlpha and isEpsilon and isEpsNumAlloc and isGroupSizes and \
    isMeansDb and isStandDev and isSession and isStat:
        return True
    else:
        return False
    

def read_stats(eps, epsilon_num_alloc, group_sizes_db, means_db, 
                    session_id, stand_dev, stat,isEst=False, isNull=False, isResults=False):
    """
    This function reads in the results from output files and results files and returns them as a list
    """

    dir_name = session_id + "_Data/" + make_output_folder(session_id, stat, isEst, isNull, isResults)
    all_files = os.listdir(dir_name)
    to_read = []
    for filename in all_files:
        if filename[-4:] == ".csv": # only read .csv files
            if file_matches_traits(eps, epsilon_num_alloc, group_sizes_db, filename, means_db, 
                    stand_dev, session_id, stat, alpha=None):
                    to_read.append(filename)
    
    stats = [_read_stats(dir_name + "/" + filename) for filename in to_read]
    return stats

def _read_stats(filepath):
    """
    Reads statistic values from a file and stores in a numpy array
    :param filepath: path to a file (string)
    :return: array of fstat
    """
    with open(filepath, "r") as file:
        first_line_length = len(file.readline().split(","))
        file.seek(0, 0)  # This resets the filepointer to the top of the document
        out = []
        if first_line_length != 1:
            for i in range(first_line_length):
                out.append([])
            for line in file:
                current_line = line.split(",")
                for count, data in enumerate(current_line):
                    out[count].append(float(data))
        else:
            for line in file:               # If only one thing per line of file
                out.append(float(line))
    return out

def write_coordinates(plotting_parameters, session_id, x_coordinates, x_parameter_name,  y_coordinates, isEst):
    """
    This function creates a file with the x-coordinates and y-coordinates, along with the 
    parameters that led to that plot.
    """
    
    save_x_parameter = plotting_parameters[x_parameter_name]
    plotting_parameters[x_parameter_name] = plotting_parameters[x_parameter_name][0] # This keeps us from having long lists in the filenames
    if x_parameter_name == "database_sizes":
        save_dbs = plotting_parameters["group_sizes_all"]
        plotting_parameters["group_sizes_all"] = plotting_parameters["group_sizes_all"][0] # we will just name the file with the first group
        
    epsilon = plotting_parameters["epsilons"]
    epsilon_num_alloc = plotting_parameters["epsilon_num_allocs"]
    group_sizes_db = plotting_parameters["group_sizes_all"]
    means_db = plotting_parameters["means_all"]
    stand_dev = plotting_parameters["stand_devs"]
    stat = plotting_parameters["stats"]
    alpha = plotting_parameters["alphas"]
    
    
    dir_name = session_id + "_Data/" + make_output_folder(session_id, stat, isEst, isNull=False, isResults=False, isGraphing=True)
    filename = name_output_file(epsilon, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat, alpha)

    plotting_parameters[x_parameter_name] = save_x_parameter
    if x_parameter_name == "database_sizes":
        plotting_parameters["group_sizes_all"] == save_dbs
    
    x_coordinates_str = str(x_coordinates)[1:-1]
    y_coordinates_str = str(y_coordinates)[1:-1]
    with open(dir_name + "/" + filename, "w") as file:
        file.write(x_coordinates_str + "\n")
        file.write(y_coordinates_str + "\n")
        file.write("Parameters \n")
        file.write("x_parameter, " + x_parameter_name + "\n")
        file.write("n_runs_null, " + str(plotting_parameters["n_runs_null"]) + "\n")
        file.write("n_runs_stat, " + str(plotting_parameters["n_runs_stat"]) + "\n")
        file.write("alpha, " + str(plotting_parameters["alphas"]) + "\n")
        file.write("epsilon, " + str(plotting_parameters["epsilons"]) + "\n")
        file.write("epsilon_num_alloc, " + str(plotting_parameters["epsilon_num_allocs"]) + "\n")
        file.write("group_sizes_db, " + str(plotting_parameters["group_sizes_all"]) + "\n")
        file.write("means_db, " + str(plotting_parameters["means_all"]) + "\n")
        file.write("stand_dev, " + str(plotting_parameters["stand_devs"]) + "\n")
        file.write("stat, " + stat.name)
        
        
def read_coordinates(alpha, eps, epsilon_num_alloc, group_sizes_db, means_db, session_id, stand_dev, stat, isEst):
    """
    This function reads in the coordinates according to the database parameters. We had to write a separate function from
    read_stats() because the coordinates files also contain all of the database specifications and other parameters.
    """
    dir_name = session_id + "_Data/" + make_output_folder(session_id, stat, isEst, isNull=False, isResults=False, isGraphing=True)
    all_files = os.listdir(dir_name)
    to_read = []

    for filename in all_files:
        if filename[-4:] == ".csv": # only read .csv's
            if file_matches_traits(eps, epsilon_num_alloc, group_sizes_db, filename, means_db, stand_dev, session_id, stat, alpha):
                to_read.append(filename)
    coordinates = [_read_coordinates(dir_name + "/" + filename) for filename in to_read]
    return coordinates
            
                     
def _read_coordinates(filename):
    """
    Read coordinates for a file.
    """
    with open(filename, "r") as file:
        str_x_coordinates = file.readline().split("\n")[0].split(",")
        x_coordinates = []
        for coord in str_x_coordinates:
            x_coordinates.append(float(coord))
        str_y_coordinates = file.readline().split("\n")[0].split(",")
        y_coordinates = []
        for coord in str_y_coordinates:
            y_coordinates.append(float(coord))
    return [x_coordinates, y_coordinates]

def stringToArrayOrFloat(string):
    """
    Helper function, inputs string that may either be returned as float or as an
    array of floats. 
    E.g. stringToArrayOrFloat("0.5") returns 0.5
         stringToArrayOrFloat("[0.5,0.2]") returns [0.5,0.2]
    """ 
    if string[0]=="[" and string[-1]=="]":
        nums = string[1:-1].split(",")
        return [float(n) for n in nums]
    else:
        return float(string)