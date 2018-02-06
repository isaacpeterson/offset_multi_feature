library(offsetsim)

source('simulated_data_params.R')

user_simulation_params = initialise_user_simulation_params()
user_global_params = initialise_user_global_params()
user_simulated_ecology_params = initialise_user_simulated_ecology_params()
user_plot_params <- initialise_user_plot_params()

osim.run(user_global_params, user_simulation_params, user_simulated_ecology_params, loglevel = 'TRACE')
current_simulation_folder = find_current_run_folder(user_global_params$simulation_folder)
#include run_number for specified run folder - leave to automatically select latest
osim.plot(user_plot_params, current_simulation_folder, loglevel = 'TRACE')