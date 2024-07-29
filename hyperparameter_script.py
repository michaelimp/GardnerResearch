import subprocess
import shutil
import os

# Back up the original parameter file
shutil.copyfile('parameters.py', 'parameters_backup.py')

# Define the values of ARRIVAL_RATE to test
arrival_rate_multipliers = [.2, .4, .6, .8, .9]  # Multipliers for NUM_SERVERS
total_fast_capacity_values = [100, 300, 600, 900]
num_fast_values = [100, 400]


def get_fixed_parameter_value(param_name):
    # Read the value of a fixed parameter from parameters.py
    with open('parameters.py', 'r') as file:
        lines = file.readlines()
    for line in lines:
        if line.strip().startswith(param_name):
            # Extract the value part from the variable declaration
            return line.strip().split('=')[1].strip()
    return "Not Found"

def modify_parameters(arrival_rate_multiplier, total_fast_capacity, num_fast):
    # Modify parameters.py with the new ARRIVAL_RATE
    with open('parameters.py', 'r') as file:
        lines = file.readlines()
    
    with open('parameters.py', 'w') as file:
        for line in lines:
            if line.strip().startswith('ARRIVAL_RATE'):
                file.write(f'ARRIVAL_RATE = {arrival_rate_multiplier} * NUM_SERVERS  # Adjusted\n')
            elif line.strip().startswith('TOTAL_FAST_CAPACITY'):
                file.write(f'TOTAL_FAST_CAPACITY = {total_fast_capacity}  # Adjusted\n')
            elif line.strip().startswith('NUM_FAST'):
                file.write(f'NUM_FAST = {num_fast}  # Adjusted\n')
            else:
                file.write(line)

def run_simulation(arrival_rate_multiplier, total_fast_capacity, num_fast):
    # Run the simulation script
    result = subprocess.run(['python', '9_Record_Proportion.py'], capture_output=True, text=True)
    #print("Simulation output: \n", result.stdout)
    with open('all_results.txt', 'a') as f:
        f.write(f"Results for ARRIVAL_RATE = {arrival_rate_multiplier}, TOTAL_FAST_CAPACITY = {total_fast_capacity}, NUM_FAST = {num_fast} \n")
        f.write(result.stdout + "\n\n")

# Prepare the results file and write fixed parameter values
results_file_path = 'all_results.txt'
if os.path.exists(results_file_path):
    os.remove(results_file_path)

# Get fixed parameter value
num_dispatcher = get_fixed_parameter_value('NUM_DISPATCHER')

# Write the fixed parameter value to the file
with open(results_file_path, 'w') as f:
    f.write(f"Fixed Parameter - NUM_DISPATCHER: {num_dispatcher}\n\n")
    
# Main execution loop
for rate in arrival_rate_multipliers:
    for capacity in total_fast_capacity_values:
        for num_fast in num_fast_values:
            print(f"Testing with ARRIVAL_RATE = {rate}, TOTAL_FAST_CAPACITY = {capacity}, NUM_FAST = {num_fast}")
            modify_parameters(rate, capacity, num_fast)
            run_simulation(rate, capacity, num_fast)

# Restore the original parameter file
shutil.copyfile('parameters_backup.py', 'parameters.py')
