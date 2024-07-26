import subprocess
import shutil

# Back up the original parameter file
shutil.copyfile('parameters.py', 'parameters_backup.py')

# Define the values of ARRIVAL_RATE to test
arrival_rates = [.2, .4, .6, .8, .9]  # Multipliers for NUM_SERVERS

def modify_parameters(arrival_rate_multiplier):
    # Modify parameters.py with the new ARRIVAL_RATE
    with open('parameters.py', 'r') as file:
        lines = file.readlines()
    
    with open('parameters.py', 'w') as file:
        for line in lines:
            if line.startswith('ARRIVAL_RATE'):
                file.write(f'ARRIVAL_RATE = {arrival_rate_multiplier} * NUM_SERVERS  # Adjusted\n')
            else:
                file.write(line)

def run_simulation():
    # Run the simulation script
    result = subprocess.run(['python', '9_Record_Proportion.py'], capture_output=True, text=True)
    print("Output of simulation with ARRIVAL_RATE multiplier:", result.stdout)

# Main execution loop
for rate in arrival_rates:
    print("Testing with ARRIVAL_RATE =", rate, "* NUM_SERVERS")
    modify_parameters(rate)
    run_simulation()

# Restore the original parameter file
shutil.copyfile('parameters_backup.py', 'parameters.py')
