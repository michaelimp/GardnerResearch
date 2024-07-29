def extract_idle_server_proportions(line):
    # Function to extract the proportion of idle servers from a line
    start = line.find('[') + 1
    end = line.find(']')
    if start > 0 and end > 0:
        proportions = line[start:end].split(',')
        if len(proportions) == 2:
            return [float(proportions[0].strip()), float(proportions[1].strip())]
    return [0.0, 0.0]  # Default return if parsing fails

def compare_idle_server_proportions(file1, file2):
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        lines1 = f1.readlines()
        lines2 = f2.readlines()

        # Assuming both files have the same number of lines where results are stored
        for i in range(min(len(lines1), len(lines2))):
            if 'proportion of idle servers' in lines1[i]:
                proportions1 = extract_idle_server_proportions(lines1[i])
                proportions2 = extract_idle_server_proportions(lines2[i])

                # Compare the proportions and check the difference threshold
                if abs(proportions1[0] - proportions2[0]) > 0.07 or abs(proportions1[1] - proportions2[1]) > 0.07:
                    print(f"Significant difference found on line {i+1}:\nFile 1: {lines1[i].strip()}\nFile 2: {lines2[i].strip()}\n")

# Specify the filenames
file_one = 'all_results_five_dispatcher.txt'
file_two = 'all_results_one_dispatcher.txt'

# Call the function to compare the files
compare_idle_server_proportions(file_one, file_two)
