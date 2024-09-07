NUM_SERVERS = 1000 # DEFAULT = 1000
ARRIVAL_RATE = .7 * NUM_SERVERS  # DEFAULT .5
d = 5 # number of queried servers       DEFAULT = 5
s = 1 # number of classes of server speeds
TOTAL_JOBS = 10**5 # DEFAULT = 10**5
EQUILIBRIUM_AMOUNT = 10**4  # DEFAULT = 10**4
NUM_OBSERVATIONS = TOTAL_JOBS - EQUILIBRIUM_AMOUNT
THRESHOLD = 5       # DEFAULT = 5
NUM_DISPATCHER = 5 # DEFAULT = 5
TOTAL_CAPACITY = NUM_SERVERS * 1

# for s = 2
NUM_FAST = 100
NUM_SLOW = NUM_SERVERS - NUM_FAST
TOTAL_FAST_CAPACITY = 900
TOTAL_SLOW_CAPACITY = TOTAL_CAPACITY - TOTAL_FAST_CAPACITY

#Policies:
x = 2
inform_dispatcher_policy_list = ["Uniform random select one", "Query x", "Notify x"] 
INFORM_DISPATCHER_POLICY = inform_dispatcher_policy_list[0] # used in choose_Dispatchers_To_Inform()

select_server_policy_list = ["Fastest idle server", "Query faster servers", "M/M/N"] 
SELECT_SERVER_POLICY = select_server_policy_list[2] # Used in pick_server(), DEFAULT is "Fastest idle server"


if s == 1:
    NUM_SERVERS_PER_CLASS = [1000]
    SERVICE_RATE_PER_CLASS = [1]
if s == 2:
    # NUM_SERVERS_PER_CLASS = [91, 909]
    # SERVICE_RATE_PER_CLASS = [10, 1/10]
    
    NUM_SERVERS_PER_CLASS = [NUM_FAST, NUM_SLOW]
    
    def calculate_service_rate():
        return [TOTAL_FAST_CAPACITY / NUM_FAST, TOTAL_SLOW_CAPACITY / NUM_SLOW]
    
    SERVICE_RATE_PER_CLASS = calculate_service_rate()
    
    
def error_checks():
    if NUM_SERVERS != sum(NUM_SERVERS_PER_CLASS):
        print('ERROR: number of servers dont add up')
    
    if TOTAL_CAPACITY != sum([num_servers * service_rate for num_servers, service_rate in zip(NUM_SERVERS_PER_CLASS, SERVICE_RATE_PER_CLASS)]):
        print('ERROR: total capacity is off')