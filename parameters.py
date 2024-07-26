NUM_SERVERS = 1000 # DEFAULT = 1000
ARRIVAL_RATE = .7 * NUM_SERVERS  # DEFAULT .5
d = 5 # number of queried servers       DEFAULT = 5
s = 1 # number of classes of server speeds
TOTAL_JOBS = 10**5 # DEFAULT = 10**5
EQUILIBRIUM_AMOUNT = 10**4
THRESHOLD = 5       # DEFAULT = 5
NUM_DISPATCHER = 5 # DEFAULT = 5

#Policies:
x = 2
inform_dispatcher_policy_list = ["Uniform random select one", "Query x", "Notify x"] 
INFORM_DISPATCHER_POLICY = inform_dispatcher_policy_list[0] # used in choose_Dispatchers_To_Inform()

select_server_policy_list = ["Fastest idle server", "Query faster servers", "M/M/N"] 
SELECT_SERVER_POLICY = select_server_policy_list[0] # Used in pick_server(), DEFAULT is "Fastest idle server"


if s == 1:
    NUM_SERVERS_PER_CLASS = [1000]
    SERVICE_RATE_PER_CLASS = [1]
if s == 2:
    NUM_SERVERS_PER_CLASS = [91, 909]
    SERVICE_RATE_PER_CLASS = [10, 1/10]