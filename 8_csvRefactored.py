# Based off of IdleList.py
#Order: 1. MM1-Simulator 2.selectingRandomServer 3. JoinShortestQueue-d.py 4. DifferentSpeedServers.py 5. QueryingRuleUpdated 6. AssignmentRule.py 7. IdleList.py
# 8. csvRefactored (makes use of csvGenerator)

# Index 0 is the fastest server class, the last index is the slowest.

import random
import operator as op
from functools import reduce
import numpy as np
import pandas as pd
import sys
from typing import List
from parameters import *
from csvGenerator import generate_querying_probability_csv


# Parameters:
NUM_SERVERS = NUM_SERVERS
ARRIVAL_RATE = ARRIVAL_RATE
d = d
s = s
x = x
TOTAL_JOBS = TOTAL_JOBS
THRESHOLD = THRESHOLD
NUM_DISPATCHER = NUM_DISPATCHER

#File names
QUERYING_CSV = "queryingRuleProbabilities.csv"
INFORM_DISPATCHER_POLICY = INFORM_DISPATCHER_POLICY
SELECT_SERVER_POLICY = SELECT_SERVER_POLICY




class Dispatcher:
    def __init__(self, index):
        self.index = index
        self.bigIdleList = [[] for _ in range(s)] # List of the lists of idle servers the dispatcher knows about. [0] corresponds to fastest idle server list

class Server:
    def __init__ (self, speed, speed_class, index):
        self.index = index
        self.queued_jobs = []
        self.next_departure_time = sys.maxsize # Each server hold onto its own next departure time (when its first job finishes) 
        self.speed = speed # Possible speed values: .5, 1, 1.5, 2, larger is faster
        self.speed_class = speed_class # Ranges from 0 through s-1, with 0 being the fastest
        self.dispatcher_indexes = [] # Empty list means it does not belong to any dispatcher's idle list
        self.time_to_idle = sys.maxsize

def generate_new_interarrival(arrival_rate):
        return np.random.exponential(1 / arrival_rate)

class Job:
    def __init__ (self, arrival_time, server_speed):
        def generate_new_job_size(server_speed):
            return np.random.exponential(1/server_speed)
        
        self.arrival_time = arrival_time
        self.time_for_completion = generate_new_job_size(server_speed)
#______________________________________________________________________________________________
# Querying

def read_querying_csv():
    '''Returns a 2D Numpy array where each row is the query mix with the next column as the corresponding 
    probability of it occurring with no idle list, then the next column where there is slowest idle server,
    and on and on until last column is the probability when there is the fastest idle server (only query mix 
    that has probability of 1 is (d, 0, 0... 0))
    The query mix goes from fastest to slowest per column index
    :rtype: 2D numpy array
    '''
    df = pd.read_csv(f'{QUERYING_CSV}', header = None)
    return df.to_numpy()

def pick_query_mix(A, fastest_existing_idle_speed: int):
    ''' Randomly picks query mix based on what the fastest known idle server class is (only include faster 
    server classes in query)
    :param: 2D numpy array, the fastest known idle server class
    :return: 1D numpy row
    :rtype: numpy row
    '''
    probability = random.random()
    total = 0
    
    columnIndex = fastest_existing_idle_speed
    if fastest_existing_idle_speed == None: # We want to look at column corresponding to 
        columnIndex = s - 1
    
    for i in range(len(A)):
        total += A[i][-(columnIndex + 1)]
        if total >= probability:
            return A[i][:-s] # Return just the query mix, exclude the probability
    
#________________________________________________________________________________
# Create assignment hashmap, assignment protocol
list_of_keys = [] # to aid in creation of assignment hashmap

def key_gen(q_lengths, threshold, server_class, current_threshold):
    '''key_gen updates list_of_keys to contain all the possible keys. A key is vector of length s, 
    where index 0 corresponds to fastest server class.
    Each index corresponds to the shortest queue length for each server class.
    Index 0 goes from 0 to threshold, and sys.max (which would occur when the fastest server class is never queried).
    Index 1 must be less than index 0, or it is sys.max
    An example key where threshold = 5, s = 4, would be: [5, sys.max, 1, 0]. Notice it is strictly decreasing
    An index could equal the global threshold, but always must be smaller than the local threshold 
    (the local threshold as determined by the shortest length of servers faster than it)
    Return: nothing, just updates list_of_keys 
    '''
    if server_class == s: #base case
        #This step is necessary to handle issues with memory referencing: 
        deepCopy = []
        for i in q_lengths:
            deepCopy.append(i)
        #__________________
        list_of_keys.append(deepCopy)
        return
    
    for i in range(threshold + 2):
        if i > current_threshold: 
            return
        if i == current_threshold:
            q_lengths[server_class] = sys.maxsize
            localThreshold = current_threshold
        elif (i == threshold + 1):
            q_lengths[server_class] = sys.maxsize
            localThreshold = current_threshold
        else:
            q_lengths[server_class] = i
            localThreshold = q_lengths[server_class]
        key_gen(q_lengths,threshold,server_class+1, localThreshold)
        


def create_assignment_hashmap():
    ''' Currently implements a shortest queue length assignment rule
    Return: dictionary with keys as described above, and the value a probability distribution
    (vector of length s, sums to 1, at each index is probability that that server class is assigned the job)
    rtype: dictionary
    '''
    qLengths = [0]*s
    key_gen(qLengths, THRESHOLD, 0, THRESHOLD+1) # creates listOfKeys
    assignmentHashmap = {}
    for key in list_of_keys:
        value = [0]*s # Probability distribution, vector [0,0...0] of length s
        value[key.index(min(key))] = 1 # Make the shortest queue length server class have probability 1 of being picked
        hashableKey = tuple(key) # list is non hashable, must convert to tuple
        assignmentHashmap[hashableKey] = value
    return assignmentHashmap

def create_assignment_key(selected_query_mix, queried_servers):
    ''' Creates assignment key based on the selected query mix that we'll hash into assignmentHashmap
    ptype: list of queriedServers
    return: assignment key
    rtype: list of len(s)'''
    assignmentKey = [0]*s 
    # Determine assignment key
    tracker = 0
    for i in range(s): # Puts shortest queue length of each server class into assignment key
        numThisClass = selected_query_mix[i]
        if numThisClass == 0: # If that server class was not queried
            assignmentKey[i] = sys.maxsize
        else: # Finds the shortest queue length of the class
            queriedServersOfSameClass = queried_servers[int(tracker):int(tracker + numThisClass)]
            minLength = sys.maxsize
            for server in queriedServersOfSameClass:
                if len(server.queued_jobs) <= minLength:
                    minLength = len(server.queued_jobs)
            assignmentKey[i] = minLength
            tracker += numThisClass
    # Finishes fixing assignment key
    localThreshold = THRESHOLD + 1
    for i in range(len(assignmentKey)): 
        if assignmentKey[i] == sys.maxsize: # This means that server class was not queried, so we don't want to do anything
            continue
        if assignmentKey[i] >= THRESHOLD: # Treat any length above threshold as at threshold
            assignmentKey[i] = THRESHOLD
        if assignmentKey[i] >= localThreshold: # If equal or above local threshold, consider it as completely unviable
            assignmentKey[i] = sys.maxsize
        else: # This would mean we have new shortest length to determine local threshold
            localThreshold = assignmentKey[i] 
    return assignmentKey
               
def pick_server_from_query_mix(probability_distribution, selectedQueryMix, queriedServers):
    '''Pick the server to assign the job to
    param probabilityDistribution: list of len(s), index corresponds to probability that server class is assigned
    return: server (shortest queue length server of the picked server class)
    '''
    probability = random.random()
    total = 0
    for i in range(len(probability_distribution)): # Index will be the server class picked
        total += probability_distribution[i]
        if total >= probability:
            tracker = 0
            for j in range(len(selectedQueryMix)):
                numThisClass = selectedQueryMix[j]
                if i == j: # When server class index matches up
                    queriedServersOfPickedClass = queriedServers[int(tracker):int(tracker + numThisClass)]
                    # Pick shortest queue length of that server class
                    minLength = sys.maxsize
                    for server in queriedServersOfPickedClass:
                        if len(server.queued_jobs) <= minLength:
                            minLength = len(server.queued_jobs)
                    for server in queriedServersOfPickedClass:
                        if len(server.queued_jobs) == minLength:
                            return server
                tracker += numThisClass
    
    
#________________________________________________________________________________
# Creating servers, dispatchers, servers_PQ
def create_servers():
    '''Returns a dictionary where each key is index ranging from 0 to s-1, with index 0 being fastest
    Currently cannot handle if s does not evenly divides numServers (eg fails if s = 3, numServers = 100, will only produce 99 servers)
    :return: Dictionary with key indicating server class and value being list of servers
    :rtype: Dictionary (key = int, value = list of servers)
    '''
    servers = {}
    if s == 1:
        speed = 1
    else:
        speed = 2
    index = 0
    for i in range(s):
        listOfServers = []
        for _ in range(NUM_SERVERS//s):
            listOfServers.append(Server(speed= speed, speed_class=i, index= index))
            index += 1
        servers[i] = listOfServers
        speed -=.5
    return servers


def create_dispatchers(servers: dict) -> List[Dispatcher]:
    def partition_list(list, n):
        """Partition a list as evenly as possible into n groups."""
        q, r = divmod(len(list), n)
        indices = [q * i + min(i, r) for i in range(n + 1)]
        return [list[indices[i]:indices[i+1]] for i in range(n)]

    '''Returns a list of dispatchers, distributes the initial idle servers evenly among dispatchers'''
    dispatchers = []
    for i in range(NUM_DISPATCHER):
        dispatchers.append(Dispatcher(index=i))
    # Distributes initial idle servers evenly among dispatchers
    for i in range(s): # If there are 25 fast servers and 3 dispatchers, we give two 8 and one 9
        sameClassServers = servers[i] # List of servers of the same class speed
        partitionedServers = partition_list(sameClassServers,NUM_DISPATCHER) # List of length numDispatcher, composed of lists that are as evenly lengthed as possible
        for j in range(len(partitionedServers)): # j is the dispatcher index
            for server in partitionedServers[j]: 
                server.dispatcher_indexes.append(j)
            dispatchers[j].bigIdleList[i] = partitionedServers[j]
    return dispatchers

def create_servers_priority_queue(servers_as_list: List[Server]):
    '''Returns priority queue of servers based on their next time to becoming idle. Implemented with sorted list
    If server is already in a dispatcher idle list, it's next time to becoming idle is sys.maxsize
    (nextTimetoIdle, sever.index, server)
    '''
    servers_PQ = []
    for server in servers_as_list:
        servers_PQ.append([server.time_to_idle, server])
    servers_PQ.sort(key= lambda x: x[0])
    return servers_PQ
#________________________________________________________________________________
# Main loop

def finish_jobs_in_servers(servers: List[Server], clock):
    '''Finish up all the jobs that ought to have been finished according to the current time in clock
    Return the time it took, which will be added to totalTime'''
    totalTimeAddition = 0
    for server in servers: 
        while True:
            if server.next_departure_time <= clock: # We need to first finish all the jobs that should have been completed by now in this server
                jobToFinish = server.queued_jobs.pop(0)
                totalTimeAddition += (server.next_departure_time - jobToFinish.arrival_time)
                if len(server.queued_jobs) != 0: # We process the next job
                    server.next_departure_time += server.queued_jobs[0].time_for_completion
                else: # We don't need to keep processing
                    server.next_departure_time = sys.maxsize
            else:
                break
    return totalTimeAddition

def choose_dispatchers_to_inform(dispatchers):
    '''Return list of dispatchers to inform of the idle server based on policy'''
    if len(dispatchers) == 1:
        return [dispatchers[0]]
    if INFORM_DISPATCHER_POLICY == "Uniform random select one":
        return [dispatchers[random.randint(0,NUM_DISPATCHER - 1)]]
    if INFORM_DISPATCHER_POLICY == "Query x": # query x dispatchers, give to dispatcher with fewest known idle servers
        fewest = float('inf')
        toReturn = None
        for dispatcher in dispatchers:
            totalIdleServers = sum(len(idleList) for idleList in dispatcher.bigIdleList)
            if totalIdleServers < fewest:
                fewest = totalIdleServers
                toReturn = dispatcher 
        return [toReturn]
    if INFORM_DISPATCHER_POLICY == "Notify x": # straight up pick x dispatchers to notify of idle server
        return random.sample(dispatchers, x)     
        

def add_idle_servers_to_idle_lists(servers_PQ, dispatchers, clock):
    '''Figure out which servers became idle and add them to dispatchers' idle list'''
    while servers_PQ[0][0] <= clock: # The earliest server to becoming idle is idle
        idleServer = servers_PQ[0][1]
        dispatchersToInform = choose_dispatchers_to_inform(dispatchers)
        for dispatcher in dispatchersToInform:
            dispatcher.bigIdleList[idleServer.speed_class].append(idleServer) # add server to dispatcher idle list
            idleServer.dispatcher_indexes.append(dispatcher.index) # tell server which dispatcher it's sent to    
        idleServer.time_to_idle = sys.maxsize # now server is officially idle
        servers_PQ[0] = [idleServer.time_to_idle, idleServer]
        servers_PQ.sort(key= lambda x: x[0])


def update_idleness_according_to_new_job(server, dispatchers, servers_PQ):
    '''A server's time_to_idle only changes when it receives a new job. This method updates the server's time_to_idle and removes it from its dispatcher's idle lists
    Then it updates servers_PQ'''
    if len(server.dispatcher_indexes) != 0: # The server was idle
        for dispatcherIndex in server.dispatcher_indexes[:]: # have to loop through copy of dispatcher_indexes or will run into issues
            for serv in dispatchers[dispatcherIndex].bigIdleList[server.speed_class]: # Loop through the dispatcher idle list and remove the no-longer idle server
                if serv.index == server.index:
                    dispatchers[dispatcherIndex].bigIdleList[server.speed_class].remove(serv)
                    break
            server.dispatcher_indexes.remove(dispatcherIndex) # server is no longer part of that dispatcher's idle list
        server.time_to_idle = server.queued_jobs[0].arrival_time + server.queued_jobs[0].time_for_completion
        for i, (irrelevant, serv) in enumerate(servers_PQ): # update servers_PQ
            if serv.index == server.index:
                servers_PQ[i] = [server.time_to_idle,server]
                servers_PQ.sort(key= lambda x: x[0])
                break
    else: # The server was not idle
        server.time_to_idle += server.queued_jobs[-1].time_for_completion
        
def pick_server(chosen_dispatcher, fastest_existing_idle_speed, clock, two_D_Array, servers, assignment_hashmap):
    ''' We will pick the server given what we know as the fastest existing idle server.
    Return the picked server and the update to how much time it took to finish the jobs in the server(s)'''
    picked_server = None
    total_time_addition = 0
    
    if SELECT_SERVER_POLICY == "Fastest idle server":
        if fastest_existing_idle_speed is not None:
            picked_server = chosen_dispatcher.bigIdleList[fastest_existing_idle_speed][0]
        else:
            picked_server = random.sample(sum(list(servers.values()),[]), 1)[0]
        
        total_time_addition += finish_jobs_in_servers([picked_server], clock)
        return picked_server, total_time_addition
        
    if SELECT_SERVER_POLICY == "Query faster servers":
        if fastest_existing_idle_speed == 0: # don't even query
            picked_server = chosen_dispatcher.bigIdleList[0][0]
            total_time_addition += finish_jobs_in_servers([picked_server], clock)
        else:    
            selectedQueryMix = pick_query_mix(two_D_Array, fastest_existing_idle_speed) # Numpy array of numbers, each index corresponding to number of servers per that class
            
            queriedServers = [] # List of servers that satisfy the selectedQueryMix
            for i in range(s):
                numThisClass = selectedQueryMix[i]
                queriedServers.extend(random.sample(servers[i], int(numThisClass)))
            
            total_time_addition += finish_jobs_in_servers(queriedServers, clock) # Finish up whatever jobs that should have been finished so the length of each queried server is accurate
            
            assignmentKey = create_assignment_key(selectedQueryMix,queriedServers) # a list
            if 0 not in assignmentKey and fastest_existing_idle_speed != None: # This would mean all queried servers has length > 0, so automatically pick the fastest existing idle server
                picked_server = chosen_dispatcher.bigIdleList[fastest_existing_idle_speed][0]
                total_time_addition += finish_jobs_in_servers([chosen_dispatcher.bigIdleList[fastest_existing_idle_speed][0]], clock) 
            else:
                picked_server = pick_server_from_query_mix(assignment_hashmap[tuple(assignmentKey)],selectedQueryMix, queriedServers) # pick the fastest server class which must be idle
        return picked_server, total_time_addition

#________________________________________________________________________________

def main():
    global NUM_SERVERS, ARRIVAL_RATE, d, s, TOTAL_JOBS, NUM_DISPATCHER
    
    # Set up our probability arrays
    generate_querying_probability_csv() # sets up on csv to read from
    two_D_Array = read_querying_csv() # Create 2D array with probabilities corresponding to all possible query mixes,
    key_gen([0]*s, THRESHOLD, 0, THRESHOLD+1)
    assignment_hashmap = create_assignment_hashmap() # Creates assignment hashmap, with assignment rule as shortest queue length
    
    
    #Create our system
    servers = create_servers() # dictionary
    servers_as_list = sum(list(servers.values()),[]) # Complex code to handle the fact that servers is a dictionary
    dispatchers = create_dispatchers(servers) # list
    servers_PQ = create_servers_priority_queue(servers_as_list) # Priority queue 
    
    # Set up system details
    jobs_added = 0
    clock = 0
    next_arrival_time = generate_new_interarrival(ARRIVAL_RATE)
    total_time = 0 # captures sum of response time of all jobs
    
    
    while jobs_added < TOTAL_JOBS:    
        clock = next_arrival_time # Jump ahead to adding our next job
        add_idle_servers_to_idle_lists(servers_PQ, dispatchers, clock) # Make sure idlelist is up to date, if any servers had became idle in between job arrivals
        
        chosenDispatcher = dispatchers[random.randint(0,NUM_DISPATCHER - 1)] # select a random dispatcher
        fastest_existing_idle_speed = None # Could be None (so dispatcher knows zero idle servers), or 0 through s-1
        for i in range(s):
            if len(chosenDispatcher.bigIdleList[i]) != 0:
                fastest_existing_idle_speed = i
                break    
        
        pickedServer, totalAdditionToTime = pick_server(chosenDispatcher, fastest_existing_idle_speed, 
                                                       clock, two_D_Array, servers, assignment_hashmap) # Pick server to give job to
        total_time += totalAdditionToTime
        
        newJob = Job(clock, pickedServer.speed)
        pickedServer.queued_jobs.append(newJob)
        if len(pickedServer.queued_jobs) == 1: # Begin working on the job if it's the only one
            pickedServer.next_departure_time = newJob.time_for_completion + clock

        update_idleness_according_to_new_job(pickedServer, dispatchers, servers_PQ)
        
        jobs_added += 1
        next_arrival_time += generate_new_interarrival(ARRIVAL_RATE)
    
    # We need to finish up any remaining jobs in the servers
    clock = sys.maxsize - 100 # Flashforward time
    total_time += finish_jobs_in_servers(servers_as_list, clock)
    
    print(total_time/TOTAL_JOBS)
    
if __name__ == '__main__':
    main()
    
