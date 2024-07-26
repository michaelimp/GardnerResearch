# Debugging queryRuleUpdated.py to verify dispatching: print out every query mix, what is the query mix that I picked. 
# Make sure that the cdf picking the query mix code works, make sure empirical distribution matches the probability. 
# Make sure after we get query mix, that we actually queried that mix

# Verified that my query mixes 2D array is correctly constructed
# Verified that empirical distribution matches the probability
# Verified that we have indeed queried the selected query mix
 
# 2-D array where column 1 is the query mix, and column 2 is probability of that query mix. The query mix is represented by a 1-D array (list or tuple)
# The issue is I can't have different data types for 2-day array. Instead, I could have each row be the query mix, with the last element as the probability
# Index 0 is the quickest server class, the last index is the slowest.

import random
import operator as op
from functools import reduce
import numpy as np
import sys
import math

# Parameters:
numServers = 1
arrival_rate = .0001 * numServers
d = 1 # Number of queried servers
s = 1 # Number of classes of server speeds
jobsAdded = 0
jobsToAdd = 1000000


class Server:
    def __init__ (self, speed):
        self.queuedJobs = []
        self.nextDepartureTime = sys.maxsize # Let each server hold onto its own next departure time (when its first job finishes). But the next arrival time is still universal. 
        self.speed = speed # Possible speed values: .5, 1, 1.5, 2
  
# Arrival rate and generateNewInterArrival is now a universal constant instead of part of the Server class.

def generateNewInterArrival(arrival_rate):
        return np.random.exponential(1 / arrival_rate)
        
def generateNewJobSize(server_speed):
    return np.random.exponential(1/server_speed)

class Job:
    def __init__ (self, arrival_time, server_speed):
        self.arrival_time = arrival_time
        self.timeForCompletion = generateNewJobSize(server_speed)
#______________________________________________________________________________________________


def ncr(n, r): # pass in n = d + s - 1, r = s -1
    ''' The total number of possible query mixes is (d + s - 1 choose s - 1) '''
    r = min(r, n-r)
    numer = reduce(op.mul, range(n, n-r, -1), 1)
    denom = reduce(op.mul, range(1, r+1), 1)
    return numer // denom


def constrained_partitions(n, k, min_elem, max_elem):
    ''' https://stackoverflow.com/questions/58915599/generate-restricted-weak-integer-compositions-or-partitions-of-an-integer-n-in'''
    allowed = range(max_elem, min_elem-1, -1)

    def helper(n, k, t):
        if k == 0:
            if n == 0:
                yield t
        elif k == 1:
            if n in allowed:
                yield t + (n,)
        elif min_elem * k <= n <= max_elem * k:
            for v in allowed:
                yield from helper(n - v, k - 1, t + (v,))

    return helper(n, k, ())

def create2DProbabilityArray():
    '''Returns a 2D Numpy array where each row is the query mix with the last column as the corresponding probability of it occurring. The query mix goes from fastest to slowest per column index
    :rtype: 2D numpy array
    '''
    A = np.zeros(s)
    for p in constrained_partitions(d, s, 0, d):
        A = np.vstack([A, p])
    A = np.delete(A,0,0)
    numQueryMixes = ncr(d+s-1,s-1)
    B = np.ones((numQueryMixes,1)) # Change so that each query mix could have different probabilities
    
    i = 0
    for p in constrained_partitions(d, s, 0, d):
        denominator = 1
        for index in p:
            denominator *= math.factorial(index)
        probability = math.pow(1/s,d) * math.factorial(d) / denominator
        B[i] = probability
        i += 1
    # numQueryMixes = ncr(d+s-1,s-1)
    # B = np.ones((numQueryMixes,1)) # Change so that each query mix could have different probabilities
    # for i in range(numQueryMixes):
    #     B[i] = 1/(numQueryMixes)
    A = np.hstack([A, B])
    return A

# 
def pickQueryMix(A):
    ''' Randomly picks query mix
    :param: 2D numpy array
    :return: 1D numpy row
    :rtype: numpy row
    '''
    probability = random.random()
    total = 0
    for i in range(len(A)):
        total += A[i][s] # A[i][s] is the probability
        if total >= probability:
            return A[i][:-1] # Return just the query mix, exclude the probability

def createServers():
    '''Returns a dictionary where each key is index ranging from 0 to s-1, with index 0 being fastest
    :return: Dictionary with key indicating server class and value being list of servers
    :rtype: Dictionary (key = int, value = list of servers)
    '''
    servers = {}
    if s == 1:
        x = 1
    else:
        x = 2
    for i in range(s):
        listOfServers = []
        for j in range(numServers//s):
            listOfServers.append(Server(x))
        servers[i] = listOfServers
        x -=.5
    return servers
    
#________________________________________________________________________________

def main():
    global numServers, arrival_rate, d, s, jobsAdded, jobsToAdd
    
    servers = createServers() # dictionary
    allServersAsList = sum(list(servers.values()),[]) # Complex code to handle the fact that servers is a dictionary
    clock = 0
    nextArrivalTime = generateNewInterArrival(arrival_rate)
    totalTime = 0 # captures sum of response time of all jobs
    
    two_D_Array = create2DProbabilityArray(d,s) # Create 2D array, let each query mix have equal probability.
    
    empiricalDistribution = dict() # For verifying equal probability of query mixes being selected
    for row in np.delete(two_D_Array,-1,1):
        hashableQueryMix = tuple(row.reshape(1, -1)[0])
        empiricalDistribution[hashableQueryMix] = 0
    
    while jobsAdded < jobsToAdd:    
        clock = nextArrivalTime # Jump ahead to adding our next job
        
        selectedQueryMix = pickQueryMix(two_D_Array,s) # List of numbers, each index corresponding to number of servers per that class
        
        # For verifying equal probability of query mixes being selected 
        hashableQueryMix = tuple(selectedQueryMix.reshape(1, -1)[0])
        empiricalDistribution[hashableQueryMix] = empiricalDistribution[hashableQueryMix] + 1
        #________________________________
        
        
        queriedServers = [] # List of servers that satisfy the selectedQueryMix
        for i in range(s):
            numThisClass = selectedQueryMix[i]
            queriedServers.extend(random.sample(servers[i], int(numThisClass)))
        
        # For verifying that we actually queried the selected query mix
        # print(selectedQueryMix)
        # for server in queriedServers:
        #     print(server.speed)
        #________________________________
        
        for server in queriedServers: # Finish up whatever jobs that should have been finished so the length of each queried server is accurate
            while True:
                if server.nextDepartureTime <= clock: # We need to first finish all the jobs that should have been completed by now in this server
                    jobToFinish = server.queuedJobs.pop(0)
                    totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
                    if len(server.queuedJobs) != 0: # We process the next job
                        server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
                    else: # We don't need to keep processing
                        server.nextDepartureTime = sys.maxsize
                else:
                    break
        
        # Select server with shortest length
        shortestServerLength = sys.maxsize
        server = queriedServers[0]
        for i in queriedServers:
            if len(i.queuedJobs) <= shortestServerLength:
                shortestServerLength = len(i.queuedJobs)
                server = i
        
        while True:
            if server.nextDepartureTime <= clock: # We need to first finish all the jobs that should have been completed by now in this server
                jobToFinish = server.queuedJobs.pop(0)
                totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
                if len(server.queuedJobs) != 0: # We process the next job
                    server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
                else: # We don't need to keep processing
                    server.nextDepartureTime = sys.maxsize
            else:
                break
        
        newJob = Job(clock, server.speed)
        server.queuedJobs.append(newJob)
        if len(server.queuedJobs) == 1: # Begin working on the job if it's the only one
            server.nextDepartureTime = newJob.timeForCompletion + clock
            
        jobsAdded += 1
        nextArrivalTime += generateNewInterArrival(arrival_rate)
    
    # We need to finish up any remaining jobs in the servers
    serverNumber = 0
    for server in allServersAsList:
        while True:
            if server.nextDepartureTime < sys.maxsize - 100: # We need to first finish all the jobs that should have been completed by now in this server
                jobToFinish = server.queuedJobs.pop(0)
                totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
                if len(server.queuedJobs) != 0: # We process the next job
                    server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
                else: # We don't need to keep processing
                    server.nextDepartureTime = sys.maxsize
            else:
                break
        serverNumber+=1
        
    print(totalTime/jobsToAdd)
    
    # For verifying equal probability of query mixes being selected
    # for key in empiricalDistribution.keys():
    #     empiricalDistribution[key] = empiricalDistribution[key] / jobsToAdd
    # print(empiricalDistribution.values())
    # #________________________________

    
if __name__ == '__main__':
    main()
    
 
# Testing results: 
# 1. MM1 Queue: 
#   arrival rate: .5
#   Result: 1.99 -
# 
# Parameters:
# numServers = 1, arrival_rate = .5 * numServers, d = 1, s = 1, jobsAdded = 0, jobsToAdd = 100000
# Result = 1.99
# 
# 2. selecting Random server
#   arrival rate: .8 * numSevers
#   numServers = 1000
#   Result = 4.932
# 
# Parameters:
# numServers = 1000, arrival_rate = .8 * numServers, d = 1, s = 1, jobsAdded = 0, jobsToAdd = 100000
# Result = 4.96
# 
# 3. JoinShortestQueue
#   Arrival rate = .5 * numservers
#   numServers = 1000
#   d = 2
#   Result = 1.266
#
# Parameters:
# numServers = 1000, arrival_rate = .5 * numServers, d = 2, s = 1, jobsAdded = 0, jobsToAdd = 100000
# Result = 1.263
# 
# 4. DifferentSpeedServers
#   Arrival rate = .5 * numservers
#   numServers = 1000
#   d = 2
#   numClassofServers = 4
#   Result = 1.0418
# 
# Parameters:
# numServers = 1000, arrival_rate = .5 * numServers, d = 2, s = 4, jobsAdded = 0, jobsToAdd = 100000
# Result = 1.0426
# 
# 
# 5. Very small arrival rate so hopefully mean response time is mean service time
# Parameters:
# numServers = 1, arrival_rate = .0001 * numServers, d = 1, s = 1, jobsAdded = 0, jobsToAdd = 100000
# Result = .99981
# I believe the job size I'm passing in is 1 so not sure why I'm getting this result
    
    