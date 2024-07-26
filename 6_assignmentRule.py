# Based off of QueryingRuleUpdated.py
#Order: 1. MM1-Simulator 2.selectingRandomServer 3. JoinShortestQueue-d.py 4. DifferentSpeedServers.py 5. QueryingRuleUpdated 6. AssignmentRule.py

# Index 0 is the fastest server class, the last index is the slowest.

import random
import operator as op
from functools import reduce
import numpy as np
import sys
import math
from parameters import *

# Global
listOfKeys = [] # to aid in creation of assignment hashmap
# Parameters:
NUM_SERVERS = NUM_SERVERS
ARRIVAL_RATE = ARRIVAL_RATE
d = d
s = s
jobsAdded = 0
TOTAL_JOBS = TOTAL_JOBS
THRESHOLD = THRESHOLD


class Server:
    def __init__ (self, speed):
        self.queuedJobs = []
        self.nextDepartureTime = sys.maxsize # Let each server hold onto its own next departure time (when its first job finishes). But the next arrival time is still universal. 
        self.speed = speed # Possible speed values: .5, 1, 1.5, 2

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
        for j in range(NUM_SERVERS//s):
            listOfServers.append(Server(x))
        servers[i] = listOfServers
        x -=.5
    return servers
    
#________________________________________________________________________________
# Create assignment hashmap

def keyGen(qLengths, threshold, servClass, currThreshold):
    '''keyGen updates listOfKeys to contain all the possible keys. A key is vector of length s, where index 0 corresponds to fastest server class.
    Each index corresponds to the shortest queue length for each server class.
    Index 0 goes from 0 to threshold, and sys.max (which would occur when the fastest server class is never queried).
    Index 1 must be less than index 0, or it is sys.max
    An example key where threshold = 5, s = 4, would be: [5, sys.max, 1, 0]. Notice it is strictly decreasing
    An index could equal the global threshold, but always must be smaller than the local threshold (the local threshold as determined by the shortest length of servers faster than it)
    Return: nothing, just updates listOfKeys 
    '''
    if servClass == s: #base case
        #This step is necessary to handle issues with memory referencing: 
        deepCopy = []
        for i in qLengths:
            deepCopy.append(i)
        #__________________
        listOfKeys.append(deepCopy)
        return
    
    for i in range(threshold + 2):
        if i > currThreshold: 
            return
        if i == currThreshold:
            qLengths[servClass] = sys.maxsize
            localThreshold = currThreshold
        elif (i == threshold + 1):
            qLengths[servClass] = sys.maxsize
            localThreshold = currThreshold
        else:
            qLengths[servClass] = i
            localThreshold = qLengths[servClass]
        keyGen(qLengths,threshold,servClass+1, localThreshold)


def createAssignmentHashmap():
    ''' Currently implements a shortest queue length assignment rule
    Return: dictionary with keys as described above, and the value a probability distribution (vector of length s, sums to 1, at each index is 
    probability that that server class is assigned the job)
    rtype: dictionary
    '''
    qLengths = [0]*s
    keyGen(qLengths, THRESHOLD, 0, THRESHOLD+1) # creates listOfKeys
    assignmentHashmap = {}
    for key in listOfKeys:
        value = [0]*s # Probability distribution, vector [0,0...0] of length s
        value[key.index(min(key))] = 1 # Make the shortest queue length server class have probability 1 of being picked
        hashableKey = tuple(key) # list is non hashable, must convert to tuple
        assignmentHashmap[hashableKey] = value
    return assignmentHashmap

def createAssignmentKey(selectedQueryMix, queriedServers):
    ''' Creates assignment key that we'll hash into assignmentHashmap
    ptype: list of queriedServers
    return: assignment key
    rtype: list of len(s)'''
    assignmentKey = [0]*s 
    # Determine assignment key
    tracker = 0
    for i in range(s): # Puts shortest queue length of each server class into assignment key
        numThisClass = selectedQueryMix[i]
        if numThisClass == 0: # If that server class was not queried
            assignmentKey[i] = sys.maxsize
        else: # Finds the shortest queue length of the class
            queriedServersOfSameClass = queriedServers[int(tracker):int(tracker + numThisClass)]
            minLength = sys.maxsize
            for server in queriedServersOfSameClass:
                if len(server.queuedJobs) <= minLength:
                    minLength = len(server.queuedJobs)
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
               
def pickServer(probabilityDistribution, selectedQueryMix, queriedServers):
    '''Pick the server to assign the job to
    param probabilityDistribution: list of len(s), index corresponds to probability that server class is assigned
    return: server (shortest queue length server of the picked server class)
    '''
    probability = random.random()
    total = 0
    for i in range(len(probabilityDistribution)): # Index will be the server class picked
        total += probabilityDistribution[i]
        if total >= probability:
            tracker = 0
            for j in range(len(selectedQueryMix)):
                numThisClass = selectedQueryMix[j]
                if i == j: # When server class index matches up
                    queriedServersOfPickedClass = queriedServers[int(tracker):int(tracker + numThisClass)]
                    # Pick shortest queue length of that server class
                    minLength = sys.maxsize
                    for server in queriedServersOfPickedClass:
                        if len(server.queuedJobs) <= minLength:
                            minLength = len(server.queuedJobs)
                    for server in queriedServersOfPickedClass:
                        if len(server.queuedJobs) == minLength:
                            return server
                tracker += numThisClass
    
    
#________________________________________________________________________________
def finishJobsInServers(servers, clock):
    totalTimeAddition = 0
    for server in servers: 
        while True:
            if server.nextDepartureTime <= clock: # We need to first finish all the jobs that should have been completed by now in this server
                jobToFinish = server.queuedJobs.pop(0)
                totalTimeAddition += (server.nextDepartureTime - jobToFinish.arrival_time)
                if len(server.queuedJobs) != 0: # We process the next job
                    server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
                else: # We don't need to keep processing
                    server.nextDepartureTime = sys.maxsize
            else:
                break
    return totalTimeAddition


def main():
    global NUM_SERVERS, ARRIVAL_RATE, d, s, jobsAdded, TOTAL_JOBS
    
    servers = createServers() # dictionary
    allServersAsList = sum(list(servers.values()),[]) # Complex code to handle the fact that servers is a dictionary
    clock = 0
    nextArrivalTime = generateNewInterArrival(ARRIVAL_RATE)
    totalTime = 0 # captures sum of response time of all jobs
    
    two_D_Array = create2DProbabilityArray() # Create 2D array, let each query mix have equal probability.
    
    qLengths = [0]*s
    keyGen(qLengths, THRESHOLD, 0, THRESHOLD+1)
    
    assignmentHashmap = createAssignmentHashmap() # Creates assignment hashmap, with assignment rule as shortest queue length
    
    while jobsAdded < TOTAL_JOBS:    
        clock = nextArrivalTime # Jump ahead to adding our next job
        
        selectedQueryMix = pickQueryMix(two_D_Array) # Numpy array of numbers, each index corresponding to number of servers per that class
        
        queriedServers = [] # List of servers that satisfy the selectedQueryMix
        for i in range(s):
            numThisClass = selectedQueryMix[i]
            queriedServers.extend(random.sample(servers[i], int(numThisClass)))
        
        
        totalTime += finishJobsInServers(queriedServers, clock)
        # for server in queriedServers: 
        #     while True:
        #         if server.nextDepartureTime <= clock: # We need to first finish all the jobs that should have been completed by now in this server
        #             jobToFinish = server.queuedJobs.pop(0)
        #             totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
        #             if len(server.queuedJobs) != 0: # We process the next job
        #                 server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
        #             else: # We don't need to keep processing
        #                 server.nextDepartureTime = sys.maxsize
        #         else:
        #             break   
        assignmentKey = createAssignmentKey(selectedQueryMix,queriedServers) # a list
        server = pickServer(assignmentHashmap[tuple(assignmentKey)],selectedQueryMix, queriedServers) # Ends up selecting shortest length queue
        
        newJob = Job(clock, server.speed)
        server.queuedJobs.append(newJob)
        if len(server.queuedJobs) == 1: # Begin working on the job if it's the only one
            server.nextDepartureTime = newJob.timeForCompletion + clock
            
        jobsAdded += 1
        nextArrivalTime += generateNewInterArrival(ARRIVAL_RATE)
    
    # We need to finish up any remaining jobs in the servers
    clock = sys.maxsize - 100 # Flashforward time
    totalTime += finishJobsInServers(allServersAsList, clock)
    
        
    print(totalTime/TOTAL_JOBS)
    

    
if __name__ == '__main__':
    main()