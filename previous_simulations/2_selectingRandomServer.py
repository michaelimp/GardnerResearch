import random
import numpy as np
import sys

#E[Response time] 1 = 1/ (𝜇 - lamda / k servers) 

#if the job arrives, the clock jumps to arrival time, I need to create next arrival time. If the queue is empty, I need to also create a departure time
#if a job departs, I need to create a new departure time for the next job to be finished 

#Two distinct approaches:
    # 1. Every time we loop back up in the while loop, we go to the next time that a job arrives, and selects a server. Then we go into this server and react accordingly.
    # It might be the case that we had jobs in this server which should have been completed earlier, so we need to retroactively handle that. Every server has its own nextDepartureTime.
    # 2. Alternatively, we take it one small step at a time. Each server has its own nextArrivalTime and nextDepartureTime. We look across all of this and find the smallest.
    # We react accordingly. The issue with this is that we would have to always loop through each arrivalTime and departureTime, which is slow, unless we can have a list of al lthe next actions and sort it.
 

class Server:
    def __init__ (self, arrival_rate):
        self.arrival_rate = arrival_rate
        self.queuedJobs = []
        self.nextDepartureTime = sys.maxsize # Let each server hold onto its own next departure time (when its first job finishes). But the next arrival time is still universal.  
    
    def generateNewInterArrival(self):
        return np.random.exponential(1 / self.arrival_rate)
  
    
def generateNewJobSize():
    return np.random.exponential(1)

class Job:
    def __init__ (self, arrival_time):
        self.arrival_time = arrival_time
        self.timeForCompletion = generateNewJobSize()
    

def main():
    servers = []
    numServers = 1000
    for i in range(numServers):
        servers.append(Server(.5*numServers))
    
    clock = 0
    nextArrivalTime = servers[0].generateNewInterArrival()
    totalTime = 0 # captures sum of response time of all jobs
    
    jobsAdded = 0
    jobsToAdd = 1000000
    
    while jobsAdded < jobsToAdd:    
        clock = nextArrivalTime # Jump ahead to adding our next job
        server = servers[random.randint(0,numServers-1)] # Randomly select a server to add this job to
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
        
        newJob = Job(clock)
        server.queuedJobs.append(newJob)
        if len(server.queuedJobs) == 1: # Begin working on the job if it's the only one
            server.nextDepartureTime = newJob.timeForCompletion + clock
            
        jobsAdded += 1
        nextArrivalTime += server.generateNewInterArrival()
    
    # We need to finish up any remaining jobs in the servers
    serverNumber = 0
    for server in servers:
        
        while True:
            if server.nextDepartureTime < sys.maxsize - 100: # We need to first finish all the jobs that should have been completed by now in this server
                jobToFinish = server.queuedJobs.pop(0)
                # print("ServerNumber", serverNumber)
                # print(server.nextDepartureTime)
                totalTime += (server.nextDepartureTime - jobToFinish.arrival_time)
                if len(server.queuedJobs) != 0: # We process the next job
                    server.nextDepartureTime += server.queuedJobs[0].timeForCompletion
                else: # We don't need to keep processing
                    server.nextDepartureTime = sys.maxsize
            else:
                break
        serverNumber+=1
        
    print(totalTime/jobsToAdd)
    

    
if __name__ == '__main__':
    main()


    
    
    
    
    
    
    