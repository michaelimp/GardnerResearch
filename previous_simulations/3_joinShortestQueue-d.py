import random
import numpy as np
import sys


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
        servers.append(Server(.5 * numServers))
    
    d = 2
    
    clock = 0
    nextArrivalTime = servers[0].generateNewInterArrival()
    totalTime = 0 # captures sum of response time of all jobs
    
    jobsAdded = 0
    jobsToAdd = 1000000
    
    #Calculate using Corollary 2 on pg 6 the theoretical mean response time for lambda = .5:
    total = 0
    for i in range(1,100):
        total += pow(.5,(pow(d,i)-d)/(d-1))
    print("Theoretical mean response time accoridng to corollary 2: ", total)
    
    
    while jobsAdded < jobsToAdd:    
        clock = nextArrivalTime # Jump ahead to adding our next job
        
        queriedServers = random.sample(servers,d)
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


    
    
    
    
    
    
    