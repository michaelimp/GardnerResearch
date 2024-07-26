s=2 # s: Number of server classes
d=2 # d: Number of queried servers
λ=.7 # λ: the scaled arrival rate
q=[.5,.5] # q: ratio of each type of server class
μ=[2.0,1.0] # μ: service rates of server classes ordered fastest to slowest
policy=:GEN_CLD #policy: policy name
# dSet is the possible querying options.  For example here it would be: [(2,0), (1,1), (0,2)] : optional
trunc_q=20 #trunc_q: max queue size
trunc_α=[10,9] # truncation policy
max_Prob_hmax=10^(-6) # truncation for reporting?
Query_Result_Prob_Thresh=10^(-6) # threshold for reporting?

GEN_CLD_seeded_soln=heter_disp_Opt_CLD_new(λ, μ, q, d;
                        #dSet=dSet,
                        trunc_q=trunc_q,
                        policy=policy, maxiter=3000, trunc_α=trunc_α,
                        max_Prob_hmax=max_Prob_hmax,
                        #initial_p=initial_p, initial_α=initial_α, initial_SolveTime=initial_SolveTime, initial_cumDist=initial_cumDist, cap=cap,
                        Query_Result_Prob_Thresh=Query_Result_Prob_Thresh)

GEN_CLD_seeded_soln[4]

# Value 1: objective value
# Value 2: nothing
# Value 3: optimal query
# Value 4: optimal assignment





#                       return(objective_value(model), nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
#                               Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
