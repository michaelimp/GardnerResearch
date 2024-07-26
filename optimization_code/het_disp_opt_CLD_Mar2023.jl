using JuMP, Ipopt, DataStructures

# λ : scaled arrival rate
# μ : vector of server speeds fast to slow in order
# q : fraction of servers in each class - Order: fastest to slowest server
# d : number of servers queried
# dSet : vector of query space thats being considered, when s=2, d=2,
#       Ex. [(1,1), (2,0)].  Need not be the whole spave.
#       only what is considered in the optimization problem
# trunc_q : truncation over the queue length in the optimization problem
#       any jobs arriving at a server with (trunc_q-1) number of jobs are
#       lost. i.e probability of having >= trunc_q jobs in any server is 0
# cap : if value exist, we are constraining the response time to less than the value
# policy : policy for optimization
# maxiter : maximum number iterations for optimization
# trunc_α : truncation for the α space
# cap_α : if trunc_α is not set we use the cap_α value across all classes
# space_α : the alpha space
# alldata : if we are returning additional information like: server class utilizations,
#               Probability of dispatch to specific server: Server_disp_prob, and
#               Probability of using a particular α decision: query_result_prob
# initial_l : start value for querying probabilities in IID format
# initial_p : start value for querying probabilities in GEN format
# initial_α : start value for α probabilities
# pGiven : query probabilities given can be for GEN or IID policies
# initial_cumDist: initial cumulative distribution

#function to get the querying probabilities as a dictionary
function get_GEN_p(prob, dSet, policy; queryDict=nothing, queryWays=nothing)
    if policy in [:GEN_CLD, :GEN_SED, :GEN_SEW, :GEN_UF, :GEN_JSQ, :GEN_BR]
        gen_p=Dict( i => prob[i] for i in dSet)
    elseif policy in [:IID_CLD, :IID_SED, :IID_SEW, :IID_UF, :IID_JSQ, :IID_BR]
        d=sum(dSet[1])
        s=length(dSet[1])
        gen_p=Dict( curr_query => factorial(big(d))*prod(((prob[j]^curr_query[j])/
                        factorial(big(curr_query[j])))
                        for j in [1:s;] if curr_query[j]>0) for curr_query in dSet)
    elseif policy in [:IND_CLD, :IND_SED, :IND_SEW, :IND_UF, :IND_JSQ, :IND_BR]
        d=sum(dSet[1])
        s=length(dSet[1])

        gen_p=Dict( queryd =>
            sum( prod(prob[(dorder,queries[dorder])] for dorder in [1:d;])
                for queries in queryWays if queryDict[queries]==queryd)
                        for queryd in dSet)

    elseif policy == :SRC_JSQ
        d=sum(dSet[1])
        s=length(dSet[1])

        # gen_p=Dict( i =>
        # ifelse(maximum(i)==d, prob[findfirst(i.==d)], 0.00)
        # for i in dSet)
        gen_p=Dict{NTuple{s,Int64}, Float64}()
        for i in dSet
            if maximum(i)==d
                push!(gen_p, i => prob[findfirst(i.==d)])
            else
                push!(gen_p, i => 0)
            end
        end
    end
    return gen_p
end


function good_soln(Prob_Dist, trunc_α,max_Prob_hmax)
    s=length(trunc_α)
    return all([Prob_Dist[(i,trunc_α[i])] for i in [1:s;]].<=max_Prob_hmax)
end

#function to get the dispatching probabilities as a dictionary
function get_α_Vals(α, space_α, policy)
    if policy in [:GEN_CLD, :IID_CLD, :IND_CLD, :GEN_SED, :GEN_SEW,:IID_CLD,
        :IID_SED, :IID_SEW, :IND_CLD, :IND_SED, :IND_SEW]
        α_Vals=Dict( i => α[i] for i in space_α)
    end
    return α_Vals
end

function get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)
    #first find the relevant hi values of interest
    s=length(trunc_α)

    QueMax2Study=map(y->
                    if Prob_Dist[(y,trunc_α[y])]<=Query_Result_Prob_Thresh
                        findfirst(x->Prob_Dist[(y,x)]<=Query_Result_Prob_Thresh, [0:trunc_α[y];])-2
                    else
                        trunc_α[y]
                    end, [1:s;])

    #QueMax2Study=map(y->findfirst(x->Prob_Dist[(y,x)]<=Query_Result_Prob_Thresh, [0:trunc_α[y];])-2, server_i)
    #The relevant query results to document
    MaxRel_QueryResults=collect(Iterators.product(map(x->([0:QueMax2Study[x];]...,trunc_α[x]+1),server_i)...))[:]
    Rel_QueryResults=NTuple{s,Int64}[]
    for CombinI in MaxRel_QueryResults
        #println(CombinI)
        checkV=0
        for (index1, value1) in enumerate(CombinI[1:s])
            if checkV==0
                for index2 in [index1+1:s;]
                    # remove if lower class has smaller queue length and the dominated class does not have length trunc_α+1
                    # or lower class has smaller queue length and lower class not at trunc value and higher class less than or equal to truncation value
                    if (((value1<CombinI[index2]) & (CombinI[index2]!=(trunc_α[index2]+1)))
                         | ((value1==CombinI[index2]) & (value1!=(trunc_α[index1])) &
                         (CombinI[index2]<=trunc_α[index2])))
                          checkV=1
                    end
                 end
             end
        end
        # remove if all values greater than truncation
        if ((checkV==0) & (sum(CombinI[1:s].>trunc_α)==s))
            checkV=1
        # remove if first class queried dominates all and the value not equal to 0
        elseif ((checkV==0) & (sum(CombinI[1:s].>trunc_α)==s-1) & (CombinI[findfirst(CombinI[1:s].<=trunc_α)]!=0))
            checkV=1
        # remove if more than d classes in picture
        elseif ((checkV==0) & (sum(CombinI[1:s].<=trunc_α)>d))
            checkV=1
        end

        if checkV==0
            push!(Rel_QueryResults,CombinI)
        end
    end
    query_result_prob = Dict{NTuple{s,Int64}, Float64}()
    query_result_prob_relevant = Dict{NTuple{s,Int64}, Float64}()
    ProbDispatch2Class=zeros(s)
    #Queues of significant probability to include
    QueSize2Consider=map(y->
                            (if (Prob_Dist[(y, trunc_α[y])]<=max_Prob_hmax)
                                findfirst(x->Prob_Dist[(y,x)]<=max_Prob_hmax, [0:trunc_α[y];])-1
                            else
                                trunc_α[y]
                            end), server_i)

            # findfirst(x->Prob_Dist[(y,x)]<=max_Prob_hmax, [0:trunc_α[y];])-1, server_i)
    qCombin=collect(Iterators.product(map(x->([0:QueSize2Consider[x];]...,trunc_q),server_i)...))[:]
    for qResult in Rel_QueryResults
        #println("---case: query fedback Result after domination: ", qResult,"------------------------------------")
        pos_undominated=findall(x->qResult[x] <= trunc_α[x], [1:s;])
        pos_undominated_untrunc_not0=findall(x->(qResult[x] < trunc_α[x]) & (qResult[x] >0), [1:s;])
        ProbqResult = 0.00
        qCombin2=copy(qCombin)
        qCombin2=deleteat!(qCombin2, findall(x->any(qCombin2[x][pos_undominated].==trunc_q), [1:length(qCombin2);]))
        qCombin2=deleteat!(qCombin2, findall(x->any(qCombin2[x][pos_undominated_untrunc_not0].!=qResult[pos_undominated_untrunc_not0]), [1:length(qCombin2);]))
        for dQuery in dSet
            if all(dQuery[pos_undominated].>=1)
                if (pos_undominated[1]>1)
                    if any(dQuery[1:pos_undominated[1]-1].!=0)
                        continue
                    end
                end
                # println("---selected dQuery: query fedback Result after domination: ", dQuery)
                probQuery=gen_p[dQuery]
                classesNotQueried=findall(x->dQuery[x]==0,[1:s;])
                classesQueried=findall(x->dQuery[x]!=0,[1:s;])
                qRelCom=copy(qCombin2)
                if length(classesNotQueried)>0
                    qRelCom=deleteat!(qRelCom, findall(x->any(qRelCom[x][classesNotQueried].!=trunc_q), [1:length(qRelCom);]))
                end
                if length(classesQueried)>0
                    qRelCom=deleteat!(qRelCom, findall(x->any(qRelCom[x][classesQueried].==trunc_q), [1:length(qRelCom);]))
                end
                # delete  where any of positions undominated not queries

                relevant_qComb=[]
                for qitem in qRelCom
                    # println("---checked qitem: query fedback Result after domination: ", qitem)
                    checkQ=true
                    if (sum(qResult.<=trunc_α)==1)
                        firstpos=findfirst(x->x==0,qResult)
                        #println("--------")
                        #println(qitem)
                        #println(qResult)
                        #println(s)
                        if qitem[firstpos]==trunc_q
                            checkQ=false
                        elseif firstpos>1
                            if any(qitem[1:firstpos-1].<trunc_q)
                                checkQ=false
                            end
                        end
                        if ((checkQ) & (firstpos<s))
                            if any(qitem[(firstpos+1):s].<qitem[firstpos])
                                checkQ=false
                            end
                        end
                    else
                        for (ind,elementq) in enumerate(qitem)
                            if (qResult[ind]==(trunc_α[ind]+1))
                                if !(elementq==trunc_q)
                                    if ind!=1
                                        #print(qResult, ind)
                                        if !any(qitem[1:(ind-1)].<= elementq)
                                            checkQ=false
                                            break
                                        end
                                    else
                                        checkQ=false
                                        break
                                    end
                                end
                            elseif (qResult[ind]==trunc_α[ind])
                                if !((elementq>=trunc_α[ind]) & (elementq<trunc_q))
                                    checkQ=false
                                    break
                                elseif ind>1
                                    if any(qitem[1:(ind-1)].<=elementq)
                                        checkQ=false
                                        break
                                    end
                                end
                            else
                                if !(elementq==qResult[ind])
                                    checkQ=false
                                    break
                                end
                            end
                        end
                    end
                    if checkQ
                        # println("----")
                        # println("query fedback Result after domination: ", qResult)
                        # println("server class query: ", dQuery)
                        # println("queried result: ", qitem)
                        queryResult=prod((Prob_Dist[ind,i_qitem]^dQuery[ind]-Prob_Dist[ind,i_qitem+1]^dQuery[ind]) for (ind,i_qitem) in enumerate(qitem) if (i_qitem!=trunc_q))
                        ProbqResult = ProbqResult+probQuery *queryResult
                        for i in server_i
                            if i in pos_undominated
                                ProbDispatch2Class[i]=ProbDispatch2Class[i]+probQuery *queryResult*α_Vals[(i,qResult...)]
                            end
                        end
                    end
                end
            end
            #println("------------------for the dQuery end--------------")
        end
        #println("------------------for the queue truncation--------------")
        push!(query_result_prob, qResult => ProbqResult)
        if ProbqResult>=Query_Result_Prob_Thresh
            push!(query_result_prob_relevant, qResult => ProbqResult)
        end
    end
    Server_disp_prob=Dict(i => ProbDispatch2Class[i] for i in server_i)

    Req_QueryRes=sort(collect(query_result_prob_relevant), by=x->x[2], rev=true)
    #make table
    return (Server_disp_prob, query_result_prob, Req_QueryRes)
end

# function to get the cumulative prob distribution as a dictionary
function get_prob_dist(queue_cum)
    classSize, MaxQueue =size(queue_cum)
    MaxQueue = MaxQueue -1
    indices = collect(Iterators.product((1:classSize),(0:MaxQueue)))[:]
    Prob_Dist= Dict((i,j) => queue_cum[i,j] for (i,j) in indices)
    return Prob_Dist
end

#function to get space_α
function get_space_α(trunc_α, d)
    s=length(trunc_α)
    space_α=NTuple{s+1,Int64}[]
    # We start with all combinations and then remove the infeasible elements
    # the space consists of (i, M)
    # i : the class to send the job to
    # M : vector of size s after the domination and truncation mapping.
    AllCombinations=reverse.(Iterators.product(map(x->(0:x+1),reverse(trunc_α))...,(1:s)))[:]
    #deleteat locations slower servers has greater number of jobs

    for CombinI in AllCombinations
        #println(CombinI)
        checkV=0
        for (index1, value1) in enumerate(CombinI[2:s])
            if checkV==0
                for index2 in [index1+1:s;]
                    # remove elements were either:
                    # 1. the dominated elements are trunc_α +1 values
                    # 2. the lower class has the same number of jobs as the higher class,
                    #       and the trunc_α values have not been reached

                    if (((value1<CombinI[index2+1]) & (CombinI[index2+1]!=(trunc_α[index2]+1)))
                        | ((value1==CombinI[index2+1]) & (value1!=(trunc_α[index1])) &
                        (CombinI[index2+1]<=trunc_α[index2])))
                         checkV=1
                         break
                    end
                 end
             end
        end

        # remove all elements where all greater than trunc_α
        if ((checkV==0) & (sum(CombinI[2:s+1].>trunc_α)==s))
            checkV=1
        # if only one non dominated class exist it must be 0
        elseif ((checkV==0) & (sum(CombinI[2:s+1].>trunc_α)==s-1) & (CombinI[findfirst(CombinI[2:s+1].<=trunc_α)+1]!=0))
            checkV=1
        # remove if the class to send job is dominated/ not queries
        elseif ((checkV==0) & (CombinI[CombinI[1]+1]>trunc_α[CombinI[1]]))
            checkV=1
        # remove if the number of active classes in space is greater than d
        elseif ((checkV==0) & (sum(CombinI[2:s+1].<=trunc_α)>d))
            checkV=1
        end

        if checkV==0
            push!(space_α,CombinI)
        end
    end
    return space_α
end

# function to get ατSet
function get_ατSet(trunc_α, d)
    s=length(trunc_α)
    ατCombin=collect(Iterators.product(map(x->(0:x+1),trunc_α)...))[:]
    ατSet=NTuple{s,Int64}[]
    for CombinI in ατCombin
        #println(CombinI)
        checkV=0
        for (index1, value1) in enumerate(CombinI[1:s])
            if checkV==0
                for index2 in [index1+1:s;]
                    # remove if lower class has smaller queue length and the dominated class does not have length trunc_α+1
                    # or lower class has smaller queue length and lower class not at trunc value and higher class less than or equal to truncation value
                    if (((value1<CombinI[index2]) & (CombinI[index2]!=(trunc_α[index2]+1)))
                         | ((value1==CombinI[index2]) & (value1!=(trunc_α[index1])) &
                         (CombinI[index2]<=trunc_α[index2])))
                          checkV=1
                    end
                 end
             end
        end
        # remove if all values greater than truncation
        if ((checkV==0) & (sum(CombinI[1:s].>trunc_α)==s))
            checkV=1
        # remove if first class queried dominates all and the value not equal to 0
        elseif ((checkV==0) & (sum(CombinI[1:s].>trunc_α)==s-1) & (CombinI[findfirst(CombinI[1:s].<=trunc_α)]!=0))
            checkV=1
        # remove if more than d classes in picture
        elseif ((checkV==0) & (sum(CombinI[1:s].<=trunc_α)>d))
            checkV=1
        end

        if checkV==0
            push!(ατSet,CombinI)
        end
    end
    return ατSet
end

# calculating
# class utilization : ρ
# Probability of dispatch to specific server: Server_disp_prob,
# Probability of using a particular α decision: query_result_prob
function get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α,
    trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
    ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
    # qCombin : all possible query results before transformation
    qCombin=collect(Iterators.product(fill(0:trunc_q,s)...))[:]
    query_result_prob = Dict{NTuple{s,Int64}, Float64}()
    ProbDispatch2Class=zeros(s)
    for qResult in ατSet
        #println("---case: query fedback Result after domination: ", qResult,"------------------------------------")
        pos_undominated=findall(x->qResult[x] <= trunc_α[x], [1:s;])
        ProbqResult = 0.00
        for dQuery in dSet
            if all(dQuery[pos_undominated].>=1)
                if (pos_undominated[1]>1)
                    if any(dQuery[1:pos_undominated[1]-1].!=0)
                        continue
                    end
                end
                # println("---selected dQuery: query fedback Result after domination: ", dQuery)
                probQuery=gen_p[dQuery]
                classesNotQueried=findall(x->dQuery[x]==0,[1:s;])
                classesQueried=findall(x->dQuery[x]!=0,[1:s;])
                qRelCom=copy(qCombin)
                if length(classesNotQueried)>0
                    qRelCom=deleteat!(qRelCom, findall(x->any(qRelCom[x][classesNotQueried].!=trunc_q), [1:length(qRelCom);]))
                end
                if length(classesQueried)>0
                    qRelCom=deleteat!(qRelCom, findall(x->any(qRelCom[x][classesQueried].==trunc_q), [1:length(qRelCom);]))
                end
                relevant_qComb=[]
                for qitem in qRelCom
                    # println("---checked qitem: query fedback Result after domination: ", qitem)
                    checkQ=true
                    if (sum(qResult.<=trunc_α)==1)
                        firstpos=findfirst(x->x==0,qResult)
                        #println("--------")
                        #println(qitem)
                        #println(qResult)
                        #println(s)
                        if qitem[firstpos]==trunc_q
                            checkQ=false
                        elseif firstpos>1
                            if any(qitem[1:firstpos-1].<trunc_q)
                                checkQ=false
                            end
                        end
                        if ((checkQ) & (firstpos<s))
                            if any(qitem[(firstpos+1):s].<qitem[firstpos])
                                checkQ=false
                            end
                        end
                    else
                        for (ind,elementq) in enumerate(qitem)
                            if (qResult[ind]==(trunc_α[ind]+1))
                                if !(elementq==trunc_q)
                                    if ind!=1
                                        #print(qResult, ind)
                                        if !any(qitem[1:(ind-1)].<= elementq)
                                            checkQ=false
                                            break
                                        end
                                    else
                                        checkQ=false
                                        break
                                    end
                                end
                            elseif (qResult[ind]==trunc_α[ind])
                                if !((elementq>=trunc_α[ind]) & (elementq<trunc_q))
                                    checkQ=false
                                    break
                                elseif ind>1
                                    if any(qitem[1:(ind-1)].<=elementq)
                                        checkQ=false
                                        break
                                    end
                                end
                            else
                                if !(elementq==qResult[ind])
                                    checkQ=false
                                    break
                                end
                            end
                        end
                    end
                    if checkQ
                        # println("----")
                        # println("query fedback Result after domination: ", qResult)
                        # println("server class query: ", dQuery)
                        # println("queried result: ", qitem)
                        queryResult=prod((Prob_Dist[ind,i_qitem]^dQuery[ind]-Prob_Dist[ind,i_qitem+1]^dQuery[ind]) for (ind,i_qitem) in enumerate(qitem) if (i_qitem!=trunc_q))
                        ProbqResult = ProbqResult+probQuery *queryResult
                        for i in server_i
                            if i in pos_undominated
                                ProbDispatch2Class[i]=ProbDispatch2Class[i]+probQuery *queryResult*α_Vals[(i,qResult...)]
                            end
                        end
                    end
                end
            end
            #println("------------------for the dQuery end--------------")
        end
        #println("------------------for the queue truncation--------------")
        push!(query_result_prob, qResult => ProbqResult)
    end
    Server_disp_prob=Dict(i => ProbDispatch2Class[i] for i in server_i)
    return (ρ, Server_disp_prob, query_result_prob)
end

#used in IND to get the relevant value in dSet
function qorder2query(i,s)
    jj=[]

    for a in 1:s
        push!(jj,sum(i.==a))
    end
    return(tuple(jj...))
end

function heter_disp_Opt_CLD_new(λ, μ, q, d; dSet=nothing, trunc_q=15,
    cap=nothing, policy=:GEN_CLD, maxiter=3000, trunc_α=nothing, cap_α=5,
    initial_l=nothing, initial_α=nothing,
    initial_p=nothing, pGiven=nothing, initial_cumDist=nothing,
    initial_SolveTime=nothing, max_Prob_hmax=10^-6,
    Query_Result_Prob_Thresh=10^-5,
    α_initialization=nothing, seedIND_CLD_type="IID")

    # s : number of classes is equal to size of μ vector
    s = length(μ)
    # server_i : creating class indices
    server_i=[1:s;]

    #BinomsDict[(i,j)] is the number of ways of choosing j from i
    BinomsDict=Dict((i,j)=> binomial(i,j) for i in [0:d;] for j in [0:i;])
    #factD[i] is the factorial of i
    factD=Dict(i => factorial(big(i)) for i in 0:d)

    # if dSet is missing
    if dSet==nothing
        dSet=reverse.(Iterators.product(fill(0:d,s)...))[:]
        dSet=deleteat!(dSet,findall(x->sum(dSet[x])!=d,[1:length(dSet);]))
    end

    #trunc_α is the truncation set for alpha space
    if isnothing(trunc_α)
        trunc_α=Int.(ones(s)*cap_α)
    end

    #space_α is the alpha space
    space_α=get_space_α(trunc_α, d)

    #newGamma is a function : given the class number i, the number of jobs in the least loaded server queried of class i - n, and the query vector d
    # it list all possible query results that can result in a dispatch to the i class server with n jobs
    function newGamma(dvec, i, n)
        if any(policy.==[:GEN_CLD, :GEN_SED, :IID_CLD, :IID_SED,
            :GEN_UF, :GEN_JSQ, :IID_UF, :IND_UF, :IID_JSQ, :GEN_SEW,:IID_SEW,
            :IND_CLD, :IND_SED, :IND_JSQ, :IND_SEW, :GEN_BR, :IID_BR, :IND_BR, :SRC_JSQ])
            QueryResultSet=collect(Iterators.product(map(x->ifelse(x==0, trunc_q, (n+1):(trunc_q-1)), dvec[1:(i-1)])...,n,
                            map(x->ifelse(x==0, trunc_q, 0:(trunc_q-1)), dvec[(i+1):s])...))[:]
            return QueryResultSet
        elseif any(policy.==[:GEN_CLD_F2, :GEN_SED_F2, :IID_CLD_F2, :IID_SED_F2,
            :GEN_CLD_UF_F2, :GEN_JSQ_F2, :IID_CLD_UF_F2, :IID_JSQ_F2, :GEN_SEW_F2,:IID_SEW_F2,
            :IND_CLD_F2, :IND_SED_F2, :IND_JSQ_F2, :IND_SEW_F2])
            # need to confirm - fix
            # QueryResultSet=collect(Iterators.product(map(x->ifelse(dvec[x]==0, hcap[x]+1, (n+1):hcap[x]), [1:(i-1);])...,n,
            #         map(x->ifelse(dvec[x]==0, hcap[x]+1, 0:hcap[x]), [(i+1):s;])...))[:]
            if trunc_α[i]>=n
                strt=n
            else
                strt=trunc_α[i]
            end
            # QueryResultSet=collect(Iterators.product(map(x->ifelse(x==0, trunc_q, (n+1):(trunc_q-1)), dvec[1:(i-1)])...,n,
            #                 map(x->ifelse(x==0, trunc_q, 0:(trunc_q-1)), dvec[(i+1):s])...))[:]
            # return QueryResultSet
        end
    end

    # Γ[(d,i,j)] dictionary of query results that can result in a possible dispatch to class i server with j jobs under query d
    if any(policy.==[:GEN_CLD, :GEN_SED, :IID_CLD, :IID_SED,
        :GEN_UF, :GEN_JSQ, :IID_UF, :IND_UF, :IID_JSQ, :GEN_SEW,:IID_SEW,
        :IND_CLD, :IND_SED, :IND_JSQ, :IND_SEW, :GEN_BR, :IID_BR, :IND_BR, :SRC_JSQ])
        Γ = Dict( (i,j,k) => newGamma(i,j,k) for i in dSet for j in [1:s;] if i[j]>=1 for k in [0:(trunc_q-1);])
    elseif any(policy.==[:GEN_CLD_F2, :GEN_SED_F2, :IID_CLD_F2, :IID_SED_F2,
        :GEN_CLD_UF_F2, :GEN_JSQ_F2, :IID_CLD_UF_F2, :IID_JSQ_F2, :GEN_SEW_F2,:IID_SEW_F2,
        :IND_CLD_F2, :IND_SED_F2, :IND_JSQ_F2, :IND_SEW_F2])
        # to complete
        #Γ = Dict( (i,j,k) => newGamma(i,j,k) for i in dSet for j in [1:s;] if i[j]>=1 for k in [0:hcap[j];])
    end

    # mapping to the corresponding alpha vector given the class i and the query result τ
    function OmegaTau(i,τ)
        if any(policy.==[:GEN_CLD, :GEN_SED, :IID_CLD, :IID_SED,
            :GEN_UF, :GEN_JSQ, :IID_UF, :IND_UF, :IID_JSQ, :GEN_SEW,:IID_SEW,
            :IND_CLD, :IND_SED, :IND_JSQ, :IND_SEW, :GEN_BR, :IID_BR, :IND_BR, :SRC_JSQ])

            # check first if dominated by the first queried server
            if (((i==1) & all(τ[i] .<= τ)) | ((i>1) & all(τ[1:(i-1)].==trunc_q) & all(τ[i] .<= τ[i:s])))
                return (i,map(x->trunc_α[x]+1,[1:(i-1);])...,0,map(x->trunc_α[x]+1,[(i+1):s;])...)
            else
                newτ=[]
                push!(newτ,i)
                for j in server_i
                    # if not queried
                    if τ[j]==trunc_q
                        push!(newτ,trunc_α[j]+1)
                    # else if queried but any previously queried had smaller queue
                    elseif ((j!=1) & any(τ[j] .>= τ[1:(j-1)]))
                        push!(newτ,trunc_α[j]+1)
                    # else if queried server has greater than equal to trunc_α
                    elseif (τ[j]>=trunc_α[j])
                        push!(newτ,trunc_α[j])
                    # else
                    else
                        push!(newτ,τ[j])
                    end
                end
                return  tuple(newτ...)
            end
        elseif any(policy.==[:GEN_CLD_F2, :GEN_SED_F2, :IID_CLD_F2, :IID_SED_F2,
            :GEN_CLD_UF_F2, :GEN_JSQ_F2, :IID_CLD_UF_F2, :IID_JSQ_F2, :GEN_SEW_F2,:IID_SEW_F2,
            :IND_CLD_F2, :IND_SED_F2, :IND_JSQ_F2, :IND_SEW_F2])
            # to check
            # if (((i==1) & all(τ[i] .<= map(x->ifelse(τ[x]>hcap[x],Inf,τ[x]),[1:s;]))) |
            #     ((i>1) & all(τ[1:(i-1)].==(hcap[1:(i-1)].+1)) & all(τ[i] .<= map(x->ifelse(τ[x]>hcap[x],Inf,τ[x]),[i:s;]))))
            #     return (i,map(x->hcap[x]+1,[1:(i-1);])...,0,map(x->hcap[x]+1,[(i+1):s;])...)
            # else
            #     newτ=[]
            #     push!(newτ,i)
            #     for j in server_i
            #         if (τ[j]==(hcap[j]+1))
            #             push!(newτ,hcap[j]+1)
            #         elseif ((j!=1) & any(τ[j] .>= τ[1:(j-1)]))
            #             push!(newτ,hcap[j]+1)
            #         elseif (τ[j]>=hcap[j])
            #             push!(newτ,hcap[j])
            #         else
            #             push!(newτ,τ[j])
            #         end
            #     end
            #     return  tuple(newτ...)
            # end
        end
    end

    #ατSet is the space of query result after truncation and mapping.
    # basically space of query result in α
    ατSet = get_ατSet(trunc_α, d)

    # set Ipopt optimizer
    model = Model(Ipopt.Optimizer)
    # set the number of iterations
    set_optimizer_attribute(model, "max_iter", maxiter)
    # probability of i clas server having greater than equal to j jobs : x[i,j]
    @variable(model, 0<=x[i in server_i, j in [0:trunc_q;]]<=1)

    # forcing some x[i,j] values
    for i in server_i
        fix(x[i, 0],1; force=true)
        fix(x[i, trunc_q],0; force=true)

        # to prevent many jobs being lost in the system
        @NLconstraint(model, x[i, trunc_q-1]<=10^(-6))
    end

    #@NLconstraint(model, sum(x[i, 1] *q[i] *μ[i] for i in server_i)>=λ-10^-6)

    # if seeded solution for cumulative prob distribution exists
    if !(isnothing(initial_cumDist))
        for i in server_i
            for j in [1:trunc_q-1;]
                if haskey(initial_cumDist, (i,j))
                    set_start_value(x[i, j], initial_cumDist[(i,j)])
                else
                    set_start_value(x[i, j], 0.00)
                end
            end
        end
    end

    # if there exist a constraint on ET
    if !(isnothing(cap))
        @NLconstraint(model,
        sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ<=cap*1.001)
    end

    # GEN_CLD policy
    if (policy==:GEN_CLD)
        # define variable - querying probabilities
        @variable(model, 0<=p[ i in dSet ]<=1)
        # give the starting points if provided
        if !(isnothing(pGiven))
            for i in dSet
                fix(p[i],pGiven[i]; force=true)
            end
        else
            # sum of queryring probabilities = 1
            @NLconstraint(model, sum(p[i] for i in dSet)==1)
        end
        # if seeded information available on querying
        if !(isnothing(initial_l))
            for i in dSet
                set_start_value(p[i], factD[d]*prod(initial_l[j]^i[j]/factD[i[j]] for j in [1:s;] if i[j]>0))
            end
        elseif !(isnothing(initial_p))
            for i in dSet
                set_start_value(p[i], initial_p[i])
            end
        end

        # define variables - α - assignment probabilities
        @variable(model, 0<=α[ i in space_α ]<=1)
        # assigning starting probabilities
        if α_initialization=="JSQ"
            α_init=Dict{NTuple{s+1,Int64},Float64}()
            for newτ in ατSet
                pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

                best_sed=pos_ser[findmin(map(x->(newτ[x]),pos_ser))[2]]
                for j in pos_ser
                    if j !=best_sed
                        push!(α_init, (j,newτ...) => 0.00)
                    else
                        push!(α_init, (j,newτ...) => 1.00)
                    end
                end
            end
            for i in space_α
                set_start_value(α[i],α_init[i])
            end
        elseif α_initialization=="SED"
            α_init=Dict{NTuple{s+1,Int64},Float64}()
            for newτ in ατSet
                pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
                #pos_ser2=findall(x->newτ[x] < trunc_α[x], [1:s;]) # actually <=, this is to help distribute load
                # if length(pos_ser2)==0
                #     pos_ser2=findall(x->newτ[x] <= trunc_α[x], [1:s;])
                # end
                best_sed=pos_ser[findmin(map(x->(newτ[x]+1)/μ[x],pos_ser))[2]]
                for j in pos_ser
                    if j !=best_sed
                        push!(α_init, (j,newτ...) => 0.00)
                    else
                        push!(α_init, (j,newτ...) => 1.00)
                    end
                end
            end
            for i in space_α
                set_start_value(α[i],α_init[i])
            end
        elseif α_initialization=="SEW"
            α_init=Dict{NTuple{s+1,Int64},Float64}()
            for newτ in ατSet
                pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
                #pos_ser2=findall(x->newτ[x] < trunc_α[x], [1:s;]) # actually <=, this is to help distribute load
                # if length(pos_ser2)==0
                #     pos_ser2=findall(x->newτ[x] <= trunc_α[x], [1:s;])
                # end
                best_sed=pos_ser[findmin(map(x->(newτ[x])/μ[x],pos_ser))[2]]
                for j in pos_ser
                    if j !=best_sed
                        push!(α_init, (j,newτ...) => 0.00)
                    else
                        push!(α_init, (j,newτ...) => 1.00)
                    end
                end
            end
            for i in space_α
                set_start_value(α[i],α_init[i])
            end
        elseif !(isnothing(initial_α))
            for i in space_α
                set_start_value(α[i],initial_α[i])
            end
        end

        # equating differential equations to 0
        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(p[queryd] * (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        # sum of dispatching probabilities = 0
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
            @NLconstraint(model, sum(α[(j,newτ...)] for j in pos_ser)==1)
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)
        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            gen_p=get_GEN_p(value.(p), dSet, policy)
            α_Vals=get_α_Vals(value.(α), space_α, policy)
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)
            return(objective_value(model), nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
            Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
        else
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, nothing, false, NaN, NaN)
        end

    elseif (policy==:IID_CLD)
        # serl: iid query probability of each servers
        @variable(model, 0<=serl[ i in server_i ]<=1)
        if !(isnothing(pGiven))
            for i in server_i
                fix(serl[i],pGiven[i]; force=true)
            end
        else
            if !(isnothing(initial_l))
                for i in server_i
                    set_start_value(serl[i],initial_l[i])
                end
            end
            # sum of queryring probabilities = 1
            @NLconstraint(model, sum(serl[i] for i in server_i)==1)
        end
        @variable(model, 0<=α[ i in space_α]<=1)
        if initial_α!=nothing
            for i in space_α
                set_start_value(α[i],initial_α[i])
            end
        end

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(factD[d]*prod(((serl[ll]^queryd[ll])/factD[queryd[ll]]) for ll in server_i if queryd[ll]>0) *
                (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
            @NLconstraint(model, sum(α[(j,newτ...)] for j in pos_ser)==1)
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            iid_p=Dict( i => value.(serl)[i] for i in server_i)
            gen_p=get_GEN_p(iid_p, dSet, policy)
            α_Vals=get_α_Vals(value.(α), space_α, policy)
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)

            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)

            return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
                    Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
            # if good_soln(Prob_Dist, trunc_α,max_Prob_hmax)
            #     iid_p=Dict( i => value.(serl)[i] for i in server_i)
            #     gen_p=get_GEN_p(iid_p, dSet, policy)
            #     α_Vals=get_α_Vals(value.(α), space_α, policy)
            #     ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            #     Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)
            #     return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, Req_QueryRes)
            # else
            #     iid_p=Dict( i => NaN for i in server_i)
            #     gen_p=Dict( i => NaN for i in dSet)
            #     α_Vals=Dict( i => NaN for i in space_α)
            #     indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            #     Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            #     ρ = Dict(i => NaN for i in server_i)
            #     Server_disp_prob=Dict(i => NaN for i in server_i)
            #     query_result_prob=Dict(i => NaN for i in ατSet)
            #     return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, nothing)
            # end
        else
            iid_p=Dict( i => NaN for i in server_i)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob,
                        query_result_prob, nothing, false, NaN, NaN)
        end

    elseif (policy==:IND_CLD)
        #l[d, i] : probability to query class i server on the dth query
        @variable(model, 0<=l[j in [1:d;], i in server_i ]<=1)
        if initial_l!=nothing
            if seedIND_CLD_type=="IND"
                for j in [1:d;]
                    for i in server_i
                        set_start_value(l[j,i], initial_l[j,i])
                    end
                end
            else
                for j in [1:d;]
                    for i in server_i
                        set_start_value(l[j,i], initial_l[i])
                    end
                end
            end
        end
        @variable(model, 0<=α[ i in space_α ]<=1)
        if initial_α!=nothing
            for i in space_α
                set_start_value(α[i],initial_α[i])
            end
        end

        @NLconstraint(model, posQueried[j = 1:d], sum(l[j,i] for i in server_i)==1)

        queryWays=collect(Iterators.product(fill(1:s,d)...))[:]
        queryDict = Dict( (i) => qorder2query(i,s) for i in queryWays)

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(
                sum( prod(l[dorder,queries[dorder]] for dorder in [1:d;]) for queries in queryWays if queryDict[queries]==queryd) *
                (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
            @NLconstraint(model, sum(α[(j,newτ...)] for j in pos_ser)==1)
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => value.(l)[i,j] for (i,j) in indices_ind)
            gen_p=get_GEN_p(ind_p, dSet, policy; queryDict=queryDict, queryWays=queryWays)
            α_Vals=get_α_Vals(value.(α), space_α, policy)
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)
            return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
            Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))

        else
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => NaN for (i,j) in indices_ind)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, nothing, false, NaN, NaN)
        end

    elseif (policy==:GEN_SED)
         @variable(model, 0<=p[ i in dSet ]<=1)
         if !(isnothing(pGiven))
             for i in dSet
                 fix(p[i],pGiven[i]; force=true)
             end
         else
             if !(isnothing(initial_p))
                 for i in dSet
                     set_start_value(p[i], initial_p[i])
                 end
             end
             @NLconstraint(model, sum(p[i] for i in dSet)==1)
         end

         α=Dict{NTuple{s+1,Int64},Float64}()
         for newτ in ατSet
             pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
             #pos_ser2=findall(x->newτ[x] < trunc_α[x], [1:s;]) # actually <=, this is to help distribute load
             # if length(pos_ser2)==0
             #     pos_ser2=findall(x->newτ[x] <= trunc_α[x], [1:s;])
             # end
             best_sed=pos_ser[findmin(map(x->(newτ[x]+1)/μ[x],pos_ser))[2]]
             for j in pos_ser
                 if j !=best_sed
                     push!(α, (j,newτ...) => 0.00)
                 else
                     push!(α, (j,newτ...) => 1.00)
                 end
             end
         end

         for i in server_i
             for j in [1:(trunc_q-1);]
                 @NLconstraint(model, λ/q[i]*
                 sum(p[queryd] * (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                 α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
             end
         end

         @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

         optimize!(model)
         if !isnothing(initial_SolveTime)
             SolveTime=solve_time(model)+initial_SolveTime
         else
             SolveTime=solve_time(model)
         end

         if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
             Prob_Dist=get_prob_dist(value.(x))
             gen_p=get_GEN_p(value.(p), dSet, policy)
             α_Vals=α
             ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
             Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)
             return(objective_value(model), nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
             Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
         else
             gen_p=Dict( i => NaN for i in dSet)
             α_Vals=α
             indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
             Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
             ρ = Dict(i => NaN for i in server_i)
             Server_disp_prob=Dict(i => NaN for i in server_i)
             query_result_prob=Dict(i => NaN for i in ατSet)
             return(NaN, nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, nothing, false, NaN, NaN)
         end

    elseif (policy==:GEN_SEW)
      @variable(model, 0<=p[ i in dSet ]<=1)
      if !(isnothing(pGiven))
          for i in dSet
              fix(p[i],pGiven[i]; force=true)
          end
      else
          if !(isnothing(initial_p))
              for i in dSet
                  set_start_value(p[i], initial_p[i])
              end
          end
          @NLconstraint(model, sum(p[i] for i in dSet)==1)
      end

      α=Dict{NTuple{s+1,Int64},Float64}()
      for newτ in ατSet
          pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
          #pos_ser2=findall(x->newτ[x] < trunc_α[x], [1:s;]) # actually <=, this is to help distribute load
          # if length(pos_ser2)==0
          #     pos_ser2=findall(x->newτ[x] <= trunc_α[x], [1:s;])
          # end
          best_sed=pos_ser[findmin(map(x->(newτ[x])/μ[x],pos_ser))[2]]
          for j in pos_ser
              if j !=best_sed
                  push!(α, (j,newτ...) => 0.00)
              else
                  push!(α, (j,newτ...) => 1.00)
              end
          end
      end

      for i in server_i
         for j in [1:(trunc_q-1);]
             @NLconstraint(model, λ/q[i]*
             sum(p[queryd] * (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
             α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
         end
      end

      @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

      optimize!(model)
      if !isnothing(initial_SolveTime)
          SolveTime=solve_time(model)+initial_SolveTime
      else
          SolveTime=solve_time(model)
      end

      if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
          Prob_Dist=get_prob_dist(value.(x))
          gen_p=get_GEN_p(value.(p), dSet, policy)
          α_Vals=α
          ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
          Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)
          return(objective_value(model), nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
          Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
      else
          gen_p=Dict( i => NaN for i in dSet)
          α_Vals=α
          indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
          Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
          ρ = Dict(i => NaN for i in server_i)
          Server_disp_prob=Dict(i => NaN for i in server_i)
          query_result_prob=Dict(i => NaN for i in ατSet)
          return(NaN, nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, nothing, false, NaN, NaN)
      end

    elseif (policy==:IID_SED)
            # serl: iid query probability of each servers
            @variable(model, 0<=serl[ i in server_i ]<=1)
            if !(isnothing(pGiven))
                for i in server_i
                    fix(serl[i],pGiven[i]; force=true)
                end
            else
                if !(isnothing(initial_l))
                    for i in server_i
                        set_start_value(serl[i],initial_l[i])
                    end
                end
                # sum of queryring probabilities = 1
                @NLconstraint(model, sum(serl[i] for i in server_i)==1)
            end

            α=Dict{NTuple{s+1,Int64},Float64}()
            for newτ in ατSet
                pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
                best_sed=pos_ser[findmin(map(x->(newτ[x]+1)/μ[x],pos_ser))[2]]
                for j in pos_ser
                    if j !=best_sed
                        push!(α, (j,newτ...) => 0.00)
                    else
                        push!(α, (j,newτ...) => 1.00)
                    end
                end
            end

            for i in server_i
                for j in [1:(trunc_q-1);]
                  @NLconstraint(model, λ/q[i]*
                  sum(factD[d]*prod(((serl[ll]^queryd[ll])/factD[queryd[ll]]) for ll in server_i if queryd[ll]>0) *
                  (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                  α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
                end
            end

            @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

            optimize!(model)

            if !isnothing(initial_SolveTime)
                SolveTime=solve_time(model)+initial_SolveTime
            else
                SolveTime=solve_time(model)
            end

            if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
                Prob_Dist=get_prob_dist(value.(x))
                iid_p=Dict( i => value.(serl)[i] for i in server_i)
                gen_p=get_GEN_p(iid_p, dSet, policy)
                α_Vals=α
                ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
                Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)

                return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
                        Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))

            else
                iid_p=Dict( i => NaN for i in server_i)
                gen_p=Dict( i => NaN for i in dSet)
                α_Vals=Dict( i => NaN for i in space_α)
                indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
                Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
                ρ = Dict(i => NaN for i in server_i)
                Server_disp_prob=Dict(i => NaN for i in server_i)
                query_result_prob=Dict(i => NaN for i in ατSet)
                return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob,
                query_result_prob, nothing, false, NaN, NaN)
            end

    elseif (policy==:IID_SEW)

        # serl: iid query probability of each servers
        @variable(model, 0<=serl[ i in server_i ]<=1)
        if !(isnothing(pGiven))
            for i in server_i
                fix(serl[i],pGiven[i]; force=true)
            end
        else
            if !(isnothing(initial_l))
                for i in server_i
                    set_start_value(serl[i],initial_l[i])
                end
            end
            # sum of queryring probabilities = 1
            @NLconstraint(model, sum(serl[i] for i in server_i)==1)
        end

        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
            best_sed=pos_ser[findmin(map(x->(newτ[x])/μ[x],pos_ser))[2]]
            for j in pos_ser
                if j !=best_sed
                    push!(α, (j,newτ...) => 0.00)
                else
                    push!(α, (j,newτ...) => 1.00)
                end
            end
        end

        for i in server_i
            for j in [1:(trunc_q-1);]
              @NLconstraint(model, λ/q[i]*
              sum(factD[d]*prod(((serl[ll]^queryd[ll])/factD[queryd[ll]]) for ll in server_i if queryd[ll]>0) *
              (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
              α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)
        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            iid_p=Dict( i => value.(serl)[i] for i in server_i)
            gen_p=get_GEN_p(iid_p, dSet, policy)
            α_Vals=α
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)

            return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
                    Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
        else
            iid_p=Dict( i => NaN for i in server_i)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob,
            query_result_prob, nothing, false, NaN, NaN)
        end

        # if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
        #     iid_p=Dict( i => value.(serl)[i] for i in server_i)
        #     gen_p=get_GEN_p(value.(serl), dSet, policy)
        #     α_Vals=α
        #     Prob_Dist=get_prob_dist(value.(x))
        #     if !alldata
        #         return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
        #     else
        #         ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
        #         return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
        #     end
        # else
        #     iid_p=Dict( i => NaN for i in server_i)
        #     gen_p=Dict( i => NaN for i in dSet)
        #     α_Vals=α
        #     indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
        #     Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
        #     if !alldata
        #         return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
        #     else
        #         ρ = Dict(i => NaN for i in server_i)
        #         Server_disp_prob=Dict(i => NaN for i in server_i)
        #         query_result_prob=Dict(i => NaN for i in ατSet)
        #         return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
        #     end
        # end

    elseif (policy==:IND_SED)
        #l[d, i] : probability to query class i server on the dth query
        @variable(model, 0<=l[j in [1:d;], i in server_i ]<=1)
        if initial_l!=nothing
            for j in [1:d;]
                for i in server_i
                    set_start_value(l[j,i], initial_l[i])
                end
            end
        end
        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
            best_sed=pos_ser[findmin(map(x->(newτ[x]+1)/μ[x],pos_ser))[2]]
            for j in pos_ser
                if j !=best_sed
                    push!(α, (j,newτ...) => 0.00)
                else
                    push!(α, (j,newτ...) => 1.00)
                end
            end
        end

        @NLconstraint(model, posQueried[j = 1:d], sum(l[j,i] for i in server_i)==1)

        queryWays=collect(Iterators.product(fill(1:s,d)...))[:]
        queryDict = Dict( (i) => qorder2query(i,s) for i in queryWays)

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(
                sum( prod(l[dorder,queries[dorder]] for dorder in [1:d;]) for queries in queryWays if queryDict[queries]==queryd) *
                (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => value.(l)[i,j] for (i,j) in indices_ind)
            gen_p=get_GEN_p(ind_p, dSet, policy; queryDict=queryDict, queryWays=queryWays)
            α_Vals=α
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)

            return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
                    Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))

        else
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => NaN for (i,j) in indices_ind)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob,
            query_result_prob, nothing, false, NaN, NaN)
        end



        # if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
        #     indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
        #     ind_p=Dict( (i,j) => value.(l)[i,j] for (i,j) in indices_ind)
        #     gen_p=get_GEN_p(ind_p, dSet, policy; queryDict=queryDict, queryWays=queryWays)
        #     α_Vals=α
        #     Prob_Dist=get_prob_dist(value.(x))
        #     if !alldata
        #         return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
        #     else
        #         ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
        #         return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
        #     end
        # else
        #     indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
        #     ind_p=Dict( (i,j) => NaN for (i,j) in indices_ind)
        #     gen_p=Dict( i => NaN for i in dSet)
        #     α_Vals=α
        #     indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
        #     Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
        #     if !alldata
        #         return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
        #     else
        #         ρ = Dict(i => NaN for i in server_i)
        #         Server_disp_prob=Dict(i => NaN for i in server_i)
        #         query_result_prob=Dict(i => NaN for i in ατSet)
        #         return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
        #     end
        # end

    elseif (policy==:IND_SEW)
        #l[d, i] : probability to query class i server on the dth query
        @variable(model, 0<=l[j in [1:d;], i in server_i ]<=1)
        if initial_l!=nothing
            for j in [1:d;]
                for i in server_i
                    set_start_value(l[j,i], initial_l[i])
                end
            end
        end
        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])
            best_sed=pos_ser[findmin(map(x->(newτ[x])/μ[x],pos_ser))[2]]
            for j in pos_ser
                if j !=best_sed
                    push!(α, (j,newτ...) => 0.00)
                else
                    push!(α, (j,newτ...) => 1.00)
                end
            end
        end

        @NLconstraint(model, posQueried[j = 1:d], sum(l[j,i] for i in server_i)==1)

        queryWays=collect(Iterators.product(fill(1:s,d)...))[:]
        queryDict = Dict( (i) => qorder2query(i,s) for i in queryWays)

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(
                sum( prod(l[dorder,queries[dorder]] for dorder in [1:d;]) for queries in queryWays if queryDict[queries]==queryd) *
                (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => value.(l)[i,j] for (i,j) in indices_ind)
            gen_p=get_GEN_p(ind_p, dSet, policy; queryDict=queryDict, queryWays=queryWays)
            α_Vals=α
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)

            return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
                    Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))

        else
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => NaN for (i,j) in indices_ind)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob,
            query_result_prob, nothing, false, NaN, NaN)
        end

    elseif (policy==:SRC_JSQ)
        # probability of querying all d servers from a certain class
        @variable(model, 0<=serl[ i in server_i ]<=1)

        @NLconstraint(model, sum(serl[i] for i in server_i)==1)

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*serl[i]*(x[i,j-1]^d-x[i,j]^d)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

            best_sed=pos_ser[findmin(map(x->(newτ[x]),pos_ser))[2]]
            for j in pos_ser
                if j !=best_sed
                    push!(α, (j,newτ...) => 0.00)
                else
                    push!(α, (j,newτ...) => 1.00)
                end
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end
        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            src_p=Dict( i => value.(serl)[i] for i in server_i)
            gen_p=get_GEN_p(value.(serl), dSet, policy)
            α_Vals=α
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)
            return(objective_value(model), src_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
                    Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
            # if good_soln(Prob_Dist, trunc_α,max_Prob_hmax)
            #
            #
            #
            #
            #
            #     query_result_prob=NaN
            #     Req_QueryRes = nothing
            #     return(objective_value(model), src_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, Req_QueryRes)
            # else
            #     src_p=Dict( i => NaN for i in server_i)
            #     gen_p=Dict( i => NaN for i in dSet)
            #     α_Vals=nothing
            #     indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            #     Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            #     ρ = Dict(i => NaN for i in server_i)
            #     Server_disp_prob = Dict(NaN => src_p[i] for i in server_i)
            #     query_result_prob=NaN
            #     Req_QueryRes = NaN
            #     return(NaN, src_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, nothing)
            # end
        else
            src_p=Dict( i => NaN for i in server_i)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob = Dict(NaN => src_p[i] for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, src_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, nothing,
                false, NaN, NaN)
        end


    elseif (policy==:GEN_UF)

         @variable(model, 0<=p[ i in dSet ]<=1)

         α=Dict{NTuple{s+1,Int64},Float64}()
         for newτ in ατSet
             pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

             for j in pos_ser
                 push!(α, (j,newτ...) => 1/length(pos_ser))
             end
         end

         @NLconstraint(model, sum(p[i] for i in dSet)==1)

         for i in server_i
             for j in [1:(trunc_q-1);]
                 @NLconstraint(model, λ/q[i]*
                 sum(p[queryd] * (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                 α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
             end
         end

         @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

         optimize!(model)

         if !isnothing(initial_SolveTime)
             SolveTime=solve_time(model)+initial_SolveTime
         else
             SolveTime=solve_time(model)
         end

         if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
             gen_p=get_GEN_p(value.(p), dSet, policy)
             α_Vals=α
             Prob_Dist=get_prob_dist(value.(x))
             if !alldata
                 return(objective_value(model), gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
             else
                 ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
                 return(objective_value(model), gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
             end
         else
             gen_p=Dict( i => NaN for i in dSet)
             α_Vals=α
             indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
             Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
             if !alldata
                 return(NaN, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
             else
                 ρ = Dict(i => NaN for i in server_i)
                 Server_disp_prob=Dict(i => NaN for i in server_i)
                 query_result_prob=Dict(i => NaN for i in ατSet)
                 return(NaN, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
             end
         end

    elseif (policy==:IID_UF)

       @variable(model, 0<=serl[ i in server_i ]<=1)
       if !(isnothing(pGiven))
           for i in server_i
             fix(serl[i],pGiven[i]; force=true)
           end
       else
           @NLconstraint(model, sum(serl[i] for i in server_i)==1)
       end

       α=Dict{NTuple{s+1,Int64},Float64}()
       for newτ in ατSet
           pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

           for j in pos_ser
               push!(α, (j,newτ...) => 1/length(pos_ser))
           end
       end

       @NLconstraint(model, sum(serl[i] for i in server_i)==1)

       for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(factD[d]*prod(((serl[ll]^queryd[ll])/factD[queryd[ll]]) for ll in server_i if queryd[ll]>0) *
                (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

       @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

       optimize!(model)

       if !isnothing(initial_SolveTime)
           SolveTime=solve_time(model)+initial_SolveTime
       else
           SolveTime=solve_time(model)
       end

       if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            iid_p=Dict( i => value.(serl)[i] for i in server_i)
            gen_p=get_GEN_p(value.(serl), dSet, policy)
            α_Vals=α
            Prob_Dist=get_prob_dist(value.(x))
            if !alldata
                return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
                return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        else
            iid_p=Dict( i => NaN for i in server_i)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=α
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            if !alldata
                return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ = Dict(i => NaN for i in server_i)
                Server_disp_prob=Dict(i => NaN for i in server_i)
                query_result_prob=Dict(i => NaN for i in ατSet)
                return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        end

    elseif (policy==:IND_UF)
        #l[d, i] : probability to query class i server on the dth query
        @variable(model, 0<=l[j in [1:d;], i in server_i ]<=1)
        if initial_l!=nothing
            for j in [1:d;]
                for i in server_i
                    set_start_value(l[j,i], initial_l[i])
                end
            end
        end
        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

            for j in pos_ser
                push!(α, (j,newτ...) => 1/length(pos_ser))
            end
        end

        @NLconstraint(model, posQueried[j = 1:d], sum(l[j,i] for i in server_i)==1)

        queryWays=collect(Iterators.product(fill(1:s,d)...))[:]
        queryDict = Dict( (i) => qorder2query(i,s) for i in queryWays)

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(
                sum( prod(l[dorder,queries[dorder]] for dorder in [1:d;]) for queries in queryWays if queryDict[queries]==queryd) *
                (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => value.(l)[i,j] for (i,j) in indices_ind)
            gen_p=get_GEN_p(ind_p, dSet, policy; queryDict=queryDict, queryWays=queryWays)
            α_Vals=α
            Prob_Dist=get_prob_dist(value.(x))
            if !alldata
                return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
                return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        else
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => NaN for (i,j) in indices_ind)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=α
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            if !alldata
                return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ = Dict(i => NaN for i in server_i)
                Server_disp_prob=Dict(i => NaN for i in server_i)
                query_result_prob=Dict(i => NaN for i in ατSet)
                return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        end

    elseif (policy==:GEN_JSQ)
        @variable(model, 0<=p[ i in dSet ]<=1)
        if !(isnothing(pGiven))
            for i in dSet
                fix(p[i],pGiven[i]; force=true)
            end
        else
            if !(isnothing(initial_p))
                for i in dSet
                    set_start_value(p[i], initial_p[i])
                end
            end
            @NLconstraint(model, sum(p[i] for i in dSet)==1)
        end

        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

            best_sed=pos_ser[findmin(map(x->(newτ[x]),pos_ser))[2]]
            for j in pos_ser
                if j !=best_sed
                    push!(α, (j,newτ...) => 0.00)
                else
                    push!(α, (j,newτ...) => 1.00)
                end
            end
        end

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(p[queryd] * (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end
        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            gen_p=get_GEN_p(value.(p), dSet, policy)
            α_Vals=α
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)
            return(objective_value(model), nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
            Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
        else
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=α
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, nothing, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob, nothing, false, NaN, NaN)
        end

    elseif (policy==:IID_JSQ)

        # serl: iid query probability of each servers
        @variable(model, 0<=serl[ i in server_i ]<=1)
        if !(isnothing(pGiven))
            for i in server_i
                fix(serl[i],pGiven[i]; force=true)
            end
        else
            if !(isnothing(initial_l))
                for i in server_i
                    set_start_value(serl[i],initial_l[i])
                end
            end
            # sum of queryring probabilities = 1
            @NLconstraint(model, sum(serl[i] for i in server_i)==1)
        end

        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

            best_sed=pos_ser[findmin(map(x->(newτ[x]),pos_ser))[2]]
            for j in pos_ser
                if j !=best_sed
                    push!(α, (j,newτ...) => 0.00)
                else
                    push!(α, (j,newτ...) => 1.00)
                end
            end
        end

        for i in server_i
            for j in [1:(trunc_q-1);]
              @NLconstraint(model, λ/q[i]*
              sum(factD[d]*prod(((serl[ll]^queryd[ll])/factD[queryd[ll]]) for ll in server_i if queryd[ll]>0) *
              (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
              α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end


        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            iid_p=Dict( i => value.(serl)[i] for i in server_i)
            gen_p=get_GEN_p(iid_p, dSet, policy)
            α_Vals=α
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)

            return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
                    Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))
        else
            iid_p=Dict( i => NaN for i in server_i)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob,
            query_result_prob, nothing, false, NaN, NaN)
        end



        # if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
        #     iid_p=Dict( i => value.(serl)[i] for i in server_i)
        #     gen_p=get_GEN_p(value.(serl), dSet, policy)
        #     α_Vals=α
        #     Prob_Dist=get_prob_dist(value.(x))
        #     if !alldata
        #         return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
        #     else
        #         ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
        #         return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
        #     end
        # else
        #     iid_p=Dict( i => NaN for i in server_i)
        #     gen_p=Dict( i => NaN for i in dSet)
        #     α_Vals=α
        #     indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
        #     Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
        #     if !alldata
        #         return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
        #     else
        #         ρ = Dict(i => NaN for i in server_i)
        #         Server_disp_prob=Dict(i => NaN for i in server_i)
        #         query_result_prob=Dict(i => NaN for i in ατSet)
        #         return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
        #     end
        # end

    elseif (policy==:IND_JSQ)
        #l[d, i] : probability to query class i server on the dth query
        @variable(model, 0<=l[j in [1:d;], i in server_i ]<=1)
        if initial_l!=nothing
            for j in [1:d;]
                for i in server_i
                    set_start_value(l[j,i], initial_l[i])
                end
            end
        end
        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

            best_sed=pos_ser[findmin(map(x->(newτ[x]),pos_ser))[2]]
            for j in pos_ser
                if j !=best_sed
                    push!(α, (j,newτ...) => 0.00)
                else
                    push!(α, (j,newτ...) => 1.00)
                end
            end
        end

        @NLconstraint(model, posQueried[j = 1:d], sum(l[j,i] for i in server_i)==1)

        queryWays=collect(Iterators.product(fill(1:s,d)...))[:]
        queryDict = Dict( (i) => qorder2query(i,s) for i in queryWays)

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(
                sum( prod(l[dorder,queries[dorder]] for dorder in [1:d;]) for queries in queryWays if queryDict[queries]==queryd) *
                (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            Prob_Dist=get_prob_dist(value.(x))
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => value.(l)[i,j] for (i,j) in indices_ind)
            gen_p=get_GEN_p(ind_p, dSet, policy; queryDict=queryDict, queryWays=queryWays)
            α_Vals=α
            ρ = Dict(i => Prob_Dist[(1,i)] for i in server_i)
            Server_disp_prob, query_result_prob, Req_QueryRes = get_Good_α_Vals(α_Vals, ατSet, d, Prob_Dist, gen_p, Query_Result_Prob_Thresh, trunc_α, server_i, dSet, trunc_q, max_Prob_hmax)

            return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob,
                    Req_QueryRes, good_soln(Prob_Dist, trunc_α,max_Prob_hmax), maximum([Prob_Dist[(i,trunc_α[i])] for i in server_i]), maximum([Prob_Dist[(i,trunc_q-1)] for i in server_i]))

        else
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => NaN for (i,j) in indices_ind)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=Dict( i => NaN for i in space_α)
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            ρ = Dict(i => NaN for i in server_i)
            Server_disp_prob=Dict(i => NaN for i in server_i)
            query_result_prob=Dict(i => NaN for i in ατSet)
            return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob,
            query_result_prob, nothing, false, NaN, NaN)
        end

    elseif (policy==:GEN_BR)
        @variable(model, 0<=p[ i in dSet ]<=1)
        if !(isnothing(pGiven))
            for i in dSet
                fix(p[i],pGiven[i]; force=true)
            end
        else
            @NLconstraint(model, sum(p[i] for i in dSet)==1)
        end

        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

            best_sed=pos_ser[findmin(map(x->(newτ[x]),pos_ser))[2]]
            for j in pos_ser
                push!(α, (j,newτ...) => μ[j]*q[j]/sum(μ[jj]*q[jj] for jj in pos_ser))
            end
        end

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(p[queryd] * (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            gen_p=get_GEN_p(value.(p), dSet, policy)
            α_Vals=α
            Prob_Dist=get_prob_dist(value.(x))
            if !alldata
                return(objective_value(model), gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
                return(objective_value(model), gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        else
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=α
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            if !alldata
                return(NaN, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ = Dict(i => NaN for i in server_i)
                Server_disp_prob=Dict(i => NaN for i in server_i)
                query_result_prob=Dict(i => NaN for i in ατSet)
                return(NaN, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        end

    elseif (policy==:IID_BR)

        # serl: iid query probability of each servers
        @variable(model, 0<=serl[ i in server_i ]<=1)
        if !(isnothing(pGiven))
            for i in server_i
              fix(serl[i],pGiven[i]; force=true)
            end
        else
            @NLconstraint(model, sum(serl[i] for i in server_i)==1)
        end

        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

            best_sed=pos_ser[findmin(map(x->(newτ[x]),pos_ser))[2]]
            for j in pos_ser
                push!(α, (j,newτ...) => μ[j]*q[j]/sum(μ[jj]*q[jj] for jj in pos_ser))
            end
        end

        for i in server_i
            for j in [1:(trunc_q-1);]
              @NLconstraint(model, λ/q[i]*
              sum(factD[d]*prod(((serl[ll]^queryd[ll])/factD[queryd[ll]]) for ll in server_i if queryd[ll]>0) *
              (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
              α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            iid_p=Dict( i => value.(serl)[i] for i in server_i)
            gen_p=get_GEN_p(value.(serl), dSet, policy)
            α_Vals=α
            Prob_Dist=get_prob_dist(value.(x))
            if !alldata
                return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
                return(objective_value(model), iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        else
            iid_p=Dict( i => NaN for i in server_i)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=α
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            if !alldata
                return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ = Dict(i => NaN for i in server_i)
                Server_disp_prob=Dict(i => NaN for i in server_i)
                query_result_prob=Dict(i => NaN for i in ατSet)
                return(NaN, iid_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        end

    elseif (policy==:IND_BR)
        #l[d, i] : probability to query class i server on the dth query
        @variable(model, 0<=l[j in [1:d;], i in server_i ]<=1)
        if initial_l!=nothing
            for j in [1:d;]
                for i in server_i
                    set_start_value(l[j,i], initial_l[i])
                end
            end
        end
        α=Dict{NTuple{s+1,Int64},Float64}()
        for newτ in ατSet
            pos_ser=findall(x->newτ[x] <= trunc_α[x], [1:s;])

            best_sed=pos_ser[findmin(map(x->(newτ[x]),pos_ser))[2]]
            for j in pos_ser
                push!(α, (j,newτ...) => μ[j]*q[j]/sum(μ[jj]*q[jj] for jj in pos_ser))
            end
        end

        @NLconstraint(model, posQueried[j = 1:d], sum(l[j,i] for i in server_i)==1)

        queryWays=collect(Iterators.product(fill(1:s,d)...))[:]
        queryDict = Dict( (i) => qorder2query(i,s) for i in queryWays)

        for i in server_i
            for j in [1:(trunc_q-1);]
                @NLconstraint(model, λ/q[i]*
                sum(
                sum( prod(l[dorder,queries[dorder]] for dorder in [1:d;]) for queries in queryWays if queryDict[queries]==queryd) *
                (sum(  (prod((x[l,τ[l]]^queryd[l]-x[l,τ[l]+1]^queryd[l]) for l in server_i if queryd[l]>0)*
                α[OmegaTau(i, τ)]) for τ in Γ[(queryd,i,j-1)])) for queryd in dSet if queryd[i]>0)==μ[i]*(x[i,j]-x[i,j+1]))
            end
        end

        @NLobjective(model,Min, sum(q[i]*sum(x[i,j] for j in [1:(trunc_q-1);]) for i in server_i)/λ)

        optimize!(model)

        if !isnothing(initial_SolveTime)
            SolveTime=solve_time(model)+initial_SolveTime
        else
            SolveTime=solve_time(model)
        end

        if ((termination_status(model) == MOI.LOCALLY_SOLVED)||(termination_status(model) == MOI.OPTIMAL)|| (termination_status(model) == MOI.OPTIMAL) || (termination_status(model) == MOI.ALMOST_OPTIMAL) || (termination_status(model) == MOI.ALMOST_LOCALLY_SOLVED))
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => value.(l)[i,j] for (i,j) in indices_ind)
            gen_p=get_GEN_p(ind_p, dSet, policy; queryDict=queryDict, queryWays=queryWays)
            α_Vals=α
            Prob_Dist=get_prob_dist(value.(x))
            if !alldata
                return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ, Server_disp_prob, query_result_prob = get_ρ_ServerDispProb_queryResultProb(dSet, gen_p, α_Vals, trunc_α, trunc_q, ατSet, space_α, Prob_Dist, q, policy, server_i, s)
                return(objective_value(model), ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        else
            indices_ind = collect(Iterators.product((1:d),(1:s)))[:]
            ind_p=Dict( (i,j) => NaN for (i,j) in indices_ind)
            gen_p=Dict( i => NaN for i in dSet)
            α_Vals=α
            indices = collect(Iterators.product((1:s),(0:trunc_q)))[:]
            Prob_Dist= Dict((i,j) => NaN for (i,j) in indices)
            if !alldata
                return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model))
            else
                ρ = Dict(i => NaN for i in server_i)
                Server_disp_prob=Dict(i => NaN for i in server_i)
                query_result_prob=Dict(i => NaN for i in ατSet)
                return(NaN, ind_p, gen_p, α_Vals, Prob_Dist, SolveTime, termination_status(model), ρ, Server_disp_prob, query_result_prob)
            end
        end

    end
end
