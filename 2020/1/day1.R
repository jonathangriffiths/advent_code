#!/bin/R
vec = read.table("day1_input.txt")[,1]
#Mission: find the two numbers that sum to 2020
# Then multiply them together
#start naively
p1_approach1 = function(x){
    #Heuristic steps
    #remove >= 2020 (though there are none)
    x = vec[vec<2020]
    #no point in adding numbers together that are both >=1010
    little = vec[vec < 1010]
    big = vec[vec > 1009]
    #for this puzzle, this has hugely reduced the search space
    #but there must be more possible...
    #anyway, let's just work through little, now
    mat = sapply(little, function(y) big + y)
    ind = which(mat == 2020)
    return(big[row(mat)[ind]] * little[col(mat)[ind]])
}

#Now we have to find _three_ numbers that sum to 2020
# Given the distribution of the numbers (most are close to 2020, we can still split off 
#    the big ones to save a lot of time)
p2_approach1 = function(x){
    x = vec[vec < 2020]
    #no point in adding together three numbers all larger than 2020/2
    little = vec[vec < 2020/2]
    big = vec[vec >= 2020/2]
    comb = combn(little, 2)
    add2 = colSums(comb)
}

#need to represent as some kind of tree of numbers previously added
#or as some huge tensor, alternatively, but that can include redundant rows?
#that's ugly, though

general_heuristic = function(x, n_add = 3){
    x = vec[vec < 2020]
    #no point in adding together numbers all larger than 2020/2
    little = vec[vec < 2020/2]
    big = vec[vec >= 2020/2]

}


# hold the phone
# this looks a bit recursive - we have taken the add-3 problem down into an add-2 problem
# that might be able to scale indefinitely, retaining our heuristic efficiencies


# There is, of course, a more brutal approach
brute_approach = function(x, n_add = 3){
    comb = combn(x, n_add)
    cs = colSums(comb)
    return(prod(comb[, cs == 2020]))
}

ptm = proc.time()
p1_approach1(vec)
proc.time() - ptm #0.007s
ptm = proc.time()
brute_approach(vec, 2)
proc.time() - ptm #0.02s



ptm = proc.time()
brute_approach(vec, 3)
proc.time() - ptm #1.3s
