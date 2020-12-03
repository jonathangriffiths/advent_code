#!/bin/R
input = read.table("day3_input.txt", strings=FALSE, comment.char = "")[,1]
matrix = do.call(rbind, lapply(input, function(x){
    vec = sapply(seq_len(nchar(x)), function(y){
        substr(x, y, y) == "#"
    })
}))
#TRUE are trees
#How many trees do we hit?

#the elegant solution would be some OOP so we can store our location on the grid
#we will be more functional here - hooray for recursion!
move = function(x_history=1, y_history=1, move_x=3, move_y=1, pattern = matrix){
    #1 indexing messes up the simpler modulo positioning
    newpos = x_history[length(x_history)] + move_x
    if(newpos > ncol(pattern)) newpos = newpos %% ncol(pattern)
    new_x = c(
        x_history,
        newpos
    )
    new_y = c(
        y_history,
        y_history[length(y_history)] + move_y
    )
    rows_keep = seq_len(length(new_y))
    if(new_y[length(new_y)] > nrow(pattern)){
        warning("Caution: Y overshoot - dropping bottom row")
        rows_keep = rows_keep[seq_len(length(rows_keep)-1)]
    }
    if(new_y[length(new_y)] >= nrow(pattern)){
        return(data.frame(
            x = new_x,
            y = new_y
        )[rows_keep,])
    } else {
        move(new_x, new_y, move_x, move_y, pattern)
    }
}

get_hits = function(xmove, ymove){
    tab = move(1,1,xmove,ymove,pattern=matrix)
    tab$value = sapply(seq_len(nrow(tab)), function(i) matrix[tab$y[i], tab$x[i]])
    sum(tab$value)
}
get_hits(3,1)

q2 = data.frame(
    x = c(1,3,5,7,1),
    y = c(rep(1,4), 2)
)
q2$hits = sapply(seq_len(nrow(q2)), function(i){
    get_hits(q2$x[i],q2$y[i])
})
prod(q2$hits)

#test the overshoot protection
# get_hits(2, 3)
