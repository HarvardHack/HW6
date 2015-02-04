Inference 3

babies = read.table("babies.txt", header=TRUE)

q1:

bwt.nonsmoke = babies$bwt[babies$smoke==0]

pop.var = var(bwt.nonsmoke)

vars = replicate(1000, var(sample(bwt.nonsmoke, 10)))

hist(vars, breaks=100)

abline(v=pop.var, col="blue")

mean(vars > pop.var * 1.5)

q2:

bwt.nonsmoke = babies$bwt[babies$smoke==0]

pop.var = var(bwt.nonsmoke)

vars = replicate(1000, var(sample(bwt.nonsmoke, 50)))

mean(vars > pop.var * 1.5)

q3:

set.seed(0)
N <- 50
smokers <- sample(babies$bwt[babies$smoke==1], N)
nonsmokers <- sample(babies$bwt[babies$smoke==0], N)
obs = median(smokers) - median(nonsmokers)

avgdiff = replicate(1000, {

all = sample(c(smokers,nonsmokers))

smokersstar = all[1:N]

nonsmokersstar = all[(N+1):(2*N)]

return(median(smokersstar) - median(nonsmokersstar))

})

mean(abs(avgdiff) > abs(obs))
