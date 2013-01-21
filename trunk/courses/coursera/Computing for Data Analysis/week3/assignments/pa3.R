par(mfrow = c(1, 1))

outcome[, 11] = as.numeric(outcome[, 11])

states = outcome$State
freq = table(states)

needed.states = names(freq[freq > 20])

outcome2 = outcome[states %in% needed.states, ]

death = outcome2[, 11]
state = outcome2$State

# ordering by median
cc = complete.cases(death, state)
death = death[cc]
state = factor(state[cc])

medians = tapply(death, state, median)
order.by.medians = order(medians)
ordered.states = levels(state)[order.by.medians]

state.by.medians = ordered(state, ordered.states)

boxplot(death ~ state.by.medians, 
    ylab="30-day Death Rate", 
    main="Heart Attack 30-day Death Rate by State", 
    pars=list(las=2) # rotates x-axis and y-axis tick label
)