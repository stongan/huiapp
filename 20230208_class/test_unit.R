
head(lavaan::PoliticalDemocracy)
## The industrialization and Political Democracy Example
## Bollen (1989), page 332
model <- '
# latent variable definitions
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + a*y2
dem61 =~ b*y3 + c*y4
dem62 =~ y1 + d*y3
dem63 =~ e*y2 + f*y4
dem65 =~ y5 + a*y6 + b*y7 + c*y8
# regressions
dem60 ~ ind60
dem61 ~ dem60
dem62 ~ ind60
dem63 ~ dem61 + dem62
dem65 ~ ind60 + dem63
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8 '
fit <- lavaan::sem(model, data = lavaan::PoliticalDemocracy)


model <- '
# latent variable definitions
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + a*y2 + b*y3 + c*y4
dem65 =~ y5 + a*y6 + b*y7 + c*y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8'
fit <- lavaan::sem(model, data = PoliticalDemocracy)

model <- '
# latent variable definitions
# ind60 =~ x1 + x2 + x3
dem60 =~ y1 + a*y2 + b*y3 + c*y4
dem65 =~ y5 + a*y6 + b*y7 + c*y8
# regressions
dem60 ~ x1
dem65 ~ x1 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8'
fit <- lavaan::sem(model, data = PoliticalDemocracy)

source("./DataInfo.R")
dfm <- GetDataInfo(fit, lavaan::PoliticalDemocracy)

w <- dfm$weighInfo
r <- dfm$simulateExoVarQuote
i <- dfm$simulateExoVar

w <- dfm$weighInfo
r <- dfm$mid_cal_matrixQuoteList[['dem60']]
i <- dfm$simulateExoVar
m <- dfm$mid_cal_matrix
m[1, 'dem61'] - w['dem60','dem61'] * w['ind60','dem60'] * i[1, 'ind60']
r[1, 'dem61']
m[1, 'dem63'] - w['dem61','dem63'] * w['dem60','dem61'] * w['ind60','dem60'] * i[1, 'ind60']
r[1, 'dem63']

mid_travel <- dfm$mid_graph_info$traveBfsList
model2 <- paste0(model, dfm$toAddWavyStr)
fit2 <- lavaan::sem(model2, data = PoliticalDemocracy)
dfm2 <- GetDataInfo(fit2, PoliticalDemocracy)

b <- lavaan::parameterEstimates(fit, rsquare = TRUE)
c <- dfm$simulateExoVar
j <- 0.3*c[,"dem60"] + 0.2*c[, "dem61"]
c[1, "dem60"]