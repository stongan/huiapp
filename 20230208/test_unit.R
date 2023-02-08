
library(lavaan)
head(PoliticalDemocracy)
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
fit <- sem(model, data = PoliticalDemocracy)


fttable <- parTable(fit)
eqs_table <- fttable[which(fttable$op=='~'),c('lhs', 'rhs')]
atable <- GetMatriOfLink(eqs_table)
columns(eqs_table)
for (a in 1:length(eqs_table)) {
  print (eqs_table['lhs'])
  break
}

lhs       <- eqs_table['lhs'][[1]]
rhs       <- eqs_table['rhs'][[1]]
union_var <- union(lhs, rhs)
n_var     <- length(union_var)
dfm <- matrix(0, n_var, n_var, dimnames = list(union_var, union_var))

source("./DataInfo.R")
dfm <- GetDataInfo(fit)
a <- dfm$var_graph_info$travelList


dfm$mid_graph_info
x <- lavaanNames(fit,type="eqs.x")
y <- lavaanNames(fit,type="eqs.y")


