
###https://rdrr.io/r/methods/refClass.html
###http://adv-r.had.co.nz/R5.html
###https://bookdown.org/xiao/RAnalysisBook/section-2.html#section-2.1.3
HGraphNode <- setRefClass("HGraphNode",
  fields = list(
    id       = "numeric", 
    name     = "character",
    parent   = "vector",
    children = "vector",
    isStart  = "logical",
    isEnd    = "logical"
  ),
  methods = list(
    initialize = function(){
      id       <<- 0
      name     <<- ""
      parent   <<- vector(mode = "character", length = 0)
      children <<- vector(mode = "character", length = 0)
      isStart  <<- FALSE
      isEnd    <<- FALSE
    }
  )
)

HGraphClass <- setRefClass("HGraphClass",
  fields=list(edgeMat      = "matrix", ## main member
              varList      = "vector",
              nVar         = "numeric",
              name2id      = "list",
              graphInfo    = "list",
              travelList   = "list",
              reachable    = "matrix",
              traveBfsList = "vector",
              visited      = "vector", ## help member
              colleListBFS = "vector",
              colleList    = "vector")
)
HGraphClass$methods(
  init = function(edgeMat_t, var_list_t) {
    edgeMat <<- edgeMat_t
    varList <<- var_list_t
    nVar    <<- length(var_list_t)
    visited <<- c(rep(FALSE,nVar))
    name2id <<- setNames(as.list(rep(1:nVar)), varList)
    for (i in 1:nVar) {
      graphInfo[[i]] <<- HGraphNode()
    }
    reachable <<- matrix(0, nVar, nVar, dimnames = list(varList, varList))
  }
)

HGraphClass$methods(
CreateGraph = function() {
  for (i in 1:nVar) {
    graphInfo[[i]]$id   <<- i
    graphInfo[[i]]$name <<- varList[i]
    for (j in 1:nVar) {
      if (edgeMat[varList[i], varList[j]] == 1) {
        graphInfo[[i]]$children <<- c(graphInfo[[i]]$children, varList[j])
        graphInfo[[j]]$parent   <<- c(graphInfo[[j]]$parent, varList[i])
      }
    }
  }
  for (i in 1:nVar) {
    cur_node <- graphInfo[[i]]
    if (length(cur_node$parent) < 1) {
      graphInfo[[i]]$isStart <<- TRUE
    }
    if (length(cur_node$children) < 1) {
      graphInfo[[i]]$isEnd <<- TRUE
    }
  } #end for
}) #end function

HGraphClass$methods(
UpdateReachable = function() {
  t_len <- length(colleList)
  if (t_len >= 2) {
    for (i in 1:(t_len-1)) {
      for (j in (i+1):t_len) {
        reachable[colleList[i], colleList[j]] <<- 1
        #print(paste0(i,"_",j,"-",colleList[i],"_",colleList[j]))
      }
    }
  }
  #print ("--------------------")
})

HGraphClass$methods(
DFS = function(idx) {
  #print (graphInfo[[idx]]$name)
  colleList <<- c(colleList, graphInfo[[idx]]$name)
  if (graphInfo[[idx]]$isEnd) {
    n_travel <- length(travelList)
    travelList[[n_travel+1]] <<- colleList
    UpdateReachable()
    #print (colleList)
    #print ("--------------------")
    return (1)
  }
  children <- graphInfo[[idx]]$children
  if (length(children) > 0) {
    for(i in 1:length(children)){
      children_name <- children[i]
      children_id   <- name2id[[children_name]]
      DFS(children_id)
      n_coll <- length(colleList)
      if (n_coll > 0) {
        colleList <<- colleList[1:n_coll-1]
      }
    } # end for
  } # end if
}) # end func

HGraphClass$methods(
PrintTravelList = function() {
  print (paste(rep('-',50), collapse=""))
  print ("PrintTravelList")
  for(i in 1:length(travelList)){
    print (paste(travelList[[i]], collapse="->"))
  }
  print (paste(rep('-',50), collapse=""))
})

## GET all reachable path
HGraphClass$methods(
TravelGraph = function() {
  #init
  travelList <<- list()
  reachable  <<- matrix(0, nVar, nVar, dimnames = list(varList, varList))
  #print ("begin TravelGraph")
  for(i in 1:nVar) {
    colleList <<- vector(mode = "character", length = 0)
    if (graphInfo[[i]]$isStart) {
      DFS(i)
    }
  }
  #PrintTravelList()
  #print ("end TravelGraph")
})

HGraphClass$methods(
PrintTravelListBFS = function() {
  print (paste(rep('-',50), collapse=""))
  print ("PrintTravelListBFS")
  print (paste(traveBfsList, collapse="->"))
  print (paste(rep('-',50), collapse=""))
})

HGraphClass$methods(
IsAllParentVisited = function(t_name, t_parent) {
  if (length(t_parent) > 0) {
    for (i in 1:length(t_parent)) {
      parent_name <- t_parent[i]
      parent_id   <- name2id[[parent_name]]
      if (!visited[parent_id]) {
        return (FALSE)
      }
    }
  }
  return (TRUE)
})

HGraphClass$methods(
TravelGraphBFS = function() {
    #init
    visited      <<- rep(FALSE, nVar)
    colleListBFS <<- vector(mode = "character", length = 0)
    traveBfsList <<- vector(mode = "character", length = 0)
    for(i in 1:nVar) {
      if (graphInfo[[i]]$isStart) {
        colleListBFS <<- c(colleListBFS, graphInfo[[i]]$name)
      }
    }
    while(length(colleListBFS) > 0) {
      curListBFS <- colleListBFS
      colleListBFS <<- vector(mode = "character", length = 0)
      if (length(curListBFS) > 0) {
        for (i in 1:length(curListBFS)) {
          cur_name <- curListBFS[i]
          cur_id   <- name2id[[cur_name]]
          if (!visited[cur_id] && IsAllParentVisited(cur_name, graphInfo[[cur_id]]$parent)) {
            traveBfsList    <<- c(traveBfsList, cur_name)
            visited[cur_id] <<- TRUE
          }
          if (length(graphInfo[[cur_id]]$children) > 0) {
            colleListBFS <<- c(colleListBFS, graphInfo[[cur_id]]$children)
          }
        }
      }
    }
    #PrintTravelListBFS()
})

HGraphClass$methods(
PlotGraph = function() {
  library(igraph)
  plot_str <- vector(mode = "character", length = 0)
  cur_edge <- edgeMat
  for (i in 1:nVar) {
    for (j in 1:nVar) {
      if (cur_edge[varList[i], varList[j]] == 1) {
        plot_str <- c(plot_str, varList[i], varList[j])
      }
    }
  }
  g <- graph(edges = plot_str, directed = TRUE)
  plot(g,
       layout=layout.fruchterman.reingold,  #layout.fruchterman.reingold表示弹簧式发散的布局，
       #其他还有环形布局layout.circle，分层布局layout.reingold.tilford，中心向外发散layout.reingold.tilford(graph,circular=T) ，核心布局layout_as_star，大型网络可视化layout_with_drl
       vertex.size=20,     #节点大小  
       vertex.shape=c('circle','rectangle'),    #节点不带边框none,,圆形边框circle,方块形rectangle  
       vertex.color="white",#设置颜色，其他如red,blue,cyan,yellow等
       vertex.label=NULL, #NULL表示不设置，为默认状态，即默认显示数据中点的名称，可以是中文。如果是NA则表示不显示任何点信息	 
       vertex.label.cex=0.8,    #节点字体大小  
       vertex.label.color='black',  #节点字体颜色,red  
       vertex.label.dist=0.4,   #标签和节点位置错开
       edge.arrow.size=0.6,#连线的箭头的大小,若为0即为无向图，当然有些数据格式不支持有向图  
       edge.width = 0.5, #连接线宽度
       edge.label=NA, #不显示连接线标签，默认为频次
       edge.color="black")  #连线颜色 
})

HGraphClass$methods(
Runner = function(t_edgeinfo, t_all_var) {
  init(t_edgeinfo, t_all_var)
  CreateGraph()
  TravelGraph()
  TravelGraphBFS()
  #PlotGraph()
})