#sget folder rain


add_points<-function(df){
  for(ii in 1:nrow(df)){
    points(df$lng,df$lat,pch=16)
  }
}

add_labels<-function(df){
  lab = c('a','b','c','d','e')
  off1=c(-.1,.1,-.1,-.1,.1)
  off2=c(0,0,0,0,0)
  for(ii in 1:nrow(df)){
    text(x=df$lng[ii]+off1[ii],
         y=df$lat[ii]+off2[ii],
         labels=lab[ii],
         cex=1)
  }
}

get_ifis_points<-function(lids){
  lids =   c(367813,367697,406874,367567,522980)
  
  url_objects ='objects.txt'
  objectlist = read.table(url_objects,sep = '|')
  colnames(objectlist)=c('lng','lat','link_id','foreign_id','upstream_area','description')
  wh = objectlist$link_id %in% lids
  objectlist = objectlist[wh,]
  objectlist$lng = as.numeric(objectlist$lng)
  objectlist$lat = as.numeric(objectlist$lat)
  objectlist$upstream_area=as.numeric(objectlist$upstream_area)
  objectlist = objectlist[order(objectlist$upstream_area),]
  return(objectlist)
  
}






read_network<-function(){
  library(rgdal)
  f = 'supershape_horder3.rds'
  network=readRDS(f)
  return(network)
}

filter_network<-function(network){
  link_id = 367813
  lft =as.numeric( network@data[network$link_id==link_id,'left'])
  rgt = as.numeric( network@data[network$link_id==link_id,'right'])
  subset = network
  wh = which(network$h_order>=4) 
  subset = subset[wh,]
  wh = which(subset$left>=lft) 
  subset = subset[wh,]
  wh = which(subset$left<rgt)
  subset = subset[wh,]
  
  
  #plot(subset)
  return(subset)
}

merge_network_input<-function(subset,input){
  lid_sub = as.integer(subset@data$link_id)
  vals = input[lid_sub]
  subset@data$acc = vals
  return(subset)
}

get_subset_lids <-function(subset){
  return(as.integer(subset@data$link_id))
}

set_color<-function(subset){
  library(RColorBrewer)
  pal = rev(brewer.pal(7,'RdYlGn'))
  #th = c(0,5,10,25,50,100,250,500)
  th = seq(0,1.75,0.25)
  subset@data$color = NA
  #display.brewer.pal(7,'RdYlGn')
  for(ii in 1:length(th)-1){
    wh= which(subset@data$acc>=th[ii] & subset@data$acc <th[ii+1])
    n = length(wh)
    if(n>0)
      subset@data$color[wh] =rep(pal[ii],n)
  }
  return(subset)
  
}

dep_get_color_from_vals<-function(input){
  library(RColorBrewer)
  out = rep(NA,length(input))
  th = c(0,25,50,100,250,500,1000)
  #th = seq(0,400,50)
  pal = rev(brewer.pal(length(th),'RdYlGn'))
  #display.brewer.pal(7,'RdYlGn')
  for(ii in 1:length(th)-1){
    wh= which(input>=th[ii] & input <th[ii+1])
    n = length(wh)
    if(n>0)
      out[wh] =rep(pal[ii],n)
  }
  return(out)
  
}

get_color_from_vals<-function(input,th){
  library(RColorBrewer)
  out = rep(NA,length(input))
  
  #th = seq(0,400,50)
  pal = rev(brewer.pal(length(th),'RdYlGn'))
  #display.brewer.pal(7,'RdYlGn')
  for(ii in 1:length(th)-1){
    wh= which(input>=th[ii] & input <th[ii+1])
    n = length(wh)
    if(n>0)
      out[wh] =rep(pal[ii],n)
  }
  return(out)
  
}

display_network<-function(subset,mycolors){
  par(mar=c(0,0,0,0))
  par(oma=c(0,0,0,0))
  plot(subset,
       xlim=c(-94,-91.5),
       ylim=c(42,44),
       xlab='',
       ylab='',
       col=mycolors,
       lwd=3,
       #lwd=as.numeric(subset@data$h_order)/5,
       bg='grey'
  )
  #  f='data_bseo/cedarrapids.kml'
  #  f = readOGR(f)
  #  saveRDS(f,file='data_bseo/cedarrapids.rds')
  f=readRDS('cedarrapids.rds')
  plot(f,add=T)
  
}

plot_legend<-function(mypalette,th){
  n = length(mypalette)
  image(0:n,1,as.matrix(1:n),col=mypalette,
        xlab="mm",
        ylab="",
        xaxt="n",
        yaxt="n",
        bty="n")
  axis(1,at=seq(0,n),labels=th,cex.axis=.7)
}

test_palette<-function(){
  th = seq(150,550,50)
  library(RColorBrewer)
  pal = rev(brewer.pal((length(th)-1),'RdYlGn'))
  plot_legend(pal,th)
}

test2<-function(){
  folder = 'bin/early/2016'
  tini = as.numeric(ISOdate(2016,9,1,0))
  tend = as.numeric(ISOdate(2016,10,1,0))
  th_rain = c(0,50,100,200,300,400,500,1000)
  th_rain = seq(150,550,50)
  input1 = get_binary_rain_imerg(folder,tini,tend)
  acc1 = accumulate_flow_direction(input1)
  
  network = read_network()
  subset = filter_network(network)
  lids = get_subset_lids(subset)
  input1 = input1[lids]
  acc1 = acc1[lids]
  colinput1 = get_color_from_vals(input1,th_rain)
  colacc1 = get_color_from_vals(acc1,th_rain)
  
  #hist(input1,breaks=seq(0,400,50),min=0,max=400)
  #subset = merge_network_input(subset,input)
  #subset = set_color(subset)
  display_network(subset,colinput1)
  display_network(subset,colacc1)
  
  #plot(subset,col=subset@data$color)
}


#test2()