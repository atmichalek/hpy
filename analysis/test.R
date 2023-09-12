library(ncdf4)
library(zoo)
source('./analysis/map1.R')
library(animation)
library(dataRetrieval)
test1 = function() {
  f = 'C:/Users/Alexander/Documents/hpy/examples/cedarrapids1/out/test.nc'
  nc = nc_open(f)
  varlid = ncvar_get(nc, 'link_id')
  lids = c(367813)
  url_objects = 'objects.txt'
  objectlist = read.table(url_objects, sep = '|', skip = 1)
  names(objectlist) = c('lng',
                        'lat',
                        'link_id',
                        'foreign_id',
                        'upstream_area',
                        'description')
  dt = data.table(objectlist)
  #dt = dt[foreign_id %like% "0546" & upstream_area >1000]
  #dt = dt[foreign_id %like% "0546" & upstream_area < 1000]
  dt = dt[foreign_id %like% "0546"]
  dt = dt[c(4, 19, 13),]
  maxq = 3
  nt = 700
  col1 = c('red', 'darkgreen', 'darkblue')
  par(mar = c(4, 5, 1, 1))
  plot(
    NA,
    xlim = c(0, nt),
    xlab = 'Hours',
    ylab = 'Discharge, m3s-1',
    ylim = c(0, maxq),
    cex.axis = 2,
    cex.lab = 2,
    cex = 2
  )
  legend(
    'topright',
    lwd = 2,
    col = col1,
    legend = c('6500 sq.mi', '5100 sq.mi', '1700 sq.mi'),
    cex = 2
  )
  for (ii in 1:nrow(dt)) {
    wh = which(varlid == dt$link_id[ii])
    if (length(wh) > 0) {
      print(ii)
      q = ncvar_get(nc,
                    'discharge',
                    start = c(wh, 1),
                    count = c(1, nt))
      lines(q, lwd = 2, col = col1[ii])
    }
    
    
  }
  nc_close(nc)
}

test2 = function() {
  f = 'C:/Users/Alexander/Documents/hpy/examples/cedarrapids1/out/test.nc'
  nc = nc_open(f)
  varlid = ncvar_get(nc, 'link_id')
  
  wh = which(varlid == 367813)
  nt = nc$dim$timedim$len
  #nt = 700
  v = ncvar_get(nc,
                'volume',
                start = c(wh, 1),
                count = c(1, nt))
  plot(
    NA,
    xlim = c(0, nt),
    xlab = 'Hours',
    ylab = 'Volume m3',
    ylim = c(0, max(v))
  )
  lines(v)
  q = ncvar_get(nc,
                'discharge',
                start = c(wh, 1),
                count = c(1, nt))
  plot(
    NA,
    xlim = c(0, nt),
    xlab = 'Hours',
    ylab = 'Discharge m3s-1',
    ylim = c(0, max(q))
  )
  lines(q)
  nc_close(nc)
}

test3 = function() {
  network = read_network()
  subset = filter_network(network)
  lids = get_subset_lids(subset)
  f = 'C:/Users/Alexander/Documents/hpy/examples/cedarrapids1/out/test.nc'
  nc = nc_open(f)
  varlid = ncvar_get(nc, 'link_id')
  s1 = order(varlid, decreasing = F)
  nlinks = nc$dim$linkdim$len
  th = seq(0, 1.75, 0.25)
  basefolder = 'plotnetwork/'
  fileout = 'animation.gif'
  animation::saveGIF(
    expr = for (ii in seq(1, 700, 10)) {
      out = array(0, dim = c(620172))
      input1 = ncvar_get(nc,
                         'discharge',
                         start = c(1, ii),
                         count = c(nlinks, 1))
      out[varlid] = input1
      input1 = out[lids]
      colinput1 = get_color_from_vals(input1, th)
      #png(filename = fileout,width = 90,height = 90,units = 'mm',res=200)
      display_network(subset, colinput1)
      #add_points(points)
      #dev.off()
    }
    
    ,
    movie.name = fileout,
    imgdir = basefolder,
    interval = 0.5,
    ani.height = 800,
    ani.width = 800,
    ani.res = 300
  )
  
}

test4 = function() {
  library(data.table)
  library(ncdf4)
  f = 'C:/Users/Alexander/Documents/hpy/examples/cedarrapids1/out/2008.nc'
  nc = nc_open(f)
  varlid = ncvar_get(nc, 'link_id')
  lids = c(367813)
  url_objects = 'objects.txt'
  objectlist = read.table(url_objects, sep = '|', skip = 1)
  names(objectlist) = c('lng',
                        'lat',
                        'link_id',
                        'foreign_id',
                        'upstream_area',
                        'description')
  dt = data.table(objectlist)
  dt = dt[foreign_id %like% "0546"]
  
  dt = dt[4]
  
  nt = nc$dim$timedim$len
  
  pdf(file='2008.pdf')
  par(mar = c(5, 6, 1, 1))
  
  wh = which(varlid == dt$link_id)
  
  
  n = names(nc$var)[14:length(nc$var)]
  vart = ncvar_get(nc,
                        'time',
                        start = c( 1),
                        count = c( nt))
  vart = as.POSIXct(vart,origin='1970-01-01',tz='UTC')
  
  for (ii in 1:length(n)) {
    #q = ncvar_get(nc,'discharge',start=c(wh,1),count=c(1,nt))
    var1 = n[ii]
    q1 = ncvar_get(nc,
                   var1,
                   start = c(wh, 1),
                   count = c(1, nt))
    maxq1 = max(q1)
    print(maxq1)
    units = ncatt_get(nc,var1,attname = 'units')$value
    if (is.na(maxq1) ) maxq1 = 0
      
    plot(
      zoo(q1,vart),
      xlab='',
      ylab = sprintf('%s, in %s',var1,units),
      ylim = c(0, maxq1),
      cex.axis = 1,
      cex.lab = 1,
      cex = 1,
      type='n'
    )
    lines(zoo(q1,vart), lwd = 2)
  }
  dev.off()
  
  
  nc_close(nc)
}

test5 = function(){
  library(data.table)
  library(ncdf4)
  f = 'C:/Users/Alexander/Documents/hpy/examples/cedarrapids1/out/2008.nc'
  nc = nc_open(f)
  varlid = ncvar_get(nc, 'link_id')  
  nl = length(varlid)
  n = names(nc$var)[1:14]
  ii=3 #surface
  var1 = n[ii]
  v = ncvar_get(nc,var1)
  nt = nc$dim$timedim$len
  for(tt in 1:nt){
    v = ncvar_get(nc,
                  var1,
                  start = c(1, tt),
                  count = c(nl, 1))
    print(sprintf('%s %s',tt,range(v)))
  }
}

get_usgs_discharge =function(usgs_code,tini,tend){
  library(dataRetrieval)
  parameterCd <- '00060'
  startDate=as.Date.POSIXct(tini)
  endDate=as.Date.POSIXct(tend)
  print(usgs_code)
  data1 <- readNWISuv(usgs_code,parameterCd,startDate,endDate)
  df = data.frame(unix_time=as.numeric(data1$dateTime),discharge_m3s=data1$X_00060_00000/35.31)
  return(df)
}

test6 = function(){
  library(data.table)
  library(ncdf4)
  f = 'C:/Users/Alexander/Documents/hpy/examples/cedarrapids1/out/2008.nc'
  nc = nc_open(f)
  varlid = ncvar_get(nc, 'link_id')
  lids = c(367813)
  url_objects = 'objects.txt'
  objectlist = read.table(url_objects, sep = '|', skip = 1)
  names(objectlist) = c('lng',
                        'lat',
                        'link_id',
                        'foreign_id',
                        'upstream_area',
                        'description')
  dt = data.table(objectlist)
  dt = dt[foreign_id %like% "0546"]
  
  #dt =dt[3] #mason city
  dt = dt[4] #cedarrapids
  
  
  nt = nc$dim$timedim$len
  vart = ncvar_get(nc,
                   'time',
                   start = c( 1),
                   count = c( nt))
  vart = as.POSIXct(vart,origin='1970-01-01',tz='UTC')

  
  usgs = '05464500'
  start = ISOdate(2008,4,1,0)
  end = ISOdate(2008,10,1)
  obs = get_usgs_discharge(usgs,start,end)
  obs$r = obs$discharge_m3s / (16400*1e6) * 1000 * 60*60
  
  pdf(file='2008_runoff_sum.pdf')
  par(mar = c(5, 6, 1, 1))
  wh = which(varlid == dt$link_id)
 names(nc$var)
  q1 = ncvar_get(nc,
                 'states/basin_surface',
                 start = c(wh, 1),
                 count = c(1, nt))
  q2 = ncvar_get(nc,
                 'states/basin_subsurface',
                 start = c(wh, 1),
                 count = c(1, nt))
  
  q3 = ncvar_get(nc,
                 'states/basin_groundwater',
                 start = c(wh, 1),
                 count = c(1, nt))
  q4 = ncvar_get(nc,
                 'states/discharge',
                 start = c(wh, 1),
                 count = c(1, nt))
  q4= q4 / (16400*1e6) * 1000 * 60*60

  plot(
    zoo(NA,vart),
    xlab='',
    ylab = sprintf('%s, in %s','runoff','mm'),
    ylim = range(q1+q2+q3),
    cex.axis = 1,
    cex.lab = 1,
    cex = 1,
    type='n'
  )
  lines(zoo(q1,vart), lwd = 1,col='orange')
  lines(zoo(q2,vart), lwd = 2,col='blue')
  lines(zoo(q3,vart), lwd = 2,col='magenta')
  lines(zoo(q4,vart), lwd = 2,col='darkgreen')
  lines(zoo(obs$r,as.POSIXct(obs$unix_time,origin='1970-01-01')), lwd = 2)
  dev.off()
  
}

test4()