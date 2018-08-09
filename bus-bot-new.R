
busUpdate<-function(oldgreen,key){

     gtfs<-tryCatch(
       GET('https://api.at.govt.nz/v2/public/realtime/tripupdates',
                  accept_json(),
                  add_headers('Ocp-Apim-Subscription-Key' = key)),
        error=function(e) NULL)

    if (is.null(gtfs)) return(NULL)
    if (status_code(gtfs)!=200) {
        print(status_code(gtfs))
        return(NULL)
    }
    
    buses<-lapply(content(gtfs)[[2]][[2]],function(x) x$trip_update$stop_time_update)
    arr<-do.call(rbind,lapply(buses, function(x) unlist(x$arrival)))
    dep<-do.call(rbind,lapply(buses, function(x) unlist(x$departure)))

    update.times<-as.POSIXct(c(arr[,"time"],dep[,"time"]),origin="1970-01-01")
    
    arr.delay<-arr[,"delay"]
    dep.delay<-dep[,"delay"]
    bus.delay<-c(arr.delay,dep.delay)
    is.arrival<-rep(c(TRUE,FALSE),c(length(arr.delay),length(dep.delay)))
    
    gtfsl<-NULL

    ## condition out map because server is borked
    if (mean(bus.delay>300)>1.25){
        gtfsl<-tryCatch(
            fromJSON(paste0("https://api.at.govt.nz/v1/public/realtime/vehiclelocations?api_key=",key)),
            error=function(e) NULL
        )
    }
    
    if (is.null(gtfsl)){
        ##beeswarm
        
        delaycat<-cut(bus.delay,c(-Inf,-180,-60,0,300,600,Inf))
        
        delaycols<-data.frame(
            delay=c("(-Inf,-180]", "(-180,-60]","(-60,0]", "(0,300]", "(300,600]", "(600, Inf]"),
            col=c("red","orange","#00EEB0","#00EEB0","orange","red"),
            pch=c(1,1,1,19,19,19), stringsAsFactors=FALSE
	)
        
        i<-match(delaycat,delaycols$delay)
        
        i[is.arrival & bus.delay<0]<-3  ## early arrival is ok
        
        newgreen<-mean(i %in% c(3,4))
        
        bus.delay<-pmin(50*60,pmax(-30*60,bus.delay))
        
        png("bus-summary.png",height=200,width=500)
        par(mar=c(4.1,1,1,1))
        
        beeswarm(bus.delay/60~is.arrival,pwcol=delaycols$col[i],pwpch=delaycols$pch[i],cex=0.5,horiz=TRUE,xlab="Minutes late",corral="gutter",method="swarm",xlim=c(-30,50))
        
        usr<-par("usr")
        text(usr[1]+1,2.3,"Arrivals",adj=0)
        text(usr[1]+1,0.6,"Departures",adj=0)
        dev.off()
    } else {
        ##map
        cat("Map\n")
        locs<-flatten((gtfsl[[2]]$entity))
        buses<-merge(locs,buses,by.y="trip_update.vehicle.id",by.x="vehicle.vehicle.id")

        xr<-range(buses$vehicle.position.longitude,na.rm=TRUE)
        yr<-range(buses$vehicle.position.latitude,na.rm=TRUE)
        arr.delay<-buses$trip_update.stop_time_update.arrival.delay
        dep.delay<-buses$trip_update.stop_time_update.departure.delay
        bus.delay<-dep.delay
        bus.delay[is.na(bus.delay)]<-arr.delay[is.na(bus.delay)]
        is.arrival<-is.na(dep.delay)
        delaycat<-cut(bus.delay,c(-Inf,-180,-60,0,300,600,Inf))
        
        delaycols<-data.frame(
            delay=c("(-Inf,-180]", "(-180,-60]","(-60,0]", "(0,300]", "(300,600]", "(600, Inf]"),
            col=c("grey60","grey60","grey60","grey60","#FF8000","#FF0000"),
            pch=c(18,18,18,20,20,20), stringsAsFactors=FALSE
	)
        
        i<-match(delaycat,delaycols$delay)
        
        i[is.na(dep.delay) & bus.delay<0]<-3  ## early arrival is ok
        
        newgreen<-mean(i %in% c(3,4))
        j<-order(bus.delay)

        png("bus-summary.png",height=300,width=300)
        par(mar=c(1,1,1,1))
        plot(auck2,border="grey90",xlim=xr,ylim=yr,col="#ffffff",bg="#00000020",lwd=0.5)
        points(vehicle.position.latitude~vehicle.position.longitude,data=locs[j,],pch=delaycols$pch[i][j],col=delaycols$col[i][j],cex=.8)
        text(174.9811181,-36.6051305,strftime(Sys.time(),"%b-%d %H:%M"))
        dev.off()
    }    
    
    
    l0<-strftime(median(update.times),"%b %d %H:%M")
    l1<-paste0(round(newgreen*100),"% on time")
    
    if (is.na(oldgreen)) l1<-paste(l1,"\n")
    else if (newgreen>=oldgreen+.05) l1<-paste0(l1, ": getting better\n")
    else if (newgreen<=oldgreen-.05) l1<-paste0(l1, ": getting worse\n")
    else l1<-paste(l1,"\n")
    
    return(list(l1,l0,newgreen,sum(!is.na(bus.delay))))
}

## startup
library(twitteR)
library(jsonlite)
library(beeswarm)
library(maptools)
library(httr)

load("auckland-thinned.rda")


load("~/.ssh/twitter-bot-secrets.rda")
with(secrets,setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret))

oldgreen<-NA

## run
mihi<-""
#mihi<-"Kia ora."

repeat({
    cat("a",system.time({
        r<-busUpdate(oldgreen,key=secrets$newapikey)
    }))
    if (is.null(r)) {
        cat(Sys.time(),"No data: waiting 5 minutes\n")
	try(tweet("I see no buses. I'll just go have a nap until the internet clears up.\n Hei kōnā mai."))
	Sys.sleep(60*5)
	repeat({
            r<-busUpdate(oldgreen,key=secrets$newapikey)
            if(!is.null(r)) break;
            cat(Sys.time(),"No data: waiting 30 minutes\n")
            Sys.sleep(60*30)
            #mihi<-"Kia ora."
        })
    }
    oldgreen<-r[[3]]
    
    
    repeat({
        worked<-tryCatch(
            tweet(paste(mihi,"At", r[[2]],"I can see",r[[4]],"buses with",r[[1]]), mediaPath="bus-summary.png"),
            error=function(e) NULL
        )
        
        if (!is.null(worked)) break
        cat(Sys.time(),"tweet failed, waiting 15\n")
        Sys.sleep(15*60)
    })
    
    mihi<-""
    
    cat(Sys.time(),"Tweeted: waiting 15 minutes\n")
    Sys.sleep(15*60)
    
    if( as.POSIXlt(Sys.time())$hour>21) {
	night<-TRUE
	tweet("It's late. The buses will soon be asleep. Pō marie.")
	cat(Sys.time(),"Night: waiting 8 hours\n")
        
	Sys.sleep(60*8*60)
	mihi<-sample(c("Ata marie.", "Mōrena."),1)
    }
    
})	

