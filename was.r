readStorageLog=function(metapath,datapath)
{
	#http://www.biosino.org/R/R-doc/R-data_cn/Variations-on-read_002etable.html
	mt=read.csv(metapath,head=FALSE,colClasses=list('character','character'))

	fl=dir(datapath)
	fl=paste(datapath,"/",fl,sep="");

	filecount=length(fl)
	if(filecount==0)return

	filei=1
	az=data.frame();
	while(filei<=filecount)
	{
		tmpaz=read.table(fl[filei],head=FALSE,sep=";",comment.char="",col.names=mt[,1],colClasses=mt[,2])
		#http://stackoverflow.com/questions/12125886/parsing-iso8601-in-r
		tmpaz$requeststarttime=strptime(tmpaz$requeststarttime,"%FT%T")
		if(filei==1)
		{
			az=tmpaz
		}else
		{
			az=merge(az,tmpaz,all=TRUE)
		}
		filei=filei+1
	}
	az
}

AnalyzeIP=function(az,rnum){
	az$clientip=as.factor(sub(':.*','',az[,'requestoripaddress']))

	r0=summary(az$clientip)
	clientips=attr(r0,'names')
	distorder=order(r0,decreasing=TRUE)
	r0=r0[distorder]

	if(length(r0)<rnum)
		rnum=length(r0)
	r0=r0[1:rnum]
	
	r1=tapply(az$endtoendlatencyinms,az$clientip,mean)
	r1=r1[order(r1,decreasing=TRUE)]
	r1=r1[1:rnum]

	r2=tapply(az$serverlatencyinms,az$clientip,mean)
	r2=r2[order(r2,decreasing=TRUE)]
	r2=r2[1:rnum]
	
	r3=tapply(az$operationtype,az$clientip,summary)
	
	m3rownames=clientips
	m3colnames=attr(r3[[1]],'names')
	m3=matrix(NA,length(m3rownames),length(m3colnames))
	rownames(m3)=m3rownames
	colnames(m3)=m3colnames
	i=1
	while(i<=length(m3rownames))
	{
		m3[i,]=r3[[i]]
		i=i+1
	}
	m3=m3[distorder,]
	m3=m3[1:rnum,]

	list(dist=r0,e2elatency=r1,serlatency=r2,optype=m3)
}

AnalyzeObjectKey=function(az,rnum){
	#az$requestedobjectkey=as.factor(az$requestedobjectkey)
	r0=summary(az$requestedobjectkey)
	r0=r0[order(r0,decreasing=TRUE)]
	
	if(length(r0)<rnum)
		rnum=length(r0)
	r0=r0[1:rnum]

	r1=tapply(az$endtoendlatencyinms,az$requestedobjectkey,mean)
	r1=r1[order(r1,decreasing=TRUE)]
	r1=r1[1:rnum]	

	r2=tapply(az$serverlatencyinms,az$requestedobjectkey,mean)
	r2=r2[order(r2,decreasing=TRUE)]
	r2=r2[1:rnum]	

	list(dist=r0,serlatency=r1,e2elatency=r2)
}
